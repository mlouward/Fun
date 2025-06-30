import asyncio
import gzip
import io
import json
import os
import re
import sys
from pathlib import Path
from typing import Any

from fastapi import Body, FastAPI, HTTPException, Request
from fastapi.responses import HTMLResponse, StreamingResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from pydantic import BaseModel, HttpUrl
from rich.console import Console
from rich.panel import Panel
from unidecode import unidecode

# Import our backend modules
from .audio_extractor import download_audio_and_cover_from_tiktok
from .nlp_processor import extract_recipe_info
from .paprika_api import PaprikaAPI
from .paprika_exporter import PaprikaRecipe
from .tiktok_description import get_tiktok_video_description
from .transcriber import transcribe_audio

if sys.platform == "win32":
    asyncio.set_event_loop_policy(asyncio.WindowsSelectorEventLoopPolicy())

# --- App Setup ---
app = FastAPI(
    title="TikTok Recipe Transcriber API",
    description="An API to download, transcribe, and process TikTok recipe videos into a structured JSON format.",
    version="1.0.0",
)

# --- Static Files and Templates ---
# Get the directory of the current script to build robust paths
script_dir = Path(__file__).parent
static_dir = script_dir / "static"
templates_dir = script_dir / "templates"
data_dir = script_dir.parent / "data"

app.mount("/static", StaticFiles(directory=static_dir), name="static")
templates = Jinja2Templates(directory=templates_dir)
console = Console()


class ProcessRequest(BaseModel):
    url: HttpUrl


@app.get("/", response_class=HTMLResponse, tags=["Frontend"])
def read_root(request: Request):
    """Serves the main HTML page for the user interface."""
    return templates.TemplateResponse("index.html", {"request": request})


@app.post("/process-url/", tags=["Recipe Processing"])
async def process_tiktok_url(request: ProcessRequest = Body(...)) -> dict[str, Any]:
    """
    Processes a TikTok URL to extract recipe information, now with TikTok video description as extra context.

    This endpoint performs the following steps:
    1. Downloads the audio and cover image from the TikTok URL.
    2. Transcribes the audio to text using Whisper.
    3. Fetches the video description using TikTokApi.
    4. Combines transcript and description as context.
    5. Extracts structured recipe data from the combined context using an NLP model.
    6. Formats the data into a Paprika-compatible JSON file and returns the content.
    """
    try:
        # Check if the data already exists
        output_path = data_dir / PaprikaRecipe._get_data_filename(str(request.url))
        if output_path.exists():
            console.rule("[bold blue]Paprika JSON Already Exists")
            paprika_recipe = PaprikaRecipe.from_file(output_path)
            console.print("[yellow]Returning cached recipe.[/yellow]")
            return format_answer(paprika_recipe.to_dict())

        # 1. Download Audio and Cover
        console.rule("[bold blue]Step 1: Download Audio and Cover")
        console.print(f"[cyan]Processing URL:[/cyan] {request.url}")
        audio_path, cover_bytes = download_audio_and_cover_from_tiktok(str(request.url))
        if not audio_path:
            console.print(Panel("[bold red]Audio download failed.[/bold red]", style="red"))
            raise HTTPException(
                status_code=500,
                detail="Audio download failed. The video may be invalid or the URL may be incorrect.",
            )

        # 2. Transcribe Audio
        console.rule("[bold blue]Step 2: Transcribe Audio")
        transcript = transcribe_audio(audio_path)
        if not transcript:
            console.print(Panel("[bold red]Transcription failed.[/bold red]", style="red"))
            raise HTTPException(
                status_code=500, detail="Transcription failed. The audio may be silent or invalid."
            )

        # 3. Fetch TikTok video description
        console.rule("[bold blue]Step 3: Fetch TikTok Description")
        description = await get_tiktok_video_description(str(request.url))

        # 4. Combine transcript and description as context
        console.rule("[bold blue]Step 4: Combine Context")
        if description:
            context = f"Description: {description}\nTranscript: {transcript}"
        else:
            context = transcript

        # 5. Extract Recipe Info
        console.rule("[bold blue]Step 5: NLP Extraction")
        recipe_info = extract_recipe_info(context)
        if not recipe_info:
            console.print(
                Panel("[bold red]Failed to extract recipe information.[/bold red]", style="red")
            )
            raise HTTPException(
                status_code=500,
                detail="Failed to extract recipe information from the transcript and description.",
            )
        console.print(Panel("[green]Recipe info extracted.[/green]", style="green"))

        # 6. Format for Paprika and get the file path
        console.rule("[bold blue]Step 6: Format for Paprika")
        paprika_recipe = PaprikaRecipe(
            recipe_info, source_url=str(request.url), photo_data=cover_bytes
        )
        recipe_file_path = paprika_recipe.save()
        console.print(f"[green]Recipe saved to:[/green] {recipe_file_path}")

        # Clean up the downloaded audio file
        try:
            os.remove(audio_path)
            console.print(f"[yellow]Cleaned up audio file:[/yellow] {audio_path}")
        except Exception as e:
            console.print(f"[red]Warning: Failed to remove audio file: {e}[/red]")

        # Return the content of the generated JSON file
        paprika_recipe = PaprikaRecipe.from_file(recipe_file_path)
        return format_answer(paprika_recipe.to_dict())

    except Exception as e:
        # Log the full error for debugging
        console.print(Panel(f"[bold red]An unexpected error occurred:[/bold red] {e}", style="red"))
        # Return a generic error to the client
        raise HTTPException(status_code=500, detail=f"An internal server error occurred: {e}")


def format_answer(recipe_json: dict[str, Any]) -> dict[str, Any]:
    return {"message": "Recipe processed successfully!", "recipe": recipe_json}


class PaprikaUploadRequest(BaseModel):
    email: str
    password: str
    url: HttpUrl


@app.post("/upload-to-paprika/", tags=["Paprika Integration"])
async def upload_to_paprika(request: PaprikaUploadRequest = Body(...)) -> dict[str, Any]:
    """
    Uploads a processed recipe to the user's Paprika account.
    """
    try:
        # Find the recipe file
        output_path = data_dir / PaprikaRecipe._get_data_filename(str(request.url))
        if not output_path.exists():
            raise HTTPException(
                status_code=404, detail="Recipe not found. Please process the URL first."
            )
        paprika_recipe = PaprikaRecipe.from_file(output_path)
        recipe = paprika_recipe.to_dict()
        # Login and upload
        api = PaprikaAPI(request.email, request.password)
        token = api.login()
        if not token:
            raise HTTPException(status_code=401, detail="Paprika login failed. Check credentials.")
        success = api.upload_recipe(recipe)
        if not success:
            raise HTTPException(status_code=500, detail="Failed to upload recipe to Paprika.")
        return {"message": "Recipe uploaded to Paprika successfully!"}
    except Exception as e:
        console.print(Panel(f"[bold red]Paprika upload error:[/bold red] {e}", style="red"))
        raise HTTPException(status_code=500, detail=f"Paprika upload error: {e}")


def normalize_filename(name: str) -> str:
    name_ascii = unidecode(name)
    name_ascii = name_ascii.lower()
    name_ascii = re.sub(r"[^a-z0-9]+", "-", name_ascii)
    name_ascii = name_ascii.strip("-")
    return name_ascii or "recipe"


@app.post("/export-paprika-file/", response_class=StreamingResponse, tags=["Paprika Integration"])
async def export_paprika_file(request: Request):
    """
    Accepts a recipe JSON and returns a .paprikarecipe (gzipped JSON) file.
    The filename is based on the recipe's 'name' field, normalized.
    """
    try:
        recipe_json = await request.body()
        recipe_dict = json.loads(recipe_json)
        # Get and normalize the name
        name = recipe_dict.get("name", "recipe")
        console.print(f"[cyan]Exporting recipe: {name}[/cyan]")
        filename = f"{normalize_filename(name)}.paprikarecipe"
        gzipped = gzip.compress(recipe_json)
        return StreamingResponse(
            io.BytesIO(gzipped),
            media_type="application/octet-stream",
            headers={"Content-Disposition": f"attachment; filename={filename}"},
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to export Paprika file: {e}")
