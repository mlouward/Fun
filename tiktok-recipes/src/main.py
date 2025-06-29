import asyncio
import json
import os
import sys
from pathlib import Path
from typing import Any

from fastapi import Body, FastAPI, HTTPException, Request
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from pydantic import BaseModel, HttpUrl

# Import our backend modules
from .audio_extractor import download_audio_from_tiktok
from .nlp_processor import extract_recipe_info
from .paprika_exporter import format_for_paprika, get_data_filename
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
    1. Downloads the audio from the TikTok URL.
    2. Transcribes the audio to text using Whisper.
    3. Fetches the video description using TikTokApi.
    4. Combines transcript and description as context.
    5. Extracts structured recipe data from the combined context using an NLP model.
    6. Formats the data into a Paprika-compatible JSON file and returns the content.
    """
    try:
        # Check if the data already exists
        output_path = data_dir / get_data_filename(str(request.url))
        if output_path.exists():
            with open(output_path, "r") as f:
                return format_answer(json.load(f))

        # 1. Download Audio
        print(f"Processing URL: {request.url}")
        audio_path = download_audio_from_tiktok(str(request.url))
        print(f"Audio downloaded to: {audio_path}")
        if not audio_path:
            raise HTTPException(
                status_code=500,
                detail="Audio download failed. The video may be invalid or the URL may be incorrect.",
            )

        # 2. Transcribe Audio
        transcript = transcribe_audio(audio_path)
        if not transcript:
            raise HTTPException(
                status_code=500, detail="Transcription failed. The audio may be silent or invalid."
            )
        print("Transcription successful.")

        # 3. Fetch TikTok video description
        description = await get_tiktok_video_description(str(request.url))
        if description:
            print(f"Fetched TikTok description: {description}")
        else:
            print("No TikTok description found or failed to fetch.")

        # 4. Combine transcript and description as context
        if description:
            context = f"Description: {description}\nTranscript: {transcript}"
        else:
            context = transcript

        # 5. Extract Recipe Info
        recipe_info = extract_recipe_info(context)
        if not recipe_info:
            raise HTTPException(
                status_code=500,
                detail="Failed to extract recipe information from the transcript and description.",
            )
        print("Recipe info extracted.")

        # 6. Format for Paprika and get the file path
        recipe_file_path = format_for_paprika(recipe_info, source_url=str(request.url))
        print(f"Recipe saved to: {recipe_file_path}")

        # Clean up the downloaded audio file
        os.remove(audio_path)
        print(f"Cleaned up audio file: {audio_path}")

        # Return the content of the generated JSON file
        with open(recipe_file_path, "r") as f:
            final_recipe = json.load(f)

        return format_answer(final_recipe)

    except Exception as e:
        # Log the full error for debugging
        print(f"An unexpected error occurred: {e}")
        # Return a generic error to the client
        raise HTTPException(status_code=500, detail=f"An internal server error occurred: {e}")


def format_answer(recipe_json: dict[str, Any]) -> dict[str, Any]:
    return {"message": "Recipe processed successfully!", "recipe": recipe_json}
