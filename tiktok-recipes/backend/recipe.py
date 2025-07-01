import base64
import gzip
import io
import json
import os
import re
from pathlib import Path
from typing import Any, List, Optional

from fastapi import APIRouter, Body, Depends, HTTPException
from fastapi.responses import StreamingResponse
from fastapi.security import OAuth2PasswordBearer
from jose import JWTError, jwt
from pydantic import BaseModel
from sqlalchemy import and_, select
from sqlalchemy.ext.asyncio import AsyncSession

from .audio_extractor import download_audio_and_cover_from_tiktok
from .auth import ALGORITHM, SECRET_KEY
from .models import AsyncSessionLocal, Recipe, User
from .nlp_processor import extract_recipe_info
from .paprika_exporter import PaprikaRecipe
from .tiktok_description import get_tiktok_video_description
from .transcriber import transcribe_audio

router = APIRouter(tags=["recipes"])

oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/auth/login")

DATA_DIR = Path(os.environ.get("DATA_DIR", Path(__file__).parent.parent / "data"))


class RecipeCreate(BaseModel):
    title: str
    servings: int
    prep_time: int
    cook_time: int
    ingredients: str
    instructions: str
    cover_image_idx: int


class ProcessRequest(BaseModel):
    url: str


async def get_db():
    async with AsyncSessionLocal() as session:
        yield session


async def get_current_user(token: str = Depends(oauth2_scheme), db: AsyncSession = Depends(get_db)):
    credentials_exception = HTTPException(
        status_code=401,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )
    try:
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        sub = payload.get("sub")
        if sub is None:
            raise credentials_exception
        username: str = sub
    except JWTError:
        raise credentials_exception
    result = await db.execute(select(User).where(User.username == username))
    user = result.scalar()
    if user is None:
        raise credentials_exception
    return user


# Helper: get a recipe for a user by tiktok_username/video_id or title
async def get_user_recipe(
    db: AsyncSession,
    user_id: int,
    tiktok_username: Optional[str] = None,
    tiktok_video_id: Optional[str] = None,
    title: Optional[str] = None,
) -> Optional[Recipe]:
    if tiktok_username and tiktok_video_id:
        result = await db.execute(
            select(Recipe).where(
                and_(
                    Recipe.user_id == user_id,
                    Recipe.tiktok_username == tiktok_username,
                    Recipe.tiktok_video_id == tiktok_video_id,
                )
            )
        )
        recipe = result.scalar()
        if recipe:
            return recipe
    if title:
        result = await db.execute(
            select(Recipe).where(and_(Recipe.user_id == user_id, Recipe.title == title))
        )
        recipe = result.scalar()
        if recipe:
            return recipe
    return None


# Helper: create a Recipe from a dict, parsing ints safely
async def create_recipe_from_dict(
    db: AsyncSession,
    user_id: int,
    data: dict,
    tiktok_username: Optional[str] = None,
    tiktok_video_id: Optional[str] = None,
) -> Recipe:
    def safe_int(val, default=0):
        try:
            return int(val)
        except Exception:
            return default

    recipe = Recipe(
        title=data.get("name") or data.get("title") or "Untitled Recipe",
        servings=safe_int(data.get("servings")),
        prep_time=safe_int(data.get("prep_time")),
        cook_time=safe_int(data.get("cook_time")),
        ingredients=data.get("ingredients", ""),
        instructions=data.get("directions") or data.get("instructions", ""),
        cover_image_idx=safe_int(data.get("cover_image_idx")),
        user_id=user_id,
        tiktok_username=tiktok_username,
        tiktok_video_id=tiktok_video_id,
    )
    db.add(recipe)
    await db.commit()
    await db.refresh(recipe)
    return recipe


@router.post("/", response_model=RecipeCreate)
async def create_recipe(
    recipe: RecipeCreate,
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user),
):
    db_recipe = Recipe(**recipe.model_dump(), user_id=current_user.id)
    db.add(db_recipe)
    await db.commit()
    await db.refresh(db_recipe)
    return db_recipe


@router.get("/", response_model=List[RecipeCreate])
async def list_recipes(
    db: AsyncSession = Depends(get_db), current_user: User = Depends(get_current_user)
):
    result = await db.execute(select(Recipe).where(Recipe.user_id == current_user.id))
    return result.scalars().all()


@router.post("/process_url", tags=["Recipe Processing"])
async def process_tiktok_url(
    request: ProcessRequest = Body(...),
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> dict[Any, Any]:
    try:
        # Parse TikTok username and video ID from URL
        match = re.search(r"tiktok\.com/@([\w.]+)/video/(\d+)", request.url)
        tiktok_username = match.group(1) if match else None
        tiktok_video_id = match.group(2) if match else None
        if not tiktok_username or not tiktok_video_id:
            raise HTTPException(status_code=400, detail="Invalid TikTok URL format.")
        # Check if this user already has this video saved
        db_recipe = await get_user_recipe(db, current_user.id, tiktok_username, tiktok_video_id)
        if db_recipe:
            result = format_answer(
                {
                    "title": db_recipe.title,
                    "servings": db_recipe.servings,
                    "prep_time": db_recipe.prep_time,
                    "cook_time": db_recipe.cook_time,
                    "ingredients": db_recipe.ingredients,
                    "instructions": db_recipe.instructions,
                    "cover_image_idx": db_recipe.cover_image_idx,
                    "tiktok_username": db_recipe.tiktok_username,
                    "tiktok_video_id": db_recipe.tiktok_video_id,
                }
            )
            return result
        # Download and transcribe as before
        audio_path, cover_bytes_list = download_audio_and_cover_from_tiktok(request.url)
        if not audio_path or not cover_bytes_list:
            raise HTTPException(status_code=500, detail="Audio download failed.")
        transcript = transcribe_audio(audio_path)
        if not transcript:
            raise HTTPException(status_code=500, detail="Transcription failed.")
        description = await get_tiktok_video_description(request.url)
        context = (
            f"Description: {description}\nTranscript: {transcript}" if description else transcript
        )
        recipe_info = extract_recipe_info(context)
        if not recipe_info:
            raise HTTPException(status_code=500, detail="Failed to extract recipe information.")
        paprika_recipe = PaprikaRecipe(
            recipe_info, source_url=request.url, photo_data=cover_bytes_list[0]
        )
        os.remove(audio_path)
        db_recipe = await create_recipe_from_dict(
            db, current_user.id, paprika_recipe.data, tiktok_username, tiktok_video_id
        )
        result = format_answer(
            {
                "title": db_recipe.title,
                "servings": db_recipe.servings,
                "prep_time": db_recipe.prep_time,
                "cook_time": db_recipe.cook_time,
                "ingredients": db_recipe.ingredients,
                "instructions": db_recipe.instructions,
                "cover_image_idx": db_recipe.cover_image_idx,
                "tiktok_username": db_recipe.tiktok_username,
                "tiktok_video_id": db_recipe.tiktok_video_id,
            }
        )
        cover_images_b64 = [base64.b64encode(b).decode("utf-8") for b in cover_bytes_list]
        result["cover_images"] = cover_images_b64
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"An internal server error occurred: {e}")


def format_answer(recipe_json: dict) -> dict:
    return {"message": "Recipe processed successfully!", "recipe": recipe_json}


@router.post("/update_recipe", tags=["Recipe Editing"])
async def update_recipe(
    data: dict = Body(...),
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user),
):
    try:
        tiktok_username = data.get("tiktok_username")
        tiktok_video_id = data.get("tiktok_video_id")
        title = data.get("title")
        recipe = await get_user_recipe(db, current_user.id, tiktok_username, tiktok_video_id, title)
        if not recipe:
            return {"error": "Recipe not found for update."}
        for field in [
            "title",
            "servings",
            "prep_time",
            "cook_time",
            "ingredients",
            "instructions",
            "cover_image_idx",
        ]:
            if field in data:
                setattr(recipe, field, data[field])
        await db.commit()
        await db.refresh(recipe)
        return {
            "message": "Recipe updated successfully!",
            "recipe": {
                "title": recipe.title,
                "servings": int(recipe.servings) if recipe.servings is not None else 0,
                "prep_time": int(recipe.prep_time) if recipe.prep_time is not None else 0,
                "cook_time": int(recipe.cook_time) if recipe.cook_time is not None else 0,
                "ingredients": recipe.ingredients,
                "instructions": recipe.instructions,
                "cover_image_idx": int(recipe.cover_image_idx)
                if recipe.cover_image_idx is not None
                else 0,
                "tiktok_username": getattr(recipe, "tiktok_username", None),
                "tiktok_video_id": getattr(recipe, "tiktok_video_id", None),
            },
        }
    except Exception as e:
        return {"error": str(e)}


@router.post("/export_paprika")
async def export_paprika(
    data: dict = Body(...),
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> StreamingResponse:
    tiktok_username = data.get("tiktok_username")
    tiktok_video_id = data.get("tiktok_video_id")
    title = data.get("name") or data.get("title")
    recipe = await get_user_recipe(db, current_user.id, tiktok_username, tiktok_video_id, title)
    if not recipe:
        raise HTTPException(status_code=404, detail="Recipe not found for export.")
    paprika_dict = {
        "name": recipe.title,
        "servings": recipe.servings,
        "prep_time": recipe.prep_time,
        "cook_time": recipe.cook_time,
        "ingredients": recipe.ingredients,
        "directions": recipe.instructions,
    }
    paprika_json = io.BytesIO()
    with gzip.GzipFile(fileobj=paprika_json, mode="wb") as gz:
        gz.write(bytes(json.dumps(paprika_dict, indent=2), "utf-8"))
    paprika_json.seek(0)
    filename = f"{recipe.title or 'recipe'}.paprikarecipe"
    return StreamingResponse(
        paprika_json,
        media_type="application/x-paprika-recipe",
        headers={"Content-Disposition": f"attachment; filename={filename}"},
    )
