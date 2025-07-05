import asyncio
import os
from typing import Literal, Optional

from sqlalchemy.ext.asyncio import AsyncSession

from audio_extractor import download_audio_and_covers_from_tiktok
from models import AsyncSessionLocal, Recipe
from nlp_processor import extract_recipe_info
from paprika_exporter import PaprikaRecipe
from tiktok_description import get_tiktok_video_description
from transcriber import transcribe_audio

from .celery_worker import celery_app


@celery_app.task
async def process_tiktok_recipe(user_id, url, tiktok_username, tiktok_video_id) -> Literal[True]:
    async def do_work():
        # Download audio and covers
        audio_path, cover_bytes_list = download_audio_and_covers_from_tiktok(url)
        if not audio_path or not cover_bytes_list:
            raise ValueError("Audio download failed.")
        transcript = transcribe_audio(audio_path)
        description = await get_tiktok_video_description(url)
        context = (
            f"Description: {description}\nTranscript: {transcript}" if description else transcript
        )
        recipe_info = extract_recipe_info(context)
        paprika_recipe = PaprikaRecipe(recipe_info, source_url=url, photo_data=cover_bytes_list[0])
        os.remove(audio_path)
        save_cover_images_to_disk(user_id, tiktok_video_id, cover_bytes_list, base_dir="data")
        # DB access
        async with AsyncSessionLocal() as db:
            await create_recipe_from_dict(
                db,
                user_id,
                paprika_recipe.data,
                tiktok_username,
                tiktok_video_id,
            )
            await db.commit()

    asyncio.run(do_work())
    return True


# Helper: save cover images to disk
def save_cover_images_to_disk(
    user_id: int, tiktok_video_id: str, cover_bytes_list: list, base_dir: str = "data"
) -> list:
    """
    Save each image in cover_bytes_list to data/{user_id}/{tiktok_video_id}/cover_{idx}.jpg
    Returns the list of file paths.
    """
    os.makedirs(f"{base_dir}/{user_id}/{tiktok_video_id}", exist_ok=True)
    file_paths = []
    for idx, img_bytes in enumerate(cover_bytes_list):
        file_path = f"{base_dir}/{user_id}/{tiktok_video_id}/cover_{idx + 1}.jpg"
        with open(file_path, "wb") as f:
            f.write(img_bytes)
        file_paths.append(file_path)
    return file_paths


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
        user_id=user_id,
        title=data.get("name") or data.get("title") or "Untitled Recipe",
        servings=safe_int(data.get("servings")),
        prep_time=safe_int(data.get("prep_time")),
        cook_time=safe_int(data.get("cook_time")),
        ingredients=data.get("ingredients", ""),
        instructions=data.get("directions") or data.get("instructions", ""),
        cover_image_idx=safe_int(data.get("cover_image_idx")),
        tiktok_username=tiktok_username,
        tiktok_video_id=tiktok_video_id,
    )
    db.add(recipe)
    await db.commit()
    await db.refresh(recipe)
    return recipe
