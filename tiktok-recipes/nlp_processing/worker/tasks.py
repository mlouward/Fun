import asyncio
import logging
import os
import re
from typing import Literal, Optional

from TikTokApi import TikTokApi

from sqlalchemy.ext.asyncio import AsyncSession

from audio_extractor import download_audio_and_covers_from_tiktok
from models import AsyncSessionLocal, Recipe
from nlp_processor import extract_recipe_info
from paprika_exporter import PaprikaRecipe
from tiktok_description import get_tiktok_video_description
from transcriber import transcribe_audio

from .celery_worker import celery_app

logger = logging.getLogger(__name__)


async def _process_recipe_async(user_id, url) -> Literal[True]:
    """Helper coroutine to run the actual processing logic."""
    # Extract tiktok_username and tiktok_video_id from the URL
    tiktok_username = None
    tiktok_video_id = None

    # Try to extract from standard TikTok URL first
    match = re.search(r"tiktok\\.com/@([\\w.]+)/video/(\\d+)", url)
    if match:
        tiktok_username = match.group(1)
        tiktok_video_id = match.group(2)
    else:
        # If it's a short URL (vm.tiktok.com), use TikTokApi to get the full info
        try:
            async with TikTokApi() as api:
                ms_token = os.environ.get("ms_token")
                await api.create_sessions(
                    ms_tokens=[ms_token] if ms_token else None,
                    num_sessions=1,
                    sleep_after=3,
                    browser=os.getenv("TIKTOK_BROWSER", "chromium"),
                )
                video = api.video(url=url)
                video_info = await video.info()
                tiktok_username = video_info.get("author", {}).get("uniqueId")
                tiktok_video_id = video_info.get("id")
        except Exception as e:
            logger.error(f"Failed to get TikTok video info for URL {url}: {e}")
            raise ValueError(f"Could not resolve TikTok video information for {url}")

    if not tiktok_username or not tiktok_video_id:
        raise ValueError(f"Could not extract TikTok username or video ID from URL: {url}")
    # Download audio and covers (sync)
    audio_path, cover_bytes_list = download_audio_and_covers_from_tiktok(url, tiktok_video_id)
    if not audio_path or not cover_bytes_list:
        raise ValueError("Audio download failed.")

    # Transcribe audio (sync)
    transcript = transcribe_audio(audio_path)

    # Get video description (async)
    description = await get_tiktok_video_description(url)

    context = f"Description: {description}\nTranscript: {transcript}" if description else transcript

    # Extract recipe info (sync)
    recipe_info = extract_recipe_info(context)

    paprika_recipe = PaprikaRecipe(recipe_info, source_url=url, photo_data=cover_bytes_list[0])
    os.remove(audio_path)
    save_cover_images_to_disk(tiktok_video_id, cover_bytes_list, base_dir="/data")

    # Database access (async)
    async with AsyncSessionLocal() as db:
        await create_recipe_from_dict(
            db,
            user_id,
            paprika_recipe.data,
            tiktok_username,
            tiktok_video_id,
        )
    return True


@celery_app.task
def process_tiktok_recipe(user_id, url) -> Literal[True]:
    """
    Synchronous Celery task that wraps the async processing logic.
    This avoids conflicts between the gevent pool and asyncio.
    """
    return asyncio.run(_process_recipe_async(user_id, url))


# Helper: save cover images to disk
def save_cover_images_to_disk(
    tiktok_video_id: str, cover_bytes_list: list, base_dir: str = "data"
) -> list:
    """
    Save each image in cover_bytes_list to data/{user_id}/{tiktok_video_id}/cover_{idx}.jpg
    Returns the list of file paths.
    """
    logger.info(f"Attempting to save cover images for tiktok_video_id: {tiktok_video_id}")
    logger.info(f"Base directory for saving: {base_dir}")
    logger.info(f"Number of images to save: {len(cover_bytes_list)}")

    target_dir = f"{base_dir}/{tiktok_video_id}"
    try:
        os.makedirs(target_dir, exist_ok=True)
        logger.info(f"Ensured directory exists: {target_dir}")
    except OSError as e:
        logger.error(f"Error creating directory {target_dir}: {e}")
        return []

    file_paths = []
    for idx, img_bytes in enumerate(cover_bytes_list):
        file_name = f"cover_{idx + 1}.jpg"
        file_path = os.path.join(target_dir, file_name)
        try:
            with open(file_path, "wb") as f:
                f.write(img_bytes)
            file_paths.append(file_path)
            logger.info(f"Successfully saved image: {file_path}")
        except IOError as e:
            logger.error(f"Error saving image {file_path}: {e}")
        except Exception as e:
            logger.error(f"An unexpected error occurred while saving {file_path}: {e}")

    logger.info(f"Finished saving images. Total saved: {len(file_paths)}")
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
