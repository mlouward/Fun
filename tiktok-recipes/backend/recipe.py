import base64
import gzip
import io
import json
import logging
import os
import re
from typing import Any, Optional

from auth import ALGORITHM, SECRET_KEY
from celery_app import celery_app
from fastapi import APIRouter, Body, Depends, HTTPException, Query
from fastapi.responses import JSONResponse, StreamingResponse
from fastapi.security import OAuth2PasswordBearer
from jose import JWTError, jwt
from models import AsyncSessionLocal, Recipe, User
from pydantic import BaseModel
from sqlalchemy import and_, select
from sqlalchemy.ext.asyncio import AsyncSession

# Set up basic logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

router = APIRouter(tags=["recipes"])

oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/auth/login")


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


def get_cover_images_from_disk(user_id: int, tiktok_video_id: str, base_dir: str = "data") -> list:
    """
    Return the list of base64-encoded image strings for a given user and tiktok_video_id.
    """
    dir_path = f"{base_dir}/{user_id}/{tiktok_video_id}"
    if not os.path.exists(dir_path):
        return []
    files = [f for f in os.listdir(dir_path) if f.startswith("cover_") and f.endswith(".jpg")]
    files.sort()
    images = []
    for f in files:
        try:
            with open(os.path.join(dir_path, f), "rb") as imgf:
                img_bytes = imgf.read()
                images.append(base64.b64encode(img_bytes).decode("utf-8"))
        except Exception:
            images.append(None)
    return images


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


@router.get("/", tags=["Recipe Gallery"])
async def list_recipes(
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user),
    limit: Optional[int] = Query(None, ge=1, le=100),
    offset: Optional[int] = Query(None, ge=0),
):
    query = select(Recipe).where(Recipe.user_id == current_user.id)
    total_result = await db.execute(query)
    total_count = len(total_result.scalars().all())
    if limit is not None and offset is not None:
        query = query.offset(offset).limit(limit)
    result = await db.execute(query)
    recipes = result.scalars().all()
    out = []
    for r in recipes:
        cover_images_b64 = get_cover_images_from_disk(r.user_id, r.tiktok_video_id)
        out.append(
            {
                "title": r.title,
                "servings": r.servings,
                "prep_time": r.prep_time,
                "cook_time": r.cook_time,
                "ingredients": r.ingredients,
                "instructions": r.instructions,
                "cover_image_idx": r.cover_image_idx,
                "tiktok_username": r.tiktok_username,
                "tiktok_video_id": r.tiktok_video_id,
                "cover_images": cover_images_b64,
            }
        )
    return JSONResponse(content={"total": total_count, "recipes": out})


@router.post("/process_url", tags=["Recipe Processing"])
async def process_tiktok_url(
    request: ProcessRequest = Body(...),
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> dict[Any, Any]:
    try:
        print(request, current_user)
        match = re.search(r"tiktok\.com/@([\w.]+)/video/(\d+)", request.url)
        tiktok_username = match.group(1) if match else None
        tiktok_video_id = match.group(2) if match else None
        if not tiktok_username or not tiktok_video_id:
            raise HTTPException(status_code=400, detail="Invalid TikTok URL format.")
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
        celery_app.send_task(
            "worker.tasks.process_tiktok_recipe",
            args=[current_user.id, request.url, tiktok_username, tiktok_video_id],
        )
        return {"message": "Recipe is being processed. You will be notified when it is ready."}
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
        logger.info("[update_recipe] Saved recipe")
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
        logger.error(f"[update_recipe] Error: {e}")
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


@router.delete("/delete_recipe", tags=["Recipe Editing"])
async def delete_recipe(
    data: dict = Body(...),
    db: AsyncSession = Depends(get_db),
    current_user: User = Depends(get_current_user),
):
    tiktok_username = data.get("tiktok_username")
    tiktok_video_id = data.get("tiktok_video_id")
    title = data.get("title")
    recipe = await get_user_recipe(db, current_user.id, tiktok_username, tiktok_video_id, title)
    if not recipe:
        raise HTTPException(status_code=404, detail="Recipe not found for deletion.")
    await db.delete(recipe)
    await db.commit()
    return {"message": "Recipe deleted successfully!"}
