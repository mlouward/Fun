# FastAPI app entrypoint (migrated from src/main.py)
from fastapi import FastAPI

from .audio_extractor import download_audio_and_cover_from_tiktok  # noqa: F401
from .auth import router as auth_router
from .models import Base, engine
from .nlp_processor import extract_recipe_info  # noqa: F401
from .paprika_exporter import PaprikaRecipe  # noqa: F401
from .recipe import router as recipe_router
from .tiktok_description import get_tiktok_video_description  # noqa: F401
from .transcriber import transcribe_audio  # noqa: F401

app = FastAPI()

app.include_router(auth_router, prefix="/api/auth")
app.include_router(recipe_router, prefix="/api/recipes")


# Create tables on startup
@app.on_event("startup")
async def on_startup():
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
