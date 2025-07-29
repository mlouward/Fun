import logging

from auth import router as auth_router
from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from models import Base, engine
from recipe import router as recipe_router

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI()


# Configure CORS
@app.middleware("http")
async def log_requests(request: Request, call_next):
    logger.info(f"Incoming request: {request.method} {request.url}")
    response = await call_next(request)
    logger.info(f"Outgoing response: {response.status_code}")
    return response


app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost", "http://[fd00::1]:80", "http://[fd00::2]:80"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

app.include_router(auth_router, prefix="/api/auth")
app.include_router(recipe_router, prefix="/api/recipes")


# Create tables on startup
@app.on_event("startup")
async def on_startup():
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
