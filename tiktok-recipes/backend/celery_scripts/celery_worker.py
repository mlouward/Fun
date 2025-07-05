import os

from celery import Celery

# Get the environment variables
broker_url = os.getenv("CELERY_BROKER_URL", "redis://localhost:6379/0")
backend_url = os.getenv("CELERY_BACKEND_URL", "redis://localhost:6379/0")

# Initialize Celery
celery_app = Celery(
    "tiktok_recipes",
    broker=broker_url,
    backend=backend_url,
)
