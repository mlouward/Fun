import logging
import os

from celery import Celery
from celery.signals import worker_init

# Configure logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)


@worker_init.connect
def on_worker_init(**kwargs):
    """Function to initialize models when a worker process starts."""
    logger.info("Worker process initializing. Loading ML models...")
    # We import here to avoid circular dependencies and ensure code is loaded in the worker.
    from nlp_processor import load_model as load_nlp_model
    from transcriber import load_model as load_transcriber_model

    load_transcriber_model()
    load_nlp_model()
    logger.info("ML models loading process completed.")


# Get the environment variables for broker and backend URLs
broker_url = os.getenv("CELERY_BROKER_URL", "redis://redis:6379/0")
backend_url = os.getenv("CELERY_BACKEND_URL", "redis://redis:6379/0")

logger.info(f"Connecting Celery to broker: {broker_url} and backend: {backend_url}")

# Initialize Celery
celery_app = Celery(
    "tiktok_recipes",
    broker=broker_url,
    backend=backend_url,
    include=["worker.tasks"],
)

# Optional configuration
celery_app.conf.update(task_serializer="json", accept_content=["json"], result_serializer="json")
