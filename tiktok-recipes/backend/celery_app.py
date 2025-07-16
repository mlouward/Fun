import os

from celery import Celery

broker_url = os.getenv("CELERY_BROKER_URL", "redis://localhost:6379/0")
backend_url = os.getenv("CELERY_BACKEND_URL", "redis://localhost:6379/0")

# Initialize Celery
celery_app = Celery(
    "backend",
    broker=broker_url,
    backend=backend_url,
    include=[],  # No tasks are loaded by the backend service
)

celery_app.conf.update(task_serializer="json", accept_content=["json"], result_serializer="json")
