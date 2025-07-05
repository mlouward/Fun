FROM python:3.13.1-slim

WORKDIR /app

RUN apt update && apt install -y build-essential

# Install system dependencies
RUN pip install uv

# Copy requirements first to leverage Docker cache
COPY pyproject.toml uv.lock .

# Install Python dependencies directly to system Python
RUN uv export --no-dev > requirements.txt && \
    uv pip install --system --extra-index-url "https://download.pytorch.org/whl/cu128" --index-strategy unsafe-best-match -r requirements.txt

# Copy the rest of the application
COPY . .

# Set working directory to celery directory
WORKDIR /app/celery_scripts

# Command to run Celery worker
CMD ["celery", "-A", "celery_worker.celery_app", "worker", "--loglevel=info"]
