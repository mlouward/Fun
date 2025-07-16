FROM python:3.13.1-slim

WORKDIR /app

RUN apt update && apt install -y build-essential

# Install system dependencies
RUN pip install uv

# Copy requirements first to leverage Docker cache
COPY pyproject.toml uv.lock .

# Install Python dependencies directly to system Python
RUN uv sync --locked --no-dev

# Copy the rest of the application
COPY . .

# Expose the port the app runs on
EXPOSE 8000

# Add virtual environment to PATH
ENV PATH="/app/.venv/bin:$PATH"

# Command to run the application
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
