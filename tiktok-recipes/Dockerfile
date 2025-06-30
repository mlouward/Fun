FROM pytorch/pytorch:2.3.0-cuda12.1-cudnn8-runtime

WORKDIR /app

COPY pyproject.toml README.md ./
COPY src ./src
COPY audio_files ./audio_files
COPY data ./data

# Install uv first
RUN pip install --no-cache-dir uv
# Create a virtual environment with uv
RUN uv venv .venv
# Activate the venv and install dependencies
RUN uv pip install .

# Expose port
EXPOSE 8000

CMD [".venv/bin/uvicorn", "src.main:app", "--host", "0.0.0.0", "--port", "8000"]
