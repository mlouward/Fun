FROM python:3.13-slim

WORKDIR /app

COPY pyproject.toml README.md ./
COPY src ./src
COPY audio_files ./audio_files
COPY data ./data

# Install build dependencies
RUN pip install --upgrade pip
RUN pip install --no-cache-dir --upgrade pip setuptools wheel

# Install project dependencies
RUN pip install --no-cache-dir .

# Expose port
EXPOSE 8000

CMD ["uvicorn", "src.main:app", "--host", "0.0.0.0", "--port", "8000"]
