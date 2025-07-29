# Stage 1: Build the application and download models
FROM python:3.13.1-slim AS builder

WORKDIR /app

# Install system dependencies required for building wheels, including gnupg for key management
RUN apt update && \
    apt install -y build-essential curl gnupg && \
    rm -rf /var/lib/apt/lists/*

# Add NVIDIA's repository for CUDA
RUN curl -fsSL https://developer.download.nvidia.com/compute/cuda/repos/debian12/x86_64/3bf863cc.pub | apt-key add - && \
    echo "deb https://developer.download.nvidia.com/compute/cuda/repos/debian12/x86_64/ /" > /etc/apt/sources.list.d/cuda.list && \
    apt update && \
    apt install -y --no-install-recommends cuda-toolkit-12-4

# Set environment variables for CUDA
ENV CUDA_HOME=/usr/local/cuda-12.4
ENV PATH=$CUDA_HOME/bin:$PATH
ENV LD_LIBRARY_PATH=$CUDA_HOME/lib64:$LD_LIBRARY_PATH

# Install uv
RUN pip install uv

# Copy only the dependency files to leverage Docker layer caching
COPY pyproject.toml uv.lock ./

# Install build dependencies
RUN uv pip install --system --extra-index-url "https://download.pytorch.org/whl/cu124" torch einops psutil wheel packaging ninja

# install Python dependencies
RUN uv pip install --system --no-build-isolation --extra-index-url "https://download.pytorch.org/whl/cu124" .


# Copy the model download script
COPY download_models.py .

# Download models into a cache directory
# This directory will be copied to the final stage
RUN python download_models.py


# Stage 2: Create the final, lean image
FROM python:3.13.1-slim

WORKDIR /app

# Install system dependencies required for runtime, including CUDA runtime and build tools for JIT
RUN apt update && \
    apt install -y --no-install-recommends curl gnupg build-essential && \
    curl -fsSL https://developer.download.nvidia.com/compute/cuda/repos/debian12/x86_64/3bf863cc.pub | apt-key add - && \
    echo "deb https://developer.download.nvidia.com/compute/cuda/repos/debian12/x86_64/ /" > /etc/apt/sources.list.d/cuda.list && \
    apt update && \
    apt install -y --no-install-recommends ffmpeg cuda-libraries-12-4 && \
    rm -rf /var/lib/apt/lists/*

# Copy installed Python packages from the builder stage
COPY --from=builder /usr/local/lib/python3.13/site-packages/ /usr/local/lib/python3.13/site-packages/
COPY --from=builder /usr/local/bin/ /usr/local/bin/

# Copy the downloaded models from the builder stage
COPY --from=builder /models /models

# Install Playwright and its browsers
RUN uv pip install --system playwright
RUN playwright install --with-deps chromium

# Copy the application code
COPY . .

# Set the environment variable for the models cache
ENV MODELS_CACHE_DIR=/models

# Command to run the Celery worker
CMD ["celery", "-A", "worker.celery_worker.celery_app", "worker", "--loglevel=info", "--pool=gevent"]
