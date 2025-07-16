import logging
import os
from pathlib import Path

import torch
from transformers import AutoModelForSpeechSeq2Seq, AutoProcessor

# Configure logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

# Global variables for the model and processor
model = None
processor = None
MODEL_ID = "distil-whisper/distil-medium.en"
CACHE_DIR = os.getenv("MODELS_CACHE_DIR", "/models")


def load_model():
    """Loads the Distil-Whisper model and processor from the local cache."""
    global model, processor
    if model and processor:
        logger.info("Distil-Whisper model and processor already loaded.")
        return

    try:
        logger.info(f"Loading Distil-Whisper model from: {MODEL_ID}")

        device = "cuda" if torch.cuda.is_available() else "cpu"
        torch_dtype = torch.float16 if torch.cuda.is_available() else torch.float32

        model = AutoModelForSpeechSeq2Seq.from_pretrained(
            MODEL_ID,
            torch_dtype=torch_dtype,
            low_cpu_mem_usage=True,
            use_safetensors=True,
            cache_dir=CACHE_DIR,
        )
        model.to(device)

        processor = AutoProcessor.from_pretrained(MODEL_ID, cache_dir=CACHE_DIR)

        logger.info("Distil-Whisper model and processor loaded successfully.")

    except Exception as e:
        logger.error(f"Error loading Distil-Whisper model: {e}", exc_info=True)
        model = None
        processor = None


def transcribe_audio(audio_path: Path) -> str:
    """Transcribes the audio file using the Distil-Whisper model."""
    if not model or not processor:
        logger.error("Distil-Whisper model not loaded. Cannot transcribe audio.")
        return ""
    try:
        logger.info(f"Transcribing audio file: {audio_path}")

        # The model is already on the correct device from the load_model function
        # The pipeline abstracts away the device placement and batching
        from transformers import pipeline

        pipe = pipeline(
            "automatic-speech-recognition",
            model=model,
            tokenizer=processor.tokenizer,
            feature_extractor=processor.feature_extractor,
            max_new_tokens=128,
            chunk_length_s=15,
            batch_size=16,
            return_timestamps=False,
            torch_dtype=model.dtype,
            device=model.device,
        )

        result = pipe(str(audio_path))
        transcribed_text = result["text"]

        logger.info("Transcription successful.")
        logger.debug(f"Transcription result: {transcribed_text}")
        return str(transcribed_text)

    except Exception as e:
        logger.error(f"An error occurred during transcription: {e}", exc_info=True)
        return ""
