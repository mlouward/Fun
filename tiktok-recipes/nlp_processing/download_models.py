import logging
import os

from transformers import (AutoModelForCausalLM, AutoModelForSpeechSeq2Seq,
                          AutoProcessor, AutoTokenizer)

# Configure basic logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")

# Define model IDs and the target directory for saving
MODELS = {
    "transcriber": "distil-whisper/distil-medium.en",
    "nlp_processor": "microsoft/Phi-4-mini-instruct",
}
CACHE_DIR = os.getenv("MODELS_CACHE_DIR", "/models")


def download_model(model_id: str, model_type: str, cache_dir: str):
    """
    Downloads and caches a specified model and its processor from Hugging Face.

    Args:
        model_id (str): The Hugging Face model identifier.
        model_type (str): The type of model ('transcriber' or 'nlp_processor').
        cache_dir (str): The directory where models should be cached.
    """
    try:
        logging.info(f"Starting download for {model_id}...")

        if model_type == "transcriber":
            # This will download and cache the model and processor
            AutoModelForSpeechSeq2Seq.from_pretrained(model_id, cache_dir=cache_dir)
            AutoProcessor.from_pretrained(model_id, cache_dir=cache_dir)
        elif model_type == "nlp_processor":
            # This will download and cache the model and processor
            AutoModelForCausalLM.from_pretrained(
                model_id, trust_remote_code=True, cache_dir=cache_dir
            )
            AutoTokenizer.from_pretrained(model_id, trust_remote_code=True, cache_dir=cache_dir)
        else:
            logging.error(f"Unknown model type: {model_type}")
            return

        logging.info(f"Successfully cached {model_id} in {cache_dir}")

    except Exception as e:
        logging.error(f"Failed to download model {model_id}. Error: {e}", exc_info=True)


if __name__ == "__main__":
    logging.info(f"Starting model download process. Target directory: {CACHE_DIR}")
    os.makedirs(CACHE_DIR, exist_ok=True)

    for model_type, model_id in MODELS.items():
        download_model(model_id, model_type, CACHE_DIR)

    logging.info("All models have been processed.")
