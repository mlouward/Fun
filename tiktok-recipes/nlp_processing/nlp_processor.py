import json
import logging
import os
import re
from pathlib import Path

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, BitsAndBytesConfig, pipeline

# Configure logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

# --- Model Configuration ---
model = None
tokenizer = None
MODEL_ID = "microsoft/Phi-4-mini-instruct"
CACHE_DIR = os.getenv("MODELS_CACHE_DIR", "/models")


def load_model():
    """Loads the Phi-4 model and tokenizer from the local cache with 4-bit quantization."""
    global model, tokenizer
    if model is not None and tokenizer is not None:
        logger.info("Phi-4 model and tokenizer already loaded.")
        return

    try:
        logger.info(f"Loading quantized Phi-4 model from: {MODEL_ID}")

        # Configure 4-bit quantization
        quantization_config = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_compute_dtype=torch.bfloat16,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_use_double_quant=True,
        )

        device = "cuda" if torch.cuda.is_available() else "cpu"
        if device == "cpu":
            logger.warning("GPU not found, loading model on CPU without quantization.")
            quantization_config = None

        model = AutoModelForCausalLM.from_pretrained(
            MODEL_ID,
            trust_remote_code=True,
            device_map=device,
            quantization_config=quantization_config,
            cache_dir=CACHE_DIR,
        )
        tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, cache_dir=CACHE_DIR)

        logger.info("Quantized Phi-4 model and tokenizer loaded successfully.")

    except Exception as e:
        logger.error(f"Error loading Phi-4 model: {e}", exc_info=True)
        model = None
        tokenizer = None


def _format_list_string(text: str) -> str:
    """Ensures newlines before list items starting with '-' or '1.'."""
    if not isinstance(text, str):
        return text
    # Add newline before '- ' if not already preceded by a newline
    text = re.sub(r"([^\n])(- )", r"\1\n\2", text)
    # Add newline before 'n. ' if not already preceded by a newline
    text = re.sub(r"([^\n])(\d\. )", r"\1\n\2", text)
    return text


def build_prompt(transcript: str) -> str:
    """Builds a detailed, structured prompt for the Phi-4 model."""

    # Load the JSON schema to instruct the model on the output format
    schema_path = Path(__file__).parent / "paprika_schema.json"
    with open(schema_path, "r") as f:
        schema = json.load(f)

    # Convert schema to a clean string for the prompt
    schema_string = json.dumps(schema["properties"], indent=2)

    prompt = f"""
<|system|>
You are an expert recipe extraction AI. Your task is to analyze the provided recipe transcript and accurately extract the information into a structured JSON format.

You must adhere to the following rules:
1.  **JSON Output Only**: Your entire output must be a single, valid JSON object. Do not include any text or explanations before or after the JSON.
2.  **Strict Schema**: The JSON object must strictly conform to the following schema. Do not add any extra fields.
3.  **Accurate Extraction**: Extract measurements, ingredients, and steps precisely as mentioned in the transcript.
4.  **Data Types**: Ensure all fields have the correct data type as specified in the schema (e.g., 'servings' must be an integer).
5.  **Formatting**:
    - `ingredients`: List each ingredient on a new line (use '\n').
    - `directions`: Number each step and list it on a new line (e.g., "1. First step.\n2. Second step.").

**JSON Schema to Follow:**
```json
{schema_string}
```
<|end|>

<|user|>
Here is the recipe transcript. Please extract the information into the specified JSON format.

**Transcript:**
{transcript}
<|end|>

<|assistant|>
```json
"""
    return prompt


def extract_recipe_info(transcript: str) -> dict:
    """Extracts structured recipe information from a transcript using Phi-4."""
    if not model or not tokenizer:
        logger.error("Phi-4 model not loaded. Cannot process transcript.")
        return {}

    logger.info("Starting recipe extraction with Phi-4.")

    prompt = build_prompt(transcript)

    # Use a pipeline for efficient generation
    pipe = pipeline(
        "text-generation",
        model=model,
        tokenizer=tokenizer,
    )

    generation_args = {
        "max_new_tokens": 1024,
        "return_full_text": False,
        "temperature": 0.0,
        "do_sample": False,
    }

    try:
        logger.debug("Generating response from Phi-4.")
        output = pipe(prompt, **generation_args)
        response_text = output[0]["generated_text"]

        # Clean up the response to ensure it's valid JSON
        if response_text.strip().startswith("```json"):
            response_text = response_text.split("```json")[1]

        if "```" in response_text:
            response_text = response_text.split("```")[0]

        last_brace = response_text.rfind("}")
        if last_brace != -1:
            response_text = response_text[: last_brace + 1]

        logger.info(f"Raw model output (cleaned):\n{response_text}")

        recipe_info = json.loads(response_text)

        if "ingredients" in recipe_info and isinstance(recipe_info["ingredients"], str):
            recipe_info["ingredients"] = _format_list_string(recipe_info["ingredients"])
        if "directions" in recipe_info and isinstance(recipe_info["directions"], str):
            recipe_info["directions"] = _format_list_string(recipe_info["directions"])

        logger.info("Successfully parsed JSON from Phi-4 output.")
        return recipe_info

    except json.JSONDecodeError as e:
        logger.error(
            f"Failed to decode JSON from model response: {e}",
            extra={"raw_response": response_text},
        )
        return {}
    except Exception as e:
        logger.error(f"An unexpected error occurred during extraction: {e}", exc_info=True)
        return {}
