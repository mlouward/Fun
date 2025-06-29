import json
from pathlib import Path

import torch
from transformers import AutoModelForVision2Seq, AutoProcessor

# --- Model and Processor Loading ---
MODEL_ID = "numind/NuExtract-2.0-2B"

try:
    print(f"Loading model: {MODEL_ID}")
    model = AutoModelForVision2Seq.from_pretrained(
        MODEL_ID,
        trust_remote_code=True,
        torch_dtype=torch.bfloat16,
        device_map="auto",
    )
    processor = AutoProcessor.from_pretrained(
        MODEL_ID, trust_remote_code=True, padding_side="left", use_fast=True
    )
    print("Model and processor loaded successfully.")
except Exception as e:
    print(f"Error loading model: {e}")
    model = None
    processor = None


def extract_recipe_info(transcript: str) -> dict:
    """
    Uses the NuExtract-2.0-2B model to extract structured recipe data from a transcript.

    Args:
        transcript (str): The raw text transcribed from the recipe video.

    Returns:
        dict: A structured dictionary containing the extracted recipe information.
    """
    if not model or not processor:
        print("Model not loaded. Cannot process transcript.")
        return {}

    # Load the JSON schema to use as a template
    schema_path = Path(__file__).parent / "paprika_schema.json"
    with open(schema_path, "r") as f:
        template = json.load(f)["properties"]
    # only keep the properties and their types, not the descriptions
    template = {k: v["type"] for k, v in template.items()}
    template_str = json.dumps(template)
    # Create a list of example input/output pairs. The input is the transcript, the output is the JSON schema.
    examples = [
        {
            "input": """Hey everyone, today we're making a simple chocolate cake. "
            "You'll need one cup of flour, two eggs, and a half cup of sugar. "
            "This recipe is very easy and takes about 10 minutes to prep and 30 minutes in the oven. "
            "First, mix the dry ingredients in a large bowl. Then, add the eggs and stir until smooth. "
            "Bake it for about 30 minutes at 350 degrees. Let it cool and enjoy! It serves 8 people.""",
            "output": "{'name':'Chocolate Cake','directions':'1. Mix the dry ingredients in a large bowl.\\n2. Add the eggs and stir until smooth.\\n3. Bake it for about 30 minutes at 350 degrees.','ingredients':'1 cup of flour\\n2 eggs\\n1/2 cup of sugar','servings':'8 people','difficulty':'Easy','cook_time':'30 minutes','prep_time':'10 minutes'}",
        }
    ]

    # Construct the prompt using the chat format for NuExtract 2.0
    messages = [
        {
            "role": "user",
            "content": f"""You are an expert at structured information extraction for recipes.
              Your goal is to provide accurate information (measurements, steps) to reproduce a recipe.
              Always format the ingredients using the measurement, then the name. You must add a newline between ingredients.
              The following TEXT includes the description and video transcript of a recipe.
              You must use the provided JSON template. Make sure you output a valid JSON,
              closing all braces and quotes. TEXT: {transcript}""",
        }
    ]

    # Apply chat template and generate
    prompt = processor.apply_chat_template(
        messages,
        template=template_str,
        examples=examples,
        add_generation_prompt=True,
        tokenize=False,
    )
    inputs = processor(text=[prompt], padding=True, return_tensors="pt").to(model.device)

    output_ids = model.generate(**inputs, max_new_tokens=2048, do_sample=False, num_beams=1)
    output_ids_trimmed = [
        out_ids[len(in_ids) :] for in_ids, out_ids in zip(inputs.input_ids, output_ids)
    ]
    print(f"Number of new tokens: {len(output_ids_trimmed[0])}")
    # Decode and parse the output
    response = processor.batch_decode(output_ids_trimmed, skip_special_tokens=True)[0]

    try:
        # Print the raw model output for debugging
        print(f"Raw model output:\n{response}")
        recipe_info = json.loads(response)
        return recipe_info
    except (IndexError, json.JSONDecodeError) as e:
        print(f"Error parsing JSON from model output: {e}")
        return {}


# --- Main execution block for testing ---
if __name__ == "__main__":
    test_transcript = """Hello, today is a recipe for a lemon pie. "
        "You'll need 2 large lemons, 1 cup of sugar, 1/2 cup of butter, and 1/2 cup of flour. "
        "This recipe is very easy and takes about 10 minutes to prep and 30 minutes in the oven. "
        "First, mix the dry ingredients in a large bowl. Then, add the eggs and stir until smooth. "
        "Add the lemon zests and mix well. Then, add the butter and mix until the butter is melted. "
        "Bake it for about 30 minutes at 200C. Let it cool and enjoy! It serves 8 people."""

    print("--- Running Test Extraction with NuExtract 2.0 ---")
    recipe_info = extract_recipe_info(test_transcript)

    if recipe_info:
        print("\n--- Extracted Recipe Info ---")
        print(json.dumps(recipe_info, indent=2))
        print("\n✅ Success! NLP processor test complete.")
    else:
        print("\n❌ Failure. Could not extract recipe info.")
