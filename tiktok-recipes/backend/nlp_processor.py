import json
from pathlib import Path

import torch
from rich.console import Console
from rich.panel import Panel
from transformers import AutoModelForVision2Seq, AutoProcessor

console = Console()

MODEL_ID = "numind/NuExtract-2.0-2B"

try:
    console.rule("[bold blue]NLP Model Loading")
    console.print(f"[yellow]Loading model:[/yellow] {MODEL_ID}")
    model = AutoModelForVision2Seq.from_pretrained(
        MODEL_ID,
        trust_remote_code=True,
        torch_dtype=torch.bfloat16,
        device_map="auto",
    )
    processor = AutoProcessor.from_pretrained(
        MODEL_ID, trust_remote_code=True, padding_side="left", use_fast=True
    )
    console.print("[green]Model and processor loaded successfully.[/green]")
except Exception as e:
    console.print(f"[bold red]Error loading model:[/bold red] {e}")
    model = None
    processor = None


def extract_recipe_info(transcript: str) -> dict:
    console.rule("[bold blue]NLP Extraction")
    if not model or not processor:
        console.print("[bold red]Model not loaded. Cannot process transcript.[/bold red]")
        return {}
    schema_path = Path(__file__).parent / "paprika_schema.json"
    with open(schema_path, "r") as f:
        template = json.load(f)["properties"]
    template = {k: v["type"] for k, v in template.items()}
    template_str = json.dumps(template)
    examples = [
        {
            "input": "Hey everyone, today we're making a simple chocolate cake. "
            "You'll need one cup of flour, two eggs, and a half cup of sugar. "
            "This recipe is very easy and takes about 10 minutes to prep and 30 minutes in the oven. "
            "First, mix the dry ingredients in a large bowl. Then, add the eggs and stir until smooth. "
            "Bake it for about 30 minutes at 350 degrees. Let it cool and enjoy! It serves 8 people.",
            "output": "{'name':'Chocolate Cake','directions':'1. Mix the dry ingredients in a large bowl.\\n2. Add the eggs and stir until smooth.\\n3. Bake it for about 30 minutes at 350 degrees.','ingredients':'1 cup of flour\\n2 eggs\\n1/2 cup of sugar','servings':'8','difficulty':'Easy','cook_time':'30','prep_time':'10'}",
        }
    ]
    prompt_instructions = (
        "When extracting the recipe fields, always output 'servings', 'prep_time', and 'cook_time' as integers only (no units, just the number of minutes or servings). "
        "For example, use 'servings': '4', 'prep_time': '10', 'cook_time': '30'. Do not include words like 'minutes', 'people', or 'servings'. "
        "Always format the ingredients using the measurement before the name. You must add a newline between ingredients. "
        "You must use the provided JSON template. Make sure you output a valid JSON, closing all braces and quotes. "
    )
    messages = [
        {
            "role": "user",
            "content": f"""You are an expert at structured information extraction for recipes. Your goal is to provide accurate information (measurements, steps) to reproduce a recipe. {prompt_instructions}\n
            The following TEXT includes the description and video transcript of a recipe. TEXT: \n{transcript}""",
        }
    ]
    prompt = processor.apply_chat_template(
        messages,
        template=template_str,
        examples=examples,
        add_generation_prompt=True,
        tokenize=False,
    )
    console.print("[cyan]Prompt constructed for model inference.[/cyan]")
    inputs = processor(text=[prompt], padding=True, return_tensors="pt").to(model.device)
    output_ids = model.generate(**inputs, max_new_tokens=2048, do_sample=False, num_beams=1)
    output_ids_trimmed = [
        out_ids[len(in_ids) :] for in_ids, out_ids in zip(inputs.input_ids, output_ids)
    ]
    console.print(f"[yellow]Number of new tokens:[/yellow] {len(output_ids_trimmed[0])}")
    response = processor.batch_decode(output_ids_trimmed, skip_special_tokens=True)[0]
    try:
        console.print(Panel(f"Raw model output:\n{response}", style="magenta"))
        recipe_info = json.loads(response)
        return recipe_info
    except (IndexError, json.JSONDecodeError) as e:
        console.print(f"[bold red]Error parsing JSON from model output:[/bold red] {e}")
        return {}
