import hashlib
import json
import urllib.parse
from datetime import datetime
from pathlib import Path


def format_for_paprika(recipe_dict: dict, source_url: str = "") -> Path:
    """
    Takes a recipe dictionary, adds server-side Paprika fields, and saves it.

    Args:
        recipe_dict (dict): The dictionary of recipe info from the NLP module.
        source_url (str): The original URL of the TikTok video.

    Returns:
        Path: The path to the saved JSON file.
    """
    output_dir = Path("data")
    output_dir.mkdir(exist_ok=True)

    # Start with the data from the NLP module
    final_recipe = recipe_dict.copy()

    # Generate and add server-side fields. Filename is tiktok video id + channel name (starts with @)
    uid = get_data_filename(source_url)

    created_timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    name = final_recipe.get("name", "Untitled Recipe")

    # Create a hash based on the core content
    hash_content = (
        f"{uid}{name}{final_recipe.get('directions', '')}{final_recipe.get('ingredients', '')}"
    )
    recipe_hash = hashlib.sha256(hash_content.encode()).hexdigest()

    # Update the dictionary with all required fields
    final_recipe.update(
        {
            "uid": uid,
            "created": created_timestamp,
            "hash": recipe_hash,
            "source": "TikTok",
            "source_url": source_url,
            # Add other default Paprika fields if they're not present
            "notes": final_recipe.get("notes", None),
            "rating": final_recipe.get("rating", 0),
            "image_url": final_recipe.get("image_url", None),
            "on_favorites": final_recipe.get("on_favorites", 0),
            "photo_hash": final_recipe.get("photo_hash", None),
            "photo": final_recipe.get("photo", None),
            "scale": final_recipe.get("scale", None),
            "deleted": final_recipe.get("deleted", False),
            "categories": final_recipe.get("categories", []),
        }
    )

    # Save to a JSON file
    file_path = output_dir / uid
    with open(file_path, "w") as f:
        json.dump(final_recipe, f, indent=4)

    print(f"Recipe saved to {file_path}")
    return file_path


def get_data_filename(url: str) -> str:
    path = urllib.parse.urlparse(url).path
    return path.split("/")[1] + "_" + path.split("/")[-1] + ".json"


# --- Main execution block for testing ---
if __name__ == "__main__":
    # This is a sample dictionary that our nlp_processor would produce
    test_nlp_output = {
        "name": "Simple Chocolate Cake",
        "ingredients": "1 cup of flour\n2 eggs\n1/2 cup of sugar",
        "directions": "1. Mix the dry ingredients.\n2. Add the eggs and stir until smooth.\n3. Bake for 30 minutes at 350 degrees.",
        "prep_time": "10 minutes",
        "cook_time": "30 minutes",
        "servings": "8 servings",
        "notes": "Let it cool completely before frosting.",
    }

    saved_file = format_for_paprika(
        recipe_dict=test_nlp_output, source_url="https://www.tiktok.com/@somebaker/video/12345"
    )

    if saved_file.exists():
        print(f"\n✅ Success! Paprika-formatted recipe saved at: {saved_file}")
