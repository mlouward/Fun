import base64
import hashlib
import json
import urllib.parse
from datetime import datetime
from pathlib import Path
from typing import Optional

from rich.console import Console
from rich.panel import Panel

console = Console()


class PaprikaRecipe:
    def __init__(self, data: dict, source_url: str = "", photo_data: Optional[bytes] = None):
        self.data = data.copy()
        self.source_url = source_url
        self.photo_data = photo_data
        self.uid = self._get_data_filename(source_url)
        self.created = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.name = self.data.get("name", "Untitled Recipe")
        self.hash = self._compute_hash()
        self._add_server_fields()

    def _compute_hash(self) -> str:
        hash_content = f"{self.uid}{self.name}{self.data.get('directions', '')}{self.data.get('ingredients', '')}"
        return hashlib.sha256(hash_content.encode()).hexdigest()

    @staticmethod
    def get_data_filename(url: str) -> str:
        if not url:
            return f"untitled_{datetime.now().timestamp()}.json"
        path = urllib.parse.urlparse(url).path
        return path.split("/")[1] + "_" + path.split("/")[-1] + ".json"

    def _add_server_fields(self):
        if self.photo_data:
            self.data["photo_data"] = base64.b64encode(self.photo_data).decode("utf-8")
        self.data.update(
            {
                "uid": self.uid,
                "created": self.created,
                "hash": self.hash,
                "source": "TikTok",
                "source_url": self.source_url,
                "notes": self.data.get("notes", None),
                "rating": self.data.get("rating", 0),
                "image_url": self.data.get("image_url", None),
                "on_favorites": self.data.get("on_favorites", 0),
                "photo_hash": self.data.get("photo_hash", None),
                "photo": self.data.get("photo", None),
                "scale": self.data.get("scale", None),
                "deleted": self.data.get("deleted", False),
                "categories": self.data.get("categories", []),
            }
        )

    def to_dict(self) -> dict:
        return self.data

    def to_json(self) -> str:
        return json.dumps(self.data, indent=4)

    def save(self, output_dir: Path = Path("data")) -> Path:
        output_dir.mkdir(exist_ok=True)
        file_path = output_dir / self.uid
        try:
            with open(file_path, "w") as f:
                json.dump(self.data, f, indent=4)
            console.print(Panel(f"Recipe saved to {file_path}", style="green"))
        except Exception as e:
            console.print(Panel(f"[bold red]Failed to save recipe:[/bold red] {e}", style="red"))
            raise
        return file_path

    @classmethod
    def from_file(cls, file_path: Path) -> "PaprikaRecipe":
        """
        Loads a PaprikaRecipe from a JSON file.
        """
        with open(file_path, "r") as f:
            data = json.load(f)
        # Try to extract source_url and photo_data if present
        source_url = data.get("source_url", "")
        photo_data = None
        if "photo_data" in data:
            try:
                photo_data = base64.b64decode(data["photo_data"])
            except Exception:
                photo_data = None
        return cls(data, source_url=source_url, photo_data=photo_data)

    # For backward compatibility, alias _get_data_filename to get_data_filename
    _get_data_filename = get_data_filename


def format_for_paprika(
    recipe_dict: dict, source_url: str = "", photo_data: bytes | None = None
) -> Path:
    """
    Takes a recipe dictionary, adds server-side Paprika fields, and saves it.
    Optionally embeds photo_data (JPEG bytes) as base64 in the JSON.

    Args:
        recipe_dict (dict): The dictionary of recipe info from the NLP module.
        source_url (str): The original URL of the TikTok video.
        photo_data (bytes | None): Optional JPEG image data to be embedded.

    Returns:
        Path: The path to the saved JSON file.
    """
    recipe = PaprikaRecipe(recipe_dict, source_url, photo_data)
    return recipe.save()


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
        console.print(
            f"\n[bold green]✅ Success! Paprika-formatted recipe saved at: {saved_file}[/bold green]"
        )
