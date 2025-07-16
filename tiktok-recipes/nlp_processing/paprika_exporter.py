import base64
import hashlib
import json
import urllib.parse
from datetime import datetime
from pathlib import Path
from typing import Optional


class PaprikaRecipe:
    def __init__(self, data: dict, source_url: str = "", photo_data: Optional[bytes] = None):
        self.data = data.copy()
        self.source_url = source_url
        self.photo_data = photo_data
        self.uid = self.get_data_filename(source_url)
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
        with open(file_path, "w") as f:
            json.dump(self.data, f, indent=4)
        return file_path

    @classmethod
    def from_file(cls, file_path: Path) -> "PaprikaRecipe":
        with open(file_path, "r") as f:
            data = json.load(f)
        source_url = data.get("source_url", "")
        photo_data = None
        if "photo_data" in data:
            try:
                photo_data = base64.b64decode(data["photo_data"])
            except Exception:
                photo_data = None
        return cls(data, source_url=source_url, photo_data=photo_data)
