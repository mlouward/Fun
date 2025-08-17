import re
from dataclasses import dataclass
from typing import List


@dataclass
class Track:
    """Represents a music track."""

    title: str
    artist: str

    def clean_title(self) -> str:
        """Return a cleaned version of the track title, without feat. annotations. and lowercased."""
        # Removes (feat.) and similar annotations from the title
        title = re.sub(r"\s*\(feat\..*?\)", "", self.title)
        return title.strip().lower()

    def to_searchable(self) -> str:
        """Return a search-friendly version of the track title."""
        return self.clean_title() + " " + self.artist.strip().lower()


@dataclass
class Playlist:
    """Represents a playlist with tracks."""

    name: str
    tracks: List[Track]
