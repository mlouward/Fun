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
        return clean_title(self.title)

    def to_searchable(self) -> str:
        """Return a search-friendly version of the track title."""
        title = self.clean_title()
        # if the title is short, append the artist name for better search results
        return title if len(title) > 8 else f"{title} {self.artist.strip().lower()}"


@dataclass
class Playlist:
    """Represents a playlist with tracks."""

    name: str
    tracks: List[Track]


def clean_title(title: str) -> str:
    """Return a cleaned version of the title, without feat. annotations."""
    # Removes anything between parentheses or brackets that indicates a feature or additional info
    cleaned = re.sub(r"\s*\(.*?\)", "", title)
    cleaned = re.sub(r"\s*\[.*?\]", "", cleaned)
    # Spotify often has the edit/remix version in the title, we need to remove those as well
    cleaned = re.sub(r"\s\-\s?.*(mix|edit)", "", cleaned, flags=re.IGNORECASE)
    return cleaned.strip().lower()
