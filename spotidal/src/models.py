from dataclasses import dataclass
from typing import List


@dataclass
class Track:
    """Represents a music track."""

    title: str
    artist: str


@dataclass
class Playlist:
    """Represents a playlist with tracks."""

    name: str
    tracks: List[Track]

    def reverse(self):
        """Reverse the order of tracks in the playlist."""
        self.tracks.reverse()
        return self
