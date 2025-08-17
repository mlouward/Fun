import json
import logging
import os
from pathlib import Path
from typing import List

import spotipy
from dotenv import load_dotenv
from rich import print
from rich.progress import Progress, SpinnerColumn, TaskID, TextColumn
from spotipy.oauth2 import SpotifyOAuth

from models import Playlist, Track

load_dotenv()

CLIENT_ID = os.getenv("SPOTIFY_CLIENT_ID")
CLIENT_SECRET = os.getenv("SPOTIFY_CLIENT_SECRET")
REDIRECT_URI = "http://127.0.0.1:8888/callback"
SCOPE = "user-library-read playlist-read-private playlist-read-collaborative"


def authenticate_spotify() -> spotipy.Spotify:
    """
    Authenticate the user with Spotify and return a Spotipy client.
    """
    sp_oauth = SpotifyOAuth(
        client_id=CLIENT_ID,
        client_secret=CLIENT_SECRET,
        redirect_uri=REDIRECT_URI,
        scope=SCOPE,
        show_dialog=True,
    )
    token_info = sp_oauth.get_access_token(as_dict=True)
    sp = spotipy.Spotify(auth=token_info["access_token"])
    return sp


def get_all_playlists(sp: spotipy.Spotify) -> List[Playlist]:
    """
    Retrieve all user playlists and their tracks from Spotify, with progress indicator.
    """
    playlists: List[Playlist] = []
    results = sp.current_user_playlists()
    with Progress(SpinnerColumn(), TextColumn("{task.description}")) as progress:
        task = progress.add_task("Loading playlists...")
        while results:
            for item in results["items"]:
                name = item["name"]
                playlist_id = item["id"]
                progress.update(task, description=f"Loading playlist: {name}")
                tracks = get_playlist_tracks(sp, playlist_id, progress, task)
                playlists.append(Playlist(name=name, tracks=tracks))
            if results["next"]:
                results = sp.next(results)
            else:
                break
        progress.update(task, description="Playlists loaded.")
    return playlists


def get_playlist_tracks(
    sp: spotipy.Spotify,
    playlist_id: str,
    progress: Progress | None = None,
    task: TaskID | None = None,
) -> List[Track]:
    """
    Retrieve all tracks from a Spotify playlist, with progress indicator.
    """
    tracks: List[Track] = []
    results = sp.playlist_tracks(playlist_id)
    playlist_name = None
    while results:
        for item in results["items"]:
            track = item["track"]
            if track:
                title = track["name"]
                artist = track["artists"][0]["name"]
                tracks.append(Track(title=title, artist=artist))
                if progress:
                    playlist_name = playlist_name or track["name"]
                    progress.update(
                        task or TaskID(0), description=f"Adding track: {title}"
                    )
        if results["next"]:
            results = sp.next(results)
        else:
            break
    return tracks


def get_liked_tracks(sp: spotipy.Spotify) -> List[Track]:
    """
    Retrieve all liked (saved) tracks from Spotify, with progress indicator.
    """
    tracks: List[Track] = []
    with Progress(SpinnerColumn(), TextColumn("{task.description}")) as progress:
        task = progress.add_task("Loading liked tracks...")
        results = sp.current_user_saved_tracks()
        while results:
            for item in results["items"]:
                track = item["track"]
                if track:
                    title = track["name"]
                    artist = track["artists"][0]["name"]
                    tracks.append(Track(title=title, artist=artist))
                    progress.update(task, description=f"Adding liked track: {title}")
            if results["next"]:
                results = sp.next(results)
            else:
                break
        progress.update(task, description="Liked tracks loaded.")
    return tracks


def export_spotify_data(json_path: str | Path) -> None:
    """
    Export playlists and liked tracks from Spotify to a JSON file.
    """
    logging.basicConfig(
        filename=Path("logs/spotify_export.log"),
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
    )
    sp = authenticate_spotify()
    print(
        "[bold blue]Retrieving playlists and liked tracks from Spotify...[/bold blue]"
    )
    playlists = get_all_playlists(sp)
    liked_tracks = get_liked_tracks(sp)
    print(
        f"[bold green]Found {len(playlists)} playlists and {len(liked_tracks)} liked tracks.[/bold green]"
    )
    data = {
        "playlists": [
            {
                "name": p.name,
                "tracks": [{"title": t.title, "artist": t.artist} for t in p.tracks],
            }
            for p in playlists
        ],
        "liked_tracks": [{"title": t.title, "artist": t.artist} for t in liked_tracks],
    }
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)
    print(f"[bold green]Exported Spotify data to {json_path}[/bold green]")


def main():
    # Path relative to the project root
    export_spotify_data("data/spotify_data.json")


if __name__ == "__main__":
    main()
