import json
import logging
import os
from difflib import SequenceMatcher
from pathlib import Path
from typing import Any

from dotenv import load_dotenv
from rich import print
from rich.progress import Progress, TaskID
from rich.progress import track as progress_tracker
from tidalapi import Session, media
from tidalapi.playlist import UserPlaylist

from models import Playlist, Track, clean_title

load_dotenv()


# Tidal API configuration
AUTHORIZATION_ENDPOINT = "https://login.tidal.com/authorize"
TOKEN_ENDPOINT = "https://auth.tidal.com/v1/oauth2/token"
CLIENT_ID = os.getenv("TIDAL_CLIENT_ID")
CLIENT_SECRET = os.getenv("TIDAL_CLIENT_SECRET")

SESSION_FILE = Path("data/tidal_session.json")

logging.basicConfig(
    filename=Path("logs/tidal.log"),
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
)


def authenticate_tidal() -> Session | None:
    """
    Authenticate the user with Tidal using OAuth and return a Session object.

    Returns:
        Session: Authenticated Tidal session, or None if authentication failed.
    """
    session = Session()
    try:
        session.login_pkce()
        return session
    except Exception as e:
        logging.error(f"Authentication failed: {e}")
        print(
            "[bold red]Authentication failed. Check tidal.log for details.[/bold red]"
        )
        return None


def save_tidal_session(session: Session, session_file: Path = SESSION_FILE) -> None:
    """
    Save the current Tidal session to a file.
    """
    try:
        session.save_session_to_file(session_file)
        logging.info(f"Tidal session saved to {session_file}")
    except Exception as e:
        logging.error(f"Failed to save Tidal session: {e}")
        print(f"[bold red]Failed to save Tidal session: {e}[/bold red]")


def load_tidal_session(session_file: Path = SESSION_FILE) -> Session | None:
    """
    Load a Tidal session from a file, or return None if not valid.
    """
    session = Session()
    try:
        ok = session.login_session_file(session_file, do_pkce=True)
        if ok:
            logging.info(f"Loaded Tidal session from {session_file}")
            return session
        else:
            logging.warning(f"Session file {session_file} invalid, login required.")
            return None
    except Exception as e:
        logging.error(f"Failed to load Tidal session: {e}")
        print(
            "[bold yellow]No valid Tidal session found, login required.[/bold yellow]"
        )
        return None


def load_spotify_data(json_path: str | Path) -> tuple[list[Playlist], list[Track]]:
    """
    Load playlists and liked tracks from the Spotify export JSON file.

    Args:
        json_path (str | Path): Path to the spotify_data.json file.

    Returns:
        tuple[list[Playlist], list[Track]]: List of playlists and liked tracks.
    """
    with open(json_path, "r", encoding="utf-8") as f:
        data: dict[str, Any] = json.load(f)
    playlists = [
        Playlist(
            name=p["name"],
            tracks=[Track(**t) for t in p["tracks"]],
        )
        for p in data.get("playlists", [])
    ]
    liked_tracks = [Track(**t) for t in data.get("liked_tracks", [])]
    # Spotify tracks are in decreasing date order, so we reverse it since
    # we want to preserve the order of addition in Tidal
    return playlists, liked_tracks[::-1]


def create_tidal_playlist(session: Session, playlist: Playlist) -> str | None:
    """
    Create a new playlist in Tidal and return its ID.

    Args:
        session (Session): Authenticated Tidal session.
        playlist (Playlist): Playlist to create.

    Returns:
        str | None: The ID of the created playlist, or None if creation failed.
    """
    try:
        tidal_playlist = session.user.create_playlist(  # pyright:ignore
            playlist.name, description="Migrated from Spotify"
        )
        logging.info(
            f"Created Tidal playlist: {playlist.name} (ID: {tidal_playlist.id})"
        )
        return tidal_playlist.id
    except Exception as e:
        logging.error(f"Failed to create playlist '{playlist.name}': {e}")
        return None


def fuzzy_match(a: str, b: str, threshold: float = 0.6) -> bool:
    """
    Return True if the similarity ratio between a and b is above the threshold.
    """
    ratio = SequenceMatcher(None, a.lower(), b.lower(), False).ratio()
    if ratio < threshold:
        logging.warning(f"Fuzzy match failed: '{a}' vs '{b}' (ratio: {ratio})")
    return ratio >= threshold


def search_tidal_track(session: Session, track: Track) -> str | None:
    """
    Search for a track in Tidal by title and artist using fuzzy matching.

    Args:
        session (Session): Authenticated Tidal session.
        track (Track): Track to search for.

    Returns:
        str | None: The ID of the found track, or None if not found.
    """
    try:
        results = session.search(track.to_searchable(), models=[media.Track])
        for result in [results["top_hit"], *results["tracks"]]:
            if not (result.full_name and result.artist and result.artist.name):
                logging.warning(f"Track data incomplete: {result.full_name}")
                continue
            title_match = fuzzy_match(
                clean_title(result.full_name), track.clean_title()
            )
            artist_match = fuzzy_match(result.artist.name, track.artist)
            if title_match and artist_match:
                logging.info(
                    f"Found track '{track.title}' by '{track.artist}' (ID: {result.id})"
                )
                return str(result.id)
        logging.warning(f"Track not found in Tidal: {track.title} by {track.artist}")
        return None
    except Exception as e:
        logging.error(
            f"Error searching for track '{track.title}' by '{track.artist}': {e}"
        )
        return None


def add_tracks_to_tidal_playlist(
    session: Session,
    playlist_id: str,
    tracks: list[Track],
    progress: Progress,
    task: TaskID,
) -> list[Track]:
    """
    Add tracks to a Tidal playlist. Returns a list of missing tracks.

    Args:
        session (Session): Authenticated Tidal session.
        playlist_id (str): Tidal playlist ID.
        tracks (list[Track]): List of tracks to add.

    Returns:
        list[Track]: Tracks not found in Tidal.
    """
    missing_tracks: list[Track] = []
    tidal_playlist = session.playlist(playlist_id).factory()
    if not isinstance(tidal_playlist, UserPlaylist):
        logging.error(
            f"Playlist {playlist_id} is not a UserPlaylist, cannot add tracks."
        )
        return tracks  # All tracks are missing if we can't add
    track_ids: list[str] = []
    for track in tracks:
        track_id = search_tidal_track(session, track)
        if track_id:
            track_ids.append(track_id)
        else:
            missing_tracks.append(track)
        progress.update(task, advance=1, description=f"Adding track: {track.title}")
    if track_ids:
        try:
            tidal_playlist.add(track_ids)
            logging.info(f"Added {len(track_ids)} tracks to playlist {playlist_id}")
        except Exception as e:
            logging.error(f"Failed to add tracks to playlist {playlist_id}: {e}")
    return missing_tracks


def migrate_playlists(
    session: Session, playlists: list[Playlist]
) -> dict[str, list[Track]]:
    """
    Create Tidal playlists and add tracks, returning missing tracks per playlist.
    """
    all_missing_tracks: dict[str, list[Track]] = {}
    for playlist in progress_tracker(playlists):
        print(f"[bold green]Creating Tidal playlist:[/bold green] {playlist.name}")
        playlist_id = create_tidal_playlist(session, playlist)
        if playlist_id:
            with Progress() as progress:
                task = progress.add_task(
                    f"Adding tracks to '{playlist.name}'", total=len(playlist.tracks)
                )
                missing = add_tracks_to_tidal_playlist(
                    session, playlist_id, playlist.tracks, progress, task
                )
            if missing:
                print(
                    f"[bold yellow]Missing tracks in playlist '{playlist.name}':[/bold yellow] {len(missing)}"
                )
                logging.warning(
                    f"Some tracks could not be found in Tidal for playlist '{playlist.name}': {len(missing)}"
                )
                all_missing_tracks[playlist.name] = missing
        else:
            print(f"[bold red]Failed to create playlist: {playlist.name}[/bold red]")
    return all_missing_tracks


def migrate_liked_tracks(session: Session, liked_tracks: list[Track]) -> list[Track]:
    """
    Like tracks in Tidal, returning missing tracks.
    """
    missing_liked: list[Track] = []
    print("[bold green]Liking tracks in Tidal...[/bold green]")
    with Progress() as progress:
        task = progress.add_task("Liking tracks", total=len(liked_tracks))
        for track in liked_tracks:
            track_id = search_tidal_track(session, track)
            if track_id:
                try:
                    session.user.favorites.add_track(track_id)  # pyright:ignore
                    logging.info(f"Liked track '{track.title}' in Tidal.")
                except Exception as e:
                    logging.error(f"Failed to like track '{track.title}': {e}")
            else:
                missing_liked.append(track)
            progress.update(task, advance=1)
    if missing_liked:
        print(f"[bold yellow]Missing liked tracks: {len(missing_liked)}[/bold yellow]")
        logging.warning(
            f"Some liked tracks could not be found in Tidal: {len(missing_liked)}"
        )
    return missing_liked


def print_missing_tracks(all_missing_tracks: dict[str, list[Track]]):
    """
    Print a summary of missing tracks per playlist and liked tracks.
    """
    if all_missing_tracks:
        print("[bold red]Some tracks could not be found in Tidal:[/bold red]")
        for key, tracks in all_missing_tracks.items():
            print(f"[bold magenta]{key}:[/bold magenta]")
            logging.warning(f"Missing tracks in {key}: {len(tracks)}")
            for track in tracks:
                print(f"- {track.title} by {track.artist}")
                logging.warning(f"- {track.title} by {track.artist}")
    else:
        print("[bold green]All tracks migrated successfully![/bold green]")


def main():
    session = load_tidal_session()
    if not session:
        session = authenticate_tidal()
        if not session:
            return
        save_tidal_session(session)
    print("[bold blue]Loading Spotify data...[/bold blue]")
    spotify_data_path = Path("data/spotify_data.json")
    if not spotify_data_path.exists():
        print(f"[bold red]Spotify data file not found: {spotify_data_path}[/bold red]")
        return
    playlists, liked_tracks = load_spotify_data(spotify_data_path)
    print(
        f"[bold blue]Found {len(playlists)} playlists and {len(liked_tracks)} liked tracks in Spotify data.[/bold blue]"
    )
    try:
        print("[bold green]Migrating playlists to Tidal...[/bold green]")
        all_missing_tracks = migrate_playlists(session, playlists)
        print("[bold green]Migrating liked tracks to Tidal...[/bold green]")
        missing_liked_tracks = migrate_liked_tracks(session, liked_tracks)
        if missing_liked_tracks:
            all_missing_tracks["Liked Tracks"] = missing_liked_tracks
        print_missing_tracks(all_missing_tracks)
    except Exception as e:
        logging.error(f"Migration failed: {e}")
        print("[bold red]Migration failed. Check tidal.log for details.[/bold red]")
        return


if __name__ == "__main__":
    main()
