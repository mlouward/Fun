import json
import logging
import os
from pathlib import Path
from typing import Any

from dotenv import load_dotenv
from rich import print
from rich.progress import Progress
from tidalapi import Session, artist, media

from models import Playlist, Track

load_dotenv()


# Tidal API configuration
AUTHORIZATION_ENDPOINT = "https://login.tidal.com/authorize"
TOKEN_ENDPOINT = "https://auth.tidal.com/v1/oauth2/token"
CLIENT_ID = os.getenv("TIDAL_CLIENT_ID")
CLIENT_SECRET = os.getenv("TIDAL_CLIENT_SECRET")

logging.basicConfig(
    filename=Path("logs/tidal_auth.log"),
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
            "[bold red]Authentication failed. Check tidal_auth.log for details.[/bold red]"
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
    return [p.reverse() for p in playlists], liked_tracks[::-1]


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


def search_tidal_track(session: Session, track: Track) -> str | None:
    """
    Search for a track in Tidal by title and artist.

    Args:
        session (Session): Authenticated Tidal session.
        track (Track): Track to search for.

    Returns:
        str | None: The ID of the found track, or None if not found.
    """
    try:
        results = session.search(track.title, models=[media.Track, artist.Artist])
        for result in results["tracks"]:
            assert result.full_name and result.artist and result.artist.name, (
                "Track data incomplete"
            )
            if result.full_name.lower() == track.title.lower() and (
                result.artist.name.lower() == track.artist.lower()
            ):
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
    session: Session, playlist_id: str, tracks: list[Track]
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
    for track in tracks:
        track_id = search_tidal_track(session, track)
        if track_id:
            try:
                session.user.add_playlist_tracks(playlist_id, [track_id])  # pyright:ignore
                logging.info(f"Added track '{track.title}' to playlist {playlist_id}")
            except Exception as e:
                logging.error(
                    f"Failed to add track '{track.title}' to playlist {playlist_id}: {e}"
                )
        else:
            missing_tracks.append(track)
    return missing_tracks


def like_tidal_tracks(session: Session, tracks: list[Track]) -> list[Track]:
    """
    Like tracks in Tidal. Returns a list of missing tracks.

    Args:
        session (Session): Authenticated Tidal session.
        tracks (list[Track]): List of tracks to like.

    Returns:
        list[Track]: Tracks not found in Tidal.
    """
    missing_tracks: list[Track] = []
    for track in tracks:
        track_id = search_tidal_track(session, track)
        if track_id:
            try:
                session.user.favorite_track(track_id)  # pyright:ignore
                logging.info(f"Liked track '{track.title}' in Tidal.")
            except Exception as e:
                logging.error(f"Failed to like track '{track.title}': {e}")
        else:
            missing_tracks.append(track)
    return missing_tracks


def migrate_playlists(
    session: Session, playlists: list[Playlist]
) -> dict[str, list[Track]]:
    """
    Create Tidal playlists and add tracks, returning missing tracks per playlist.
    """
    all_missing_tracks: dict[str, list[Track]] = {}
    for playlist in playlists:
        print(f"[bold green]Creating Tidal playlist:[/bold green] {playlist.name}")
        playlist_id = create_tidal_playlist(session, playlist)
        if playlist_id:
            missing: list[Track] = []
            with Progress() as progress:
                task = progress.add_task(
                    f"Adding tracks to '{playlist.name}'", total=len(playlist.tracks)
                )
                for track in playlist.tracks:
                    track_id = search_tidal_track(session, track)
                    if track_id:
                        try:
                            session.user.add_playlist_tracks(playlist_id, [track_id])  # pyright:ignore
                            logging.info(
                                f"Added track '{track.title}' to playlist {playlist_id}"
                            )
                        except Exception as e:
                            logging.error(
                                f"Failed to add track '{track.title}' to playlist {playlist_id}: {e}"
                            )
                    else:
                        missing.append(track)
                    progress.update(task, advance=1)
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
                    session.user.favorite_track(track_id)  # pyright:ignore
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
    session = authenticate_tidal()
    if not session:
        print("[bold red]Failed to authenticate with Tidal.[/bold red]")
        return
    print("[bold blue]Loading Spotify data...[/bold blue]")
    # Path relative to the project root
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
        all_missing_tracks = migrate_playlists(session, playlists[:1])
        # missing_liked = migrate_liked_tracks(session, liked_tracks)
        # if missing_liked:
        #     all_missing_tracks["liked_tracks"] = missing_liked
        print_missing_tracks(all_missing_tracks)
    except Exception as e:
        logging.error(f"Migration failed: {e}")
        print(
            "[bold red]Migration failed. Check tidal_auth.log for details.[/bold red]"
        )
        return


if __name__ == "__main__":
    main()
