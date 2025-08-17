# Spotidal

A tool to export Spotify playlists and import them into Tidal.

## Overview

This tool is composed of two scripts:

- `spotify.py`: Handles the extraction of playlists and liked tracks from Spotify.
- `tidal.py`: Manages the creation of playlists and liked tracks in Tidal.

## Usage

Create a `.env` file in the root directory with the following content:

```plaintext
TIDAL_CLIENT_ID=<your_tidal_client_id>
TIDAL_CLIENT_SECRET=<your_tidal_client_secret>

SPOTIFY_CLIENT_ID=<your_spotify_client_id>
SPOTIFY_CLIENT_SECRET=<your_spotify_client_secret>
```

Install dependencies using uv

```bash
uv sync
```

Run the scripts in the following order:

```bash
python src/spotify.py
python src/tidal.py
```

Alternatively, you can use `uv run` to run both scripts sequentially:

```bash
uv run src/spotify.py
uv run src/tidal.py
```

This will export your Spotify playlists and liked tracks to a JSON file and then import them into Tidal.

## JSON Structure

The exported JSON file will have the following structure:

```json
{
  "playlists": [
    {
      "name": "Playlist 1",
      "tracks": [
        {
          "title": "Track 1",
          "artist": "Artist 1"
        },
        {
          "title": "Track 2",
          "artist": "Artist 2"
        }
      ]
    }
  ],
  "liked_tracks": [
    {
      "title": "Liked Track 1",
      "artist": "Liked Artist 1"
    }
  ]
}
```
