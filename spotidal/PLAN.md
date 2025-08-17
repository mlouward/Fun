# Spotify-to-Tidal music migration tool

This tool is meant to let users migrate their playlists and liked tracks from Spotify to Tidal. It uses the Spotify and Tidal APIs to extract playlists and tracks from Spotify and create them in Tidal.

## Structure

This tool is composed of 2 scripts:

- spotify.py: Handles the extraction of playlists and tracks from Spotify.
- tidal.py: Manages the creation of playlists and tracks in Tidal.

### Spotify.py

This script uses the Spotify API to authenticate the user, retrieve their playlists and liked tracks, and format them for transfer. It handles pagination to ensure all data is retrieved.
Then, it saves the data in a JSON file for later use.

### Tidal.py

This script uses the Tidal API to authenticate the user and create playlists and tracks based on the data retrieved from Spotify. It reads the JSON file created by Spotify.py and recreates the playlists and tracks in Tidal.

## Usage

1. Run `spotify.py` to start exporting your tracks. It will prompt you to authenticate with Spotify and retrieve your playlists and liked tracks. This will create a `spotify_data.json` file. Ths JSON structure is as follows:

```json
{
  "playlists": [
    {
      "name": "Playlist Name",
      "tracks": [
        {
          "title": "Track Title",
          "artist": "Artist Name"
        }
      ]
    }
  ],
  "liked_tracks": [
    {
      "title": "Track Title",
      "artist": "Artist Name"
    }
  ]
}
```

## Specifics

### Spotify API

This script uses the [spotipy](https://spotipy.readthedocs.io/en/2.25.1/) library to interact with the Spotify API. It will authenticate the user using OAuth2 and retrieve their playlists and liked tracks.

1. The script will authenticate the user using OAuth2 and retrieve their playlists and liked tracks.
2. For each playlist, it will retrieve the name and all tracks in the playlist.
3. For each track, it will retrieve the title & artist.
4. The script will handle pagination to ensure all playlists and tracks are retrieved.
5. The script will save the data in a JSON file named `spotify_data.json`.
6. The script will handle rate limits by checking for a 429 status code and waiting for the specified time before retrying the request. If no "Retry-After" header is present, it will wait 30 seconds before retrying.
7. The script will log all actions and errors to a log file for debugging purposes.

### Tidal API

This script uses the [tidalapi](https://tidalapi.netlify.app) library to interact with the Tidal API. It will read the `spotify_data.json` file created by `spotify.py` and recreate the playlists and tracks in Tidal.

1. For each playlist, the script will create a new playlist in Tidal with the same name.
2. For each track in the playlist, it will search for the track in Tidal using the title and artist.
3. If the track is found, it will be added to the newly created playlist. Else, it will log an error message indicating that the track was not found in Tidal.
4. For liked tracks, it will also like the track in Tidal if it exists.
5. The script will handle pagination for both playlists and tracks to ensure all data is processed.
6. The script will return a list of the missing tracks that were not found in Tidal, allowing the user to review them per playlist & liked tracks (separately).
7. Tidal API rate limit: If we encounter a 429 code, try to read the "Retry-After" header and wait for the specified time before retrying the request. Else, wait 30 secs
