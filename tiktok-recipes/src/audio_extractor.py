import re
from pathlib import Path

import yt_dlp
from rich.console import Console
from rich.panel import Panel

console = Console()


def download_audio_from_tiktok(url: str, output_dir: str = "audio_files") -> Path | None:
    """
    Downloads the audio from a TikTok URL and saves it as an MP3 file.

    Args:
        url (str): The URL of the TikTok video.
        output_dir (str): The directory where the audio file will be saved.

    Returns:
        Path: The file path of the downloaded audio file, or None if download fails.
    """
    console.rule("[bold blue]TikTok Audio Download")
    # Ensure the output directory exists
    Path(output_dir).mkdir(parents=True, exist_ok=True)

    # Get the video Id to use as filename
    video_id = re.search(r"video/([0-9]+)", url)
    if not video_id:
        console.print(f"[bold red]Error:[/bold red] Could not extract video ID from URL: {url}")
        return None
    video_id = video_id.group(1)
    output_template = Path(output_dir) / f"{video_id}.%(ext)s"
    console.print(f"[green]Output template:[/green] {output_template}")
    # yt-dlp options
    # For more info on options: https://github.com/yt-dlp/yt-dlp#embedding-yt-dlp
    ydl_opts = {
        "format": "bestaudio/best",  # Select the best audio-only stream
        "outtmpl": str(output_template),  # Set the output file template
        "postprocessors": [
            {
                "key": "FFmpegExtractAudio",  # Use FFmpeg to extract audio
                "preferredcodec": "mp3",  # Convert the audio to mp3
                "preferredquality": "192",  # Set the audio quality to 192kbps
            }
        ],
        "quiet": False,  # Set to True to suppress console output from yt-dlp
        "no_warnings": True,
    }

    try:
        with yt_dlp.YoutubeDL(ydl_opts) as ydl:
            console.print(f"[yellow]Downloading audio from:[/yellow] {url}")
            ydl.download([url])
            # The exact filename is determined by yt-dlp, we need to find it.
            # After download, the file will be named {unique_filename}.mp3
            final_filepath = Path(output_dir) / f"{video_id}.mp3"
            console.print(f"[green]Expected output:[/green] {final_filepath}")
            if final_filepath.exists():
                console.print(
                    Panel(
                        f"✅ [bold green]Successfully downloaded and converted to:[/bold green]\n{final_filepath}",
                        style="green",
                    )
                )
                return final_filepath
            else:
                console.print("[bold red]Error:[/bold red] Could not find the downloaded file.")
                return None
    except Exception as e:
        console.print(f"[bold red]An error occurred during download:[/bold red] {e}")
        return None


def extract_first_frame(video_path: Path) -> bytes | None:
    """
    Extracts the first frame of a video file as JPEG bytes.

    Args:
        video_path (Path): Path to the video file.

    Returns:
        bytes | None: JPEG bytes of the first frame, or None if extraction fails.
    """
    try:
        import cv2
        import numpy as np

        cap = cv2.VideoCapture(str(video_path))
        ret, frame = cap.read()
        cap.release()
        if not ret:
            console.print("[bold red]Failed to read first frame from video.[/bold red]")
            return None
        is_success, buffer = cv2.imencode(".jpg", frame)
        if not is_success:
            console.print("[bold red]Failed to encode frame as JPEG.[/bold red]")
            return None
        return buffer.tobytes()
    except Exception as e:
        console.print(f"[bold red]Error extracting first frame:[/bold red] {e}")
        return None


def download_audio_and_cover_from_tiktok(
    url: str, output_dir: str = "audio_files"
) -> tuple[Path | None, bytes | None]:
    """
    Downloads the video from a TikTok URL, extracts the first frame as JPEG bytes, then extracts audio as MP3.
    Returns a tuple: (audio_path, cover_bytes)
    """
    console.rule("[bold blue]TikTok Audio & Cover Download")
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    video_id = re.search(r"video/([0-9]+)", url)
    if not video_id:
        console.print(f"[bold red]Error:[/bold red] Could not extract video ID from URL: {url}")
        return None, None
    video_id = video_id.group(1)
    video_path = Path(output_dir) / f"{video_id}.mp4"
    audio_path = Path(output_dir) / f"{video_id}.mp3"
    # Download the video (not just audio)
    ydl_opts = {
        "format": "mp4",
        "outtmpl": str(video_path),
        "quiet": False,
        "no_warnings": True,
    }
    try:
        with yt_dlp.YoutubeDL(ydl_opts) as ydl:
            console.print(f"[yellow]Downloading video for cover extraction from:[/yellow] {url}")
            ydl.download([url])
        if not video_path.exists():
            console.print("[bold red]Error:[/bold red] Could not find the downloaded video file.")
            return None, None
        # Extract first frame
        cover_bytes = extract_first_frame(video_path)
        # Now extract audio from the video file
        ydl_opts_audio = {
            "format": "bestaudio/best",
            "outtmpl": str(audio_path),
            "postprocessors": [
                {
                    "key": "FFmpegExtractAudio",
                    "preferredcodec": "mp3",
                    "preferredquality": "192",
                }
            ],
            "quiet": False,
            "no_warnings": True,
        }
        with yt_dlp.YoutubeDL(ydl_opts_audio) as ydl:
            console.print(f"[yellow]Extracting audio from video:[/yellow] {video_path}")
            ydl.download([str(video_path)])
        if not audio_path.exists():
            console.print("[bold red]Error:[/bold red] Could not find the extracted audio file.")
            return None, cover_bytes
        return audio_path, cover_bytes
    except Exception as e:
        console.print(f"[bold red]An error occurred during download or extraction:[/bold red] {e}")
        return None, None


# --- Main execution block for testing ---
if __name__ == "__main__":
    # You can replace this URL with any TikTok video URL to test the script
    test_tiktok_url = "https://www.tiktok.com/@amysheppardfood/video/7474287630218284310?lang=fr"

    # Call the function
    audio_file_path = download_audio_from_tiktok(test_tiktok_url)

    if audio_file_path:
        console.print(f"\n[bold green]✅ Success! Audio saved at: {audio_file_path}[/bold green]")
    else:
        console.print("\n[bold red]❌ Failure. Could not retrieve audio.[/bold red]")
