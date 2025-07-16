import re
import subprocess
from pathlib import Path

import yt_dlp
from rich.console import Console
from rich.panel import Panel

console = Console()


def download_audio_from_tiktok(url: str, output_dir: str = "audio_files") -> Path | None:
    console.rule("[bold blue]TikTok Audio Download")
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    video_id = re.search(r"video/([0-9]+)", url)
    if not video_id:
        console.print(f"[bold red]Error:[/bold red] Could not extract video ID from URL: {url}")
        return None
    video_id = video_id.group(1)
    output_template = Path(output_dir) / f"{video_id}.%(ext)s"
    console.print(f"[green]Output template:[/green] {output_template}")
    ydl_opts = {
        "format": "bestaudio/best",
        "outtmpl": str(output_template),
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
    try:
        with yt_dlp.YoutubeDL(ydl_opts) as ydl:
            console.print(f"[yellow]Downloading audio from:[/yellow] {url}")
            ydl.download([url])
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
    try:
        import cv2

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


def extract_n_frames(video_path: Path, n: int = 5) -> list[bytes]:
    frames = []
    try:
        import cv2

        cap = cv2.VideoCapture(str(video_path))
        total_frames = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
        if total_frames < n:
            n = total_frames
        frame_indices = [int(i * total_frames / n) for i in range(n)]
        for idx in frame_indices:
            cap.set(cv2.CAP_PROP_POS_FRAMES, idx)
            ret, frame = cap.read()
            if not ret:
                continue
            is_success, buffer = cv2.imencode(".jpg", frame)
            if is_success:
                frames.append(buffer.tobytes())
        cap.release()
    except Exception as e:
        console.print(f"[bold red]Error extracting frames:[/bold red] {e}")
    return frames


def download_audio_and_covers_from_tiktok(
    url: str, output_dir: str = "audio_files"
) -> tuple[Path | None, list[bytes] | None]:
    console.rule("[bold blue]TikTok Audio & Cover Download")
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    video_id = re.search(r"video/([0-9]+)", url)
    if not video_id:
        console.print(f"[bold red]Error:[/bold red] Could not extract video ID from URL: {url}")
        return None, None
    video_id = video_id.group(1)
    video_path = Path(output_dir) / f"{video_id}.mp4"
    audio_path = Path(output_dir) / f"{video_id}.mp3"
    video_ydl_opts = {
        "format": "mp4",
        "outtmpl": str(video_path),
        "quiet": False,
        "no_warnings": True,
    }
    try:
        with yt_dlp.YoutubeDL(video_ydl_opts) as ydl:
            console.print(f"[yellow]Downloading video for cover extraction from:[/yellow] {url}")
            ydl.download([url])
        if not video_path.exists():
            console.print("[bold red]Error:[/bold red] Could not find the downloaded video file.")
            return None, None
        console.print(f"[yellow]Extracting 5 frames from video:[/yellow] {video_path}")
        cover_bytes_list = extract_n_frames(video_path, n=5)
        if not cover_bytes_list:
            console.print("[bold red]Error:[/bold red] Could not extract frames as JPEG.")
            return None, None

        ffmpeg_cmd = [
            "ffmpeg",
            "-y",
            "-i",
            str(video_path),
            "-vn",
            "-acodec",
            "mp3",
            str(audio_path),
        ]
        console.print(
            f"[yellow]Extracting audio from local video file with ffmpeg:[/yellow] {video_path}"
        )
        result = subprocess.run(ffmpeg_cmd, capture_output=True)
        if result.returncode != 0:
            console.print(
                f"[bold red]ffmpeg error:[/bold red] {result.stderr.decode(errors='ignore')}"
            )
            return None, cover_bytes_list
        if not audio_path.exists():
            console.print("[bold red]Error:[/bold red] Could not find the extracted audio file.")
            return None, cover_bytes_list
        return audio_path, cover_bytes_list
    except Exception as e:
        console.print(f"[bold red]An error occurred during download or extraction:[/bold red] {e}")
        return None, None
