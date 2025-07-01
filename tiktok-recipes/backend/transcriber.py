from pathlib import Path

import torch
import whisper
from rich.console import Console
from rich.panel import Panel

console = Console()

try:
    console.rule("[bold blue]Whisper Model Loading")
    console.print("[yellow]Loading Whisper model...[/yellow]")
    model = whisper.load_model("small")
    console.print("[green]Whisper model loaded successfully.[/green]")
except Exception as e:
    console.print(f"[bold red]Error loading model:[/bold red] {e}")
    model = None


def transcribe_audio(audio_path: Path) -> str:
    console.rule("[bold blue]Audio Transcription")
    if not model:
        console.print("[bold red]Model not loaded. Cannot transcribe audio.[/bold red]")
        return ""
    try:
        if torch.cuda.is_available():
            whisper_model = model.to(torch.device("cuda")).float()
        else:
            whisper_model = model
        console.print(f"[cyan]Transcribing audio file:[/cyan] {audio_path}")
        result = whisper_model.transcribe(str(audio_path), fp16=False)
        transcribed_text = result["text"]
        console.print(Panel("[green]Transcription successful.[/green]", style="green"))
        console.print(transcribed_text)
        return str(transcribed_text)
    except Exception as e:
        console.print(f"[bold red]An error occurred during transcription:[/bold red] {e}")
        return ""
