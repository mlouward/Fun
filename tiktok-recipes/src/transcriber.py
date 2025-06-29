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
    """
    Transcribes an audio file using OpenAI's Whisper model.

    Args:
        audio_path (Path): The path to the audio file.

    Returns:
        str: The transcribed text.
    """
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
        # Force transcription in float32 to avoid NaN errors on some GPUs
        result = whisper_model.transcribe(str(audio_path), fp16=False)
        transcribed_text = result["text"]
        console.print(Panel("[green]Transcription successful.[/green]", style="green"))
        # Optionally print the transcription
        console.print(transcribed_text)
        return transcribed_text  # type: ignore
    except Exception as e:
        console.print(f"[bold red]An error occurred during transcription:[/bold red] {e}")
        return ""


# --- Main execution block for testing ---
if __name__ == "__main__":
    from pathlib import Path

    # Example usage:
    # Construct path relative to this script's location to ensure it's always found
    script_dir = Path(__file__).parent
    test_audio_file = script_dir.parent / "audio_files" / "7474287630218284310.mp3"
    transcription = transcribe_audio(test_audio_file)
    if transcription:
        console.print("\n[bold green]--- Transcription ---[/bold green]")
        console.print(transcription)
