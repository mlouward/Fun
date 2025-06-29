from pathlib import Path

import torch
import whisper

try:
    print("Loading Whisper model...")
    model = whisper.load_model("small")
except Exception as e:
    print(f"Error loading model: {e}")
    model = None


def transcribe_audio(audio_path: Path) -> str:
    """
    Transcribes an audio file using OpenAI's Whisper model.

    Args:
        audio_path (Path): The path to the audio file.

    Returns:
        str: The transcribed text.
    """
    if not model:
        print("Model not loaded. Cannot transcribe audio.")
        return ""
    try:
        if torch.cuda.is_available():
            whisper = model.to(torch.device("cuda")).float()
        print(f"Transcribing audio file: {audio_path}")
        # Force transcription in float32 to avoid NaN errors on some GPUs
        result = whisper.transcribe(str(audio_path), fp16=False)
        transcribed_text = result["text"]
        print("Transcription successful.")
        print(transcribed_text)
        return transcribed_text  # type: ignore
    except Exception as e:
        print(f"An error occurred during transcription: {e}")
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
        print("\n--- Transcription ---")
        print(transcription)
