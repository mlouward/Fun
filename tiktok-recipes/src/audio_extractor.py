import re
from pathlib import Path

import yt_dlp


def download_audio_from_tiktok(url: str, output_dir: str = "audio_files") -> str | None:
	"""
	Downloads the audio from a TikTok URL and saves it as an MP3 file.

	Args:
	    url (str): The URL of the TikTok video.
	    output_dir (str): The directory where the audio file will be saved.

	Returns:
	    str: The file path of the downloaded audio file, or None if download fails.
	"""
	# Ensure the output directory exists
	Path(output_dir).mkdir(parents=True, exist_ok=True)

	# Get the video Id to use as filename
	video_id = re.search(r"video/([0-9]+)", url)
	if not video_id:
		print(f"Error: Could not extract video ID from URL: {url}")
		return None
	video_id = video_id.group(1)
	output_template = Path(output_dir) / f"{video_id}.%(ext)s"

	# yt-dlp options
	# For more info on options: https://github.com/yt-dlp/yt-dlp#embedding-yt-dlp
	ydl_opts = {
		"format": "bestaudio/best",  # Select the best audio-only stream
		"outtmpl": output_template,  # Set the output file template
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
			print(f"Downloading audio from: {url}")
			ydl.download([url])
			# The exact filename is determined by yt-dlp, we need to find it.
			# After download, the file will be named {unique_filename}.mp3
			final_filepath = Path(output_dir) / f"{video_id}.mp3"
			if final_filepath.exists():
				print(f"Successfully downloaded and converted to: {final_filepath}")
				return final_filepath
			else:
				print("Error: Could not find the downloaded file.")
				return None
	except Exception as e:
		print(f"An error occurred during download: {e}")
		return None


# --- Main execution block for testing ---
if __name__ == "__main__":
	# You can replace this URL with any TikTok video URL to test the script
	test_tiktok_url = "https://www.tiktok.com/@amysheppardfood/video/7474287630218284310?lang=fr"

	# Call the function
	audio_file_path = download_audio_from_tiktok(test_tiktok_url)

	if audio_file_path:
		print(f"\n✅ Success! Audio saved at: {audio_file_path}")
	else:
		print("\n❌ Failure. Could not retrieve audio.")
