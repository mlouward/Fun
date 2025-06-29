import asyncio
import os
from typing import Optional

from rich.console import Console
from TikTokApi import TikTokApi

console = Console()


async def get_tiktok_video_description(url: str, ms_token: Optional[str] = None) -> Optional[str]:
    """
    Fetches the description for a TikTok video using TikTokApi.

    Args:
        url (str): The TikTok video URL.
        ms_token (Optional[str]): TikTok ms_token for authentication/session (optional but recommended).

    Returns:
        Optional[str]: The video description, or None if fetching fails.
    """
    console.rule("[bold blue]TikTok Description Fetch")
    ms_token = ms_token or os.environ.get("ms_token")
    try:
        async with TikTokApi() as api:
            await api.create_sessions(
                ms_tokens=[ms_token] if ms_token else None,  # type: ignore
                num_sessions=1,
                sleep_after=3,
                browser=os.getenv("TIKTOK_BROWSER", "chromium"),
            )
            video = api.video(url=url)
            video_info = await video.info()
            desc = video_info.get("desc")
            if desc:
                console.print(f"[green]Fetched TikTok description:[/green] {desc}")
            else:
                console.print("[yellow]No description found for this video.[/yellow]")
            return desc
    except Exception as e:
        console.print(f"[bold red]Failed to fetch TikTok description:[/bold red] {e}")
        return None


# For standalone testing
if __name__ == "__main__":
    import sys

    url = (
        sys.argv[1]
        if len(sys.argv) > 1
        else "https://www.tiktok.com/@amysheppardfood/video/7474287630218284310?lang=fr"
    )
    desc = asyncio.run(get_tiktok_video_description(url))
    console.print(f"Description: {desc}")
