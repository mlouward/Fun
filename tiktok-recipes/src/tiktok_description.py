import asyncio
import os
from typing import Optional

from TikTokApi import TikTokApi


async def get_tiktok_video_description(url: str, ms_token: Optional[str] = None) -> Optional[str]:
    """
    Fetches the description for a TikTok video using TikTokApi.

    Args:
        url (str): The TikTok video URL.
        ms_token (Optional[str]): TikTok ms_token for authentication/session (optional but recommended).

    Returns:
        Optional[str]: The video description, or None if fetching fails.
    """
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
            return video_info.get("desc")
    except Exception as e:
        print(f"Failed to fetch TikTok description: {e}")
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
    print(f"Description: {desc}")
