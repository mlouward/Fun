import argparse
import logging
import os
import re
import sys
import time
from pathlib import Path

import undetected_chromedriver as uc
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait

parser = argparse.ArgumentParser(description="Download drama episodes from KissAsian")
parser.add_argument("drama_name", type=str, help="Name of the drama")
DRAMA_NAME = parser.parse_args().drama_name
OUTPUT_PATH = Rf"D:\Videos\{DRAMA_NAME}"
LOG_FILE = OUTPUT_PATH + Rf"\{DRAMA_NAME}.log"


def ensure_output_path_exists():
    if not os.path.exists(OUTPUT_PATH):
        os.makedirs(OUTPUT_PATH, exist_ok=True)


def setup_logging():
    # Set up logging
    logging.basicConfig(
        filename=LOG_FILE,
        filemode="a",
        format="[%(asctime)s] %(levelname)s: %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.INFO,
    )

    # Log to console
    console = logging.StreamHandler(sys.stdout)
    console.setLevel(logging.INFO)
    formatter = logging.Formatter("[%(asctime)s] %(levelname)s: %(message)s")
    console.setFormatter(formatter)
    logging.getLogger("").addHandler(console)

    logging.info("Starting script")
    logging.info(f"Output path: {OUTPUT_PATH}")
    logging.info(f"Log file: {LOG_FILE}")
    logging.info(f"Drama name: {DRAMA_NAME}")


def get_driver():
    # Add the path to the uBlock Origin extension to the ChromeOptions object
    chrome_options = uc.ChromeOptions()
    chrome_options.add_argument(
        f"--load-extension={Path('C:/Users/Maxime/Documents/kdrama-video-downloader/ublock')}"
    )

    chrome_options.add_experimental_option(
        "prefs",
        {
            "download.default_directory": OUTPUT_PATH,
            "download.prompt_for_download": False,
            "download.directory_upgrade": True,
            "safebrowsing.enabled": True,
        },
    )
    # Open the website
    driver = uc.Chrome(options=chrome_options)
    logging.info("Driver started")
    return driver


def login(driver: uc.Chrome):
    # Navigate to the login page and fill in the form
    logging.info("Logging in")
    driver.get("https://kissasian.li/Login")
    time.sleep(6)
    username_field = driver.find_element(by=By.NAME, value="username")
    username_field.send_keys("aehoard")
    password_field = driver.find_element(by=By.NAME, value="password")
    password_field.send_keys("JGPGc2%mJnHRW%WG$kY")
    login_button = driver.find_element(by=By.ID, value="btnSubmit")
    login_button.click()
    logging.info("Logged in")
    time.sleep(3)


def download_started(expected_length: int):
    """
    Checks OUTPUT_PATH for the expected number of files.
    Only checks files ending with .crdownload or .mp4
    """
    logging.info("Checking if download started...")
    length = len(
        [x for x in os.listdir(OUTPUT_PATH) if x.endswith((".crdownload", ".mp4"))]
    )
    logging.info(f"Expected length: {expected_length}, actual length: {length}")
    return length == expected_length


def get_episodes_links(driver: uc.Chrome) -> list[WebElement]:
    # Wait for the page to load and for the link to be clickable
    logging.info("Getting episodes links")
    driver.get(f"https://kissasian.li/Drama/{re.sub(r'[^a-zA-Z0-9]', '-', DRAMA_NAME)}")
    WebDriverWait(driver, 20).until(
        EC.element_to_be_clickable(
            (
                By.CSS_SELECTOR,
                "#leftside > div:nth-child(3) > div.barContent.episodeList > div:nth-child(2) > table > tbody > tr:nth-child(3)",
            )
        )
    )

    # find all links (all episodes) in the table with class="listing"
    episodes_links = driver.find_elements(By.CSS_SELECTOR, ".listing a")
    logging.info(f"Found {len(episodes_links)} episodes")
    return episodes_links


def rename_files() -> None:
    logging.info("Renaming files...")
    files = sorted(Path(OUTPUT_PATH).glob("*.mp4"), key=lambda x: x.stat().st_ctime)
    for idx, file in enumerate(files, start=1):
        file.rename(file.parent / f"{DRAMA_NAME} Episode {idx:02}.mp4")
    logging.info(f"Renamed {len(files)} files")


def episode_already_downloaded(idx) -> bool:
    return Path(OUTPUT_PATH).joinpath(f"{DRAMA_NAME} Episode {idx}.mp4").exists()


def download_all_episodes(driver: uc.Chrome, episodes_links: list[WebElement]) -> bool:
    logging.info("Starting download...")
    for idx, link in enumerate(episodes_links, start=1):
        if episode_already_downloaded(idx):
            logging.info(f"Found episode {idx} already downloaded, skipping...")
            continue
        # Sleep for 10 sec if nb of files ending in .crdownload is >= 2:
        while (
            len([x for x in os.listdir(OUTPUT_PATH) if x.endswith(".crdownload")]) >= 2
        ):
            logging.info("Waiting for download to finish...")
            time.sleep(10)
        # open link
        actions = ActionChains(driver)
        actions.key_down(Keys.CONTROL).click(link).key_up(Keys.CONTROL).perform()
        driver.switch_to.window(driver.window_handles[-1])
        logging.info(f"Opened episode {idx}")
        # wait for the page to load
        try:
            WebDriverWait(driver, 20).until(
                EC.element_to_be_clickable((By.CSS_SELECTOR, "#divDownload > a"))
            )
        except TimeoutException:
            logging.error(f"Episode {idx} not available, aborting...")
            return False
        # find download button
        logging.info("Finding download button")
        download_button = driver.find_element(By.CSS_SELECTOR, "#divDownload > a")
        download_button.click()
        logging.info("Clicked download button")
        # Switch to the new tab that has been opened
        driver.switch_to.window(driver.window_handles[-1])
        # Wait for the button with id "todl" to be clickable and click it
        logging.info("Waiting for todl button to be clickable")
        todl_button = WebDriverWait(driver, 20).until(
            EC.element_to_be_clickable((By.ID, "todl"))
        )
        todl_button.click()
        logging.info("Clicked todl button")
        # Wait for the button with id "downloadbtn" to be clickable and click it
        WebDriverWait(driver, 20).until(
            EC.element_to_be_clickable((By.ID, "downloadbtn"))
        )

        download_button = driver.find_element(By.ID, "downloadbtn")
        download_button.click()
        logging.info("Clicked download button")
        logging.info("Waiting for download to start...")
        time.sleep(3)
        while not download_started(idx):
            # go back to the previous page
            logging.info("Download didn't start, going back...")
            driver.back()
            time.sleep(0.7)
            download_button = driver.find_element(By.ID, "downloadbtn")
            download_button.click()  # click the download button again
            logging.info("Clicked download button")
            logging.info("Waiting for download to start...")
            time.sleep(7)  # wait for the download to start

        # Back to main window
        logging.info("Back to main window")
        driver.switch_to.window(driver.window_handles[0])

    return True


def main() -> None:
    ensure_output_path_exists()

    setup_logging()

    driver = get_driver()

    login(driver)

    episodes_links = get_episodes_links(driver)
    episodes_links.reverse()

    dl_completed = download_all_episodes(driver, episodes_links)

    logging.info("Waiting for downloads to finish...")
    # wait while there are files in output_path ending in .crdownload
    while any(file.suffix == ".crdownload" for file in Path(OUTPUT_PATH).iterdir()):
        time.sleep(10)

    if dl_completed:
        logging.info("All downloads finished!")
    else:
        logging.error("Some downloads failed. Check the logs for more info.")

    # Rename all files downloaded once over
    # Sort them by creation date first
    rename_files()


if __name__ == "__main__":
    main()
