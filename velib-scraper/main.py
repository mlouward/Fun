import logging
from datetime import datetime, time
from pathlib import Path

import polars as pl
import requests

# Configure logging
log_dir = Path("logs")
log_dir.mkdir(exist_ok=True)
logging_file = log_dir / "velib_errors.log"

logging.basicConfig(
    filename=logging_file,
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
)

# Constants
API_URL = "https://opendata.paris.fr/api/records/1.0/search/"
DATASET = "velib-disponibilite-en-temps-reel"
PARQUET_DIR = Path("data")
PARQUET_DIR.mkdir(exist_ok=True)

# Target stations
TARGET_STATIONS = [
    "Pyrénées - Ménilmontant",
    "Villiers de l'Isle Adam - Pyrénées",
    "Mairie du 20ème",
    "Gambetta - Père Lachaise",
]


def is_time_in_range() -> tuple[bool, time]:
    """Check if current time is between 7am and 10am"""
    now = datetime.now().time()
    start_time = time(7, 0)
    end_time = time(10, 0)
    return start_time <= now <= end_time, now


def get_station_data(station_name: str) -> dict | None:
    """Fetch data for a specific station from Velib API"""
    params = {
        "dataset": DATASET,
        "q": station_name,
        "rows": 1,  # We only need one result per station
    }

    try:
        response = requests.get(API_URL, params=params)
        response.raise_for_status()
        data = response.json()

        if data.get("nhits", 0) > 0 and data.get("records"):
            return data["records"][0]
        else:
            logging.warning(f"No data found for station: {station_name}")
            return None
    except Exception as e:
        logging.error(f"Error fetching data for {station_name}: {str(e)}")
        return None


def process_station_data(station_name: str) -> None:
    """Process data for a specific station and save to parquet"""
    timestamp = datetime.now()
    parquet_file = (
        PARQUET_DIR
        / f"{station_name.replace(' - ', '_').replace(' ', '_').replace("'", '').lower()}.parquet"
    )

    # Create a record with timestamp even if the API call fails
    record = {
        "timestamp": timestamp,
        "date": timestamp.date(),
        "time": timestamp.time().strftime("%H:%M:%S"),
        "station_name": station_name,
    }

    # Fetch data from API
    api_data = get_station_data(station_name)

    if api_data:
        fields = api_data.get("fields", {})
        record.update(
            {
                "station_id": fields.get("stationcode"),
                "station_name_api": fields.get("name"),
                "mechanical_bikes": fields.get("mechanical", 0),
                "ebikes": fields.get("ebike", 0),
                "capacity": fields.get("capacity", 0),
                "docks_available": fields.get("numdocksavailable", 0),
                "bikes_available": fields.get("numbikesavailable", 0),
                "is_installed": fields.get("is_installed", False),
                "is_renting": fields.get("is_renting", False),
                "is_returning": fields.get("is_returning", False),
                "last_reported": fields.get("last_reported"),
                "lat": fields.get("coordonnees_geo", [0, 0])[0]
                if fields.get("coordonnees_geo")
                else None,
                "lon": fields.get("coordonnees_geo", [0, 0])[1]
                if fields.get("coordonnees_geo")
                else None,
            }
        )

    # Convert to Polars DataFrame
    df_new = pl.DataFrame([record])

    # Read existing data if file exists
    if parquet_file.exists():
        try:
            df_existing = pl.read_parquet(parquet_file)
            df = pl.concat([df_existing, df_new])
        except Exception as e:
            logging.error(
                f"Error reading existing parquet file for {station_name}: {str(e)}"
            )
            df = df_new
    else:
        df = df_new

    # Save to parquet
    try:
        df.write_parquet(parquet_file)
        print(f"Data saved for {station_name} at {timestamp}")
    except Exception as e:
        logging.error(f"Error saving parquet file for {station_name}: {str(e)}")


def main():
    """Main function to process all stations"""
    # Only run between 7am and 10am
    in_range, now = is_time_in_range()
    if not in_range:
        print(
            f"Current time {now.strftime('%H:%M:%S')} is outside the 7am-10am timeframe. Exiting."
        )
        return

    print(f"Starting Velib data collection at {now}")

    for station in TARGET_STATIONS:
        process_station_data(station)

    print(f"Finished Velib data collection at {datetime.now()}")


if __name__ == "__main__":
    main()
