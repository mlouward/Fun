from pathlib import Path

import matplotlib.pyplot as plt  # Still needed as seaborn works on top of matplotlib
import pandas as pd
import polars as pl
import seaborn as sns

# Set seaborn style
sns.set_theme(style="whitegrid")
sns.set_context("notebook", font_scale=1.1)

# Constants
PARQUET_DIR = Path("data")
OUTPUT_DIR = Path("plots")
OUTPUT_DIR.mkdir(exist_ok=True)

# Target stations - same as in main.py to ensure consistency
TARGET_STATIONS = [
    "Pyrénées - Ménilmontant",
    "Villiers de l'Isle Adam - Pyrénées",
    "Mairie du 20ème",
    "Gambetta - Père Lachaise",
]

# Mapping station names to their parquet file names
STATION_TO_FILENAME = {
    station: station.replace(" - ", "_").replace(" ", "_").replace("'", "").lower()
    + ".parquet"
    for station in TARGET_STATIONS
}


def plot_station_availability(station_name: str) -> None:
    """Create a line plot of bike availability by day for a specific station."""
    filename = STATION_TO_FILENAME[station_name]
    parquet_path = PARQUET_DIR / filename

    if not parquet_path.exists():
        print(f"No data file found for station: {station_name}")
        return

    # Read the parquet file
    df = pl.read_parquet(parquet_path)

    # Convert timestamp to datetime if it's stored as string
    if df["timestamp"].dtype == pl.Utf8:
        df = df.with_column(pl.col("timestamp").str.to_datetime())

    # Convert the columns "date" and "time" to datetime
    df = df.with_columns(
        pl.col("date").cast(pl.Date),
        pl.col("time").str.to_datetime("%H:%M:%S").dt.time(),
    )

    # Filter data to only include the relevant morning window (7am-10am)
    df = df.filter(
        (pl.col("time").dt.hour() >= 7) & (pl.col("time").dt.hour() < 10)
    )

    # Convert to pandas for plotting
    pdf = df.to_pandas()

    # Convert time to minutes since midnight for plotting
    pdf["minutes_since_midnight"] = pdf["time"].apply(
        lambda t: t.hour * 60 + t.minute
    )

    # Create the plot
    plt.figure(figsize=(12, 6))

    # Get unique dates and assign colors using the viridis colormap
    unique_dates = pdf["date"].unique()
    colors = sns.color_palette("viridis", n_colors=len(unique_dates))
    date_to_color = dict(zip(unique_dates, colors))

    # Plot each date separately to ensure proper connections only between actual data points
    for date in unique_dates:
        # Filter data for this date
        date_data = pdf[pdf["date"] == date].sort_values("minutes_since_midnight")

        # Format date for legend
        date_str = pd.to_datetime(date).strftime("%Y-%m-%d")

        # Plot with lines and markers
        plt.plot(
            date_data["minutes_since_midnight"],
            date_data["bikes_available"],
            "-o",
            label=date_str,
            color=date_to_color[date],
            markersize=6,
            linewidth=1.5,
        )

    # Set x-axis limits to show from 7:00 to 10:00 (420 to 600 minutes)
    plt.xlim(410, 610)  # Slightly wider than 7:00-10:00 for better visualization

    # Set x-ticks at 30-minute intervals from 7:00 to 10:00
    tick_positions = range(420, 601, 30)  # 7:00 to 10:00 in 30-min steps
    tick_labels = [
        f"{h:02d}:{m:02d}" for h in range(7, 11) for m in [0, 30] if h < 10 or m == 0
    ]

    # Apply the ticks
    plt.xticks(tick_positions, tick_labels, rotation=45)

    # Add a grid to make it easier to see the time intervals
    plt.grid(True, axis="x", linestyle="--", alpha=0.7)

    # Add labels and title with seaborn styling
    plt.title(f"Bike Availability at {station_name}", fontsize=14)
    plt.xlabel("Time of Day", fontsize=12)
    plt.ylabel("Number of Bikes Available", fontsize=12)

    # Configure legend - more compact for many days
    if len(pdf["date"].unique()) > 10:
        plt.legend(
            title="Date",
            fontsize="small",
            ncol=2,
            title_fontsize=10,
            loc="upper center",
            bbox_to_anchor=(0.5, -0.15),
        )
    else:
        plt.legend(title="Date", title_fontsize=10, loc="best")

    # Save the plot
    output_filename = (
        OUTPUT_DIR / f"{filename.replace('.parquet', '')}_availability.png"
    )
    plt.tight_layout()
    plt.savefig(output_filename, dpi=120)
    print(f"Plot saved to {output_filename}")
    plt.close()


def main():
    """Create visualizations for all stations."""
    print("Starting Velib data visualization...")

    for station in TARGET_STATIONS:
        print(f"Processing station: {station}")
        plot_station_availability(station)

    print("Visualization complete!")


if __name__ == "__main__":
    main()
