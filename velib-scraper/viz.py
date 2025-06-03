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

    # Assign two colors from the Viridis palette for weekdays and weekends
    viridis_colors = sns.color_palette("viridis", 2)

    # Add a column for weekday/weekend
    pdf["day_type"] = pdf["date"].apply(lambda d: "Weekday" if pd.to_datetime(d).weekday() < 5 else "Weekend")

    # Group by day_type and minutes_since_midnight, then average
    grouped = pdf.groupby(["day_type", "minutes_since_midnight"])["bikes_available"].mean().reset_index()

    # Plot average availability for weekdays and weekends
    for i, day_type in enumerate(["Weekday", "Weekend"]):
        day_data = grouped[grouped["day_type"] == day_type].copy()
        # Plot the original average (with markers)
        plt.plot(
            day_data["minutes_since_midnight"],
            day_data["bikes_available"],
            "-o",
            label=f"{day_type} Avg",
            color=viridis_colors[i],
            markersize=6,
            linewidth=2.5,
        )

        # --- Add moving average trendline (60-minute window) ---
        window_size = 20
        # Compute moving average and rolling std
        day_data_sorted = day_data.sort_values("minutes_since_midnight")
        ma = day_data_sorted["bikes_available"].rolling(window=window_size, min_periods=1, center=True).mean()
        std = day_data_sorted["bikes_available"].rolling(window=window_size, min_periods=1, center=True).std()
        # Smooth the std with another rolling mean
        std_smooth = std.rolling(window=window_size, min_periods=1, center=True).mean()
        # Plot the moving average trendline
        plt.plot(
            day_data_sorted["minutes_since_midnight"],
            ma,
            label=f"{day_type} Trend (20min MA)",
            color=viridis_colors[i],
            linewidth=4,
            alpha=0.5,
        )
        # Plot the smoothed rolling std as a shaded area around the trendline
        plt.fill_between(
            day_data_sorted["minutes_since_midnight"],
            ma - 3 * std_smooth,
            ma + 3 * std_smooth,
            color=viridis_colors[i],
            alpha=0.18,
            label=f"{day_type} ±3 Smoothed Std"
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

    # Configure legend for weekday/weekend
    plt.legend(title="Day Type", title_fontsize=10, loc="best")

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
