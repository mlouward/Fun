import re
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional

import matplotlib.pyplot as plt
import pandas as pd
import polars as pl
import seaborn as sns
import typer
from matplotlib.axes import Axes

# --- Constants and Setup ---
sns.set_theme(style="whitegrid")
sns.set_context("notebook", font_scale=1.1)

PARQUET_DIR: Path = Path("data")
OUTPUT_DIR: Path = Path("plots")
OUTPUT_DIR.mkdir(exist_ok=True)

TARGET_STATIONS: List[str] = [
    "Pyrénées - Ménilmontant",
    "Villiers de l'Isle Adam - Pyrénées",
    "Mairie du 20ème",
    "Gambetta - Père Lachaise",
]

STATION_TO_FILENAME: Dict[str, str] = {
    station: station.replace(" - ", "_").replace(" ", "_").replace("'", "").lower() + ".parquet"
    for station in TARGET_STATIONS
}

app = typer.Typer()

class PlotMode(str, Enum):
    average = "average"
    stacked = "stacked"


def get_station_data(station_name: str) -> Optional[pl.DataFrame]:
    """Reads and returns the Polars DataFrame for a given station."""
    filename = STATION_TO_FILENAME.get(station_name)
    if not filename:
        print(f"Station '{station_name}' not found in mapping.")
        return None

    parquet_path = PARQUET_DIR / filename
    if not parquet_path.exists():
        print(f"No data file found for station: {station_name}")
        return None

    return pl.read_parquet(parquet_path)


def parse_period(period_str: Optional[str]) -> Optional[timedelta]:
    if not period_str:
        return None

    match = re.match(r"(\d+)([dwmy])", period_str.lower())
    if not match:
        typer.echo(f"Invalid period format: {period_str}. Ignoring.")
        return None

    value, unit = int(match.group(1)), match.group(2)

    if unit == 'd':
        return timedelta(days=value)
    elif unit == 'w':
        return timedelta(weeks=value)
    elif unit == 'm':
        # Approximate month as 30 days
        return timedelta(days=value * 30)
    elif unit == 'y':
        # Approximate year as 365 days
        return timedelta(days=value * 365)
    return None


def preprocess_data(df: pl.DataFrame, period: Optional[str] = None) -> pd.DataFrame:
    """Preprocesses the raw data and prepares it for plotting."""

    # Date filtering based on period
    if period:
        delta = parse_period(period)
        if delta:
            start_date = datetime.now().date() - delta
            df = df.filter(pl.col("date") >= start_date)

    # Ensure correct dtypes and filter for the morning window
    processed_df = df.with_columns(
        pl.col("date").cast(pl.Date),
        pl.col("time").str.to_datetime("%H:%M:%S").dt.time(),
    ).filter((pl.col("time").dt.hour() >= 7) & (pl.col("time").dt.hour() < 10))

    pdf = processed_df.to_pandas()
    if pdf.empty:
        return pdf
    pdf["minutes_since_midnight"] = pdf["time"].apply(lambda t: t.hour * 60 + t.minute)
    pdf["day_type"] = pdf["date"].apply(lambda d: "Weekday" if pd.to_datetime(d).weekday() < 5 else "Weekend")
    return pdf


def plot_average_mode(ax: Axes, data: pd.DataFrame) -> None:
    """Plots the average bike availability for weekdays and weekends."""
    viridis_colors = sns.color_palette("viridis", 2)
    grouped = data.groupby(["day_type", "minutes_since_midnight"])["bikes_available"].mean().reset_index()

    for i, day_type in enumerate(["Weekday", "Weekend"]):
        day_data = grouped[grouped["day_type"] == day_type].copy()
        ax.plot(
            day_data["minutes_since_midnight"],
            day_data["bikes_available"],
            "-o",
            label=f"{day_type} Avg",
            color=viridis_colors[i],
            markersize=6,
            linewidth=2.5,
        )

        # Add moving average trendline
        window_size = 20
        day_data_sorted = day_data.sort_values("minutes_since_midnight")
        ma = day_data_sorted["bikes_available"].rolling(window=window_size, min_periods=1, center=True).mean()
        ax.plot(
            day_data_sorted["minutes_since_midnight"],
            ma,
            label=f"{day_type} Trend (20min MA)",
            color=viridis_colors[i],
            linewidth=4,
            alpha=0.5,
        )


def plot_stacked_mode(ax: Axes, data: pd.DataFrame) -> None:
    """Plots bike availability for each day individually."""
    viridis_colors = sns.color_palette("viridis", 2)
    for date in data["date"].unique():
        day_data = data[data["date"] == date]
        day_type = "Weekday" if pd.to_datetime(date).weekday() < 5 else "Weekend"
        color = viridis_colors[0] if day_type == "Weekday" else viridis_colors[1]
        ax.plot(
            day_data["minutes_since_midnight"],
            day_data["bikes_available"],
            label=f"{pd.to_datetime(date).strftime('%Y-%m-%d')} ({day_type})",
            color=color,
            alpha=0.7,
        )


def configure_plot_aesthetics(ax: Axes, station_name: str) -> None:
    """Configures the aesthetics of the plot (labels, title, ticks)."""
    ax.set_xlim(410, 610)
    tick_positions = range(420, 601, 30)
    tick_labels = [f"{h:02d}:{m:02d}" for h in range(7, 11) for m in [0, 30] if h < 10 or m == 0]
    ax.set_xticks(tick_positions)
    ax.set_xticklabels(tick_labels, rotation=45)
    ax.grid(True, axis="x", linestyle="--", alpha=0.7)
    ax.set_title(f"Bike Availability at {station_name}", fontsize=14)
    ax.set_xlabel("Time of Day", fontsize=12)
    ax.set_ylabel("Number of Bikes Available", fontsize=12)
    ax.legend(title="Day Type", title_fontsize=10, loc="best")


def plot_station_visual(station_name: str, mode: str, save_to_file: bool, period: Optional[str] = None) -> None:
    """Orchestrates the creation of a single station visualization."""
    raw_data = get_station_data(station_name)
    if raw_data is None:
        return

    plot_data = preprocess_data(raw_data, period)

    if plot_data.empty:
        print(f"No data available for station '{station_name}' in the selected period.")
        return

    fig, ax = plt.subplots(figsize=(12, 6))

    if mode == "average":
        plot_average_mode(ax, plot_data)
    elif mode == "stacked":
        plot_stacked_mode(ax, plot_data)

    configure_plot_aesthetics(ax, station_name)

    plt.tight_layout()
    if save_to_file:
        filename = STATION_TO_FILENAME[station_name]
        output_path = OUTPUT_DIR / f"{filename.replace('.parquet', '')}_availability.png"
        plt.savefig(output_path, dpi=120)
        print(f"Plot saved to {output_path}")
        plt.close(fig)
    else:
        plt.show()


@app.command()
def main(
    mode: PlotMode = typer.Argument(
        ..., help="'average' to show avg daily availability, 'stacked' to show all daily data."
    ),
    save_to_file: bool = typer.Option(
        False, "-s", "--save-to-file", help="Save plots to file instead of displaying them."
    ),
    period: Optional[str] = typer.Option(
        None, "-p", "--period", help="Filter data for a specific period (e.g., '10d', '2w', '1m', '1y')."
    ),
) -> None:
    """Main function to run the visualization script."""
    print("Starting Velib data visualization...")

    for station in TARGET_STATIONS:
        print(f"Processing station: {station}")
        plot_station_visual(station, mode=mode.value, save_to_file=save_to_file, period=period)

    print("Visualization complete!")


if __name__ == "__main__":
    app()
