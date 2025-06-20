# Velib Station Data Scraper

This script collects availability data for 4 specific Velib stations in Paris:

1. "Pyrénées - Ménilmontant"
2. "Villiers de l'Isle Adam - Pyrénées"
3. "Mairie du 20ème"
4. "Gambetta - Père Lachaise"

## Features

- Runs every minute between 7am and 10am
- Fetches data from the Velib API
- Saves data to parquet files using polars library
- Logs errors to a separate log file
- Tracks various metrics including timestamp, bike availabilities, etc.

## Setup

1. Create a virtual environment (optional but recommended):

```powershell
python -m venv .
```

2. Set up Windows Task Scheduler to run the PowerShell script:
   - Open Task Scheduler (search for it in the Start menu)
   - Click "Create Basic Task"
   - Name it "Velib Data Collection" and click Next
   - Select "Daily" and click Next
   - Set the start time and click Next
   - Select "Start a program" and click Next
   - For Program/script, enter: `powershell.exe`
   - For Add arguments, enter: `-NoProfile -ExecutionPolicy Bypass -File "D:\Fun\velib-scraper\run_velib_scraper.ps1"`
   - For Start in, enter: `D:\Fun\velib-scraper`
   - Click Next, then Finish
   - Right-click on the created task and select "Properties"
   - Go to the "Triggers" tab, edit the trigger
   - Check "Repeat task every" and set it to 1 minute
   - Set "for a duration of" to 3 hours
   - Click OK to save

## Data Storage

Data is stored in the `data` directory as parquet files, one for each station.

## Visualization

The `viz.py` script allows you to visualize the collected data. It offers several command-line options to customize the plots.

### Usage

To run the visualization script, use the following command structure:

```bash
python viz.py <MODE> [OPTIONS]
```

Where `<MODE>` is either `average` or `stacked`.

### Arguments

- `MODE`: (Required) The type of plot to generate.
  - `average`: Shows the average bike availability, separating weekdays and weekends. It also includes a smoothed trendline.
  - `stacked`: Plots the availability for each individual day on the same graph to show daily variations.
- `-s`, `--save-to-file`: (Optional) Saves the plots as PNG files in the `plots/` directory. If this flag is not provided, the plots will be displayed on-screen directly.

### Examples

**1. Show the average availability plots for each station:**

```bash
python viz.py average
```

**2. Show the "stacked" plots with all daily data:**

```bash
python viz.py stacked
```

**3. Save the average availability plots to the `plots/` directory:**

```bash
python viz.py average -s
```

## Logs

Errors are logged to `logs/velib_errors.log`.

## API Reference

This script uses the Velib API available at:
<https://opendata.paris.fr/explore/dataset/velib-disponibilite-en-temps-reel/api/>
