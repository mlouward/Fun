# PowerShell script to activate venv and run the Velib scraper

# Script location
$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $scriptPath

# Activate the virtual environment
Write-Host "Activating virtual environment..."
.\Scripts\Activate.ps1

# Check if requirements are installed
try {
    python -c "import polars"
    Write-Host "Requirements are already installed."
} catch {
    Write-Host "Installing requirements..."
    python -m pip install -e .
}

# Run the Python script
Write-Host "Running Velib scraper..."
python main.py

# Deactivate virtual environment
deactivate
Write-Host "Done."
