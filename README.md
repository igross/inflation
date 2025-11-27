# Inflation Analysis Repository

This repository contains tools to analyse Australian CPI data and calculate personalised inflation rates.

## Project Structure

- `scripts/`
  - `cpi_analysis.R`: Main analysis script. Checks for new ABS CPI releases, downloads data, calculates annualised rates, and produces charts/tables.
  - `cpi_components.R`: Downloads detailed CPI components and weights. Contains the "Persona Engine" to recalculate CPI for different household types.
- `app/`
  - `personal_cpi_app.R`: Interactive Shiny application allowing users to toggle household characteristics and see their personalised inflation rate.
- `.github/workflows/`
  - `cpi-analysis.yml`: GitHub Actions workflow to run the analysis daily (executes only on the day after a CPI release).
- `output/`: Directory where generated charts, tables, and data files are saved.

## Usage

### Running Locally

1. **Install Dependencies**:
   ```r
   install.packages(c("readabs", "tidyverse", "lubridate", "scales", "patchwork", "gt", "shiny"))
   ```

2. **Run Analysis**:
   ```bash
   Rscript scripts/cpi_analysis.R
   ```
   *Note: This script will exit early if today is not the day after a CPI release.*
   The images and tables in `output/` are only generated when new CPI data is detected,
   so running the script on other days will finish quickly without creating charts.

3. **Generate Component Data**:
   ```bash
   Rscript scripts/cpi_components.R
   ```
   This downloads component data and saves `output/cpi_data.rds`.

4. **Run the App**:
   Open `app/personal_cpi_app.R` in RStudio and click "Run App", or:
   ```r
   shiny::runApp("app/personal_cpi_app.R")
   ```

## Methodology

- **Annualisation**: Inflation rates are annualised using the formula `(1 + Δ_m)^(12 / m) - 1`.
- **RBA Target**: Charts include a grey band (2-3%) and a black line (2.5%) representing the RBA inflation target.
- **Personas**: The personalised CPI is calculated by re-weighting the official CPI components based on user selection (e.g., Vegetarian, Car Owner, Renter).

## Automation

The GitHub Action runs daily at 01:31 UTC (11:31 AEST). It checks if a new CPI release occurred yesterday. If so, it runs the analysis and updates the outputs.
