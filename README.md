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

## Testing

The test suite runs the whole pipeline offline against seeded, deterministic
synthetic data - no ABS download is made and the committed `output/` folder is
never touched.

```bash
# from the repository root
Rscript -e 'install.packages(c("readabs","readxl","tidyverse","lubridate","scales","patchwork","gt","shiny","testthat","openxlsx","png","xml2"))'
Rscript tests/testthat.R
```

- `tests/synthetic/generate_synthetic_data.R` builds readabs-shaped tibbles for
  6401.0 Tables 1, 7 and 8 and 6484.0 Table 1, plus a synthetic
  `Consumer Price Index - 2025 Weighting Pattern.xlsx` with the ABS layout.
- `tests/synthetic/mock_profile.R` is used as `R_PROFILE_USER` for the script
  subprocesses; it replaces `readabs::read_abs()` with the synthetic version when
  the scripts call `library(readabs)`. Proxy variables point at a dead port so
  any stray network call fails.
- `tests/testthat/test-pipeline-e2e.R` copies `scripts/` and `app/` into a temp
  sandbox, runs `cpi_analysis.R` and `cpi_components.R` there with `Rscript`, and
  checks every export (the four chart PNGs, `cpi_annualised_summary.csv`/`.html`,
  `cpi_data.rds`, `persona_cpi_indices.csv`) plus the Shiny app's server outputs.
- `tests/testthat/test-persona-weights.R` unit-tests `calculate_persona_weights()`.

The tests also run on every pull request (`.github/workflows/tests.yml`).

## Methodology

- **Annualisation**: Inflation rates are annualised using the formula `(1 + Δ_m)^(12 / m) - 1`.
- **RBA Target**: Charts include a grey band (2-3%) and a black line (2.5%) representing the RBA inflation target.
- **Personas**: The personalised CPI is calculated by re-weighting the official CPI components based on user selection (e.g., Vegetarian, Car Owner, Renter).

## Automation

The GitHub Action runs daily at 01:31 UTC (11:31 AEST). It checks if a new CPI release occurred yesterday. If so, it runs the analysis and updates the outputs.
