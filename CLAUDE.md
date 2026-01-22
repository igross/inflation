# CLAUDE.md - AI Assistant Guidelines for Inflation Repository

## Project Overview

This repository contains tools for analyzing Australian Consumer Price Index (CPI) data and calculating personalized inflation rates based on household characteristics. The primary data source is the Australian Bureau of Statistics (ABS).

## Repository Structure

```
inflation/
├── scripts/
│   ├── cpi_analysis.R       # Main analysis script - downloads data, calculates rates, generates charts
│   └── cpi_components.R     # CPI components and weights, contains Persona Engine
├── app/
│   └── personal_cpi_app.R   # Interactive Shiny web application
├── output/                   # Generated artifacts (charts, tables, data files)
│   ├── chart_*.png          # Inflation charts
│   ├── cpi_annualised_summary.csv/html  # Summary tables
│   ├── cpi_data.rds         # Processed CPI data
│   └── persona_cpi_indices.csv
├── .github/workflows/
│   └── run_r_scripts.yml    # GitHub Actions for automated daily analysis
├── Consumer Price Index - 2025 Weighting Pattern.xlsx  # ABS weighting data
└── README.md
```

## Key Technologies

- **R**: Primary language for data analysis
- **Required R packages**: `readabs`, `tidyverse`, `lubridate`, `scales`, `patchwork`, `gt`, `shiny`, `readxl`
- **Data Source**: Australian Bureau of Statistics via `readabs` package
- **Automation**: GitHub Actions (runs daily at 02:00 UTC / ~1pm Sydney time)

## Core Scripts

### cpi_analysis.R
- Downloads CPI data from ABS (catalog 6401.0 and 6484.0)
- Calculates annualised inflation rates for multiple time periods (1m, 3m, 6m, 12m)
- Generates visualization charts with RBA target band (2-3%)
- Produces summary tables in HTML and CSV format
- **Important**: Script checks if new CPI data is available before proceeding

### cpi_components.R
- Downloads detailed CPI component data (Table 7)
- Loads weights from bundled Excel workbook
- Contains `calculate_persona_weights()` function (the "Persona Engine")
- Exports reusable function for the Shiny app

### personal_cpi_app.R (Shiny App)
- Interactive UI for personalized inflation calculation
- Supports household toggles: vegetarian/vegan, transport mode, housing status, etc.
- Requires `output/cpi_data.rds` to exist (run `cpi_components.R` first)

## Development Workflow

### Running Locally

```bash
# Install R dependencies
Rscript -e 'install.packages(c("readabs", "tidyverse", "lubridate", "scales", "patchwork", "gt", "shiny", "readxl"))'

# Run main analysis
Rscript scripts/cpi_analysis.R

# Generate component data (required for Shiny app)
Rscript scripts/cpi_components.R

# Run Shiny app
Rscript -e 'shiny::runApp("app/personal_cpi_app.R")'
```

### GitHub Actions Automation

The workflow in `.github/workflows/run_r_scripts.yml`:
- Triggers: daily schedule, pull requests, manual dispatch
- Runs on macOS (for XQuartz graphics support)
- Auto-commits output changes with message "Update CPI analysis outputs [skip ci]"

## Key Concepts

### Annualisation Formula
Inflation rates are annualised using: `(1 + rate)^(12/months) - 1`

### Persona Engine
The `calculate_persona_weights()` function adjusts CPI component weights based on household characteristics:
- **Diet**: Vegetarian/Vegan (reallocates meat/dairy weights to plant-based foods)
- **Transport**: Car Owner / Car Free (adjusts vehicle vs public transport weights)
- **Housing**: Renter / Homeowner (adjusts rent vs ownership cost weights)
- **Lifestyle**: Traveller, Family, Smoker, Energy usage level

### RBA Target
Charts display the Reserve Bank of Australia's inflation target band (2-3%) with midpoint at 2.5%.

## Data Flow

1. `cpi_analysis.R` downloads raw ABS data and generates charts/tables
2. `cpi_components.R` processes component-level data and saves to `output/cpi_data.rds`
3. Shiny app loads `cpi_data.rds` and uses `calculate_persona_weights()` for calculations

## File Conventions

- Output files go in `output/` directory
- Charts are named `chart_<measure_name>.png`
- Data files use `.rds` format for R serialization
- Summary tables are generated in both CSV and HTML formats

## Important Notes for AI Assistants

1. **Data Freshness**: The analysis script checks ABS release dates - charts are only regenerated when new data is detected
2. **Dependencies**: The Shiny app depends on `cpi_components.R` being run first
3. **Output Directory**: Scripts create `output/` if it doesn't exist
4. **Weights File**: The Excel workbook with CPI weights must be present in the root directory
5. **Legacy Files**: `package.json` and `hardhat.config.js` appear to be from an unrelated Solidity project and are not used by the R analysis

## Common Tasks

### Adding a New Persona Toggle
1. Add toggle logic to `calculate_persona_weights()` in `cpi_components.R`
2. Add UI element in `personal_cpi_app.R` sidebar
3. Update server-side `toggles` list in the Shiny app

### Modifying Chart Appearance
- Chart theme is set in `cpi_analysis.R` using `theme_set(theme_minimal(base_size = 12))`
- RBA target band is hardcoded as 2-3% grey shading

### Updating CPI Weights
- Replace `Consumer Price Index - 2025 Weighting Pattern.xlsx` with new ABS weights file
- Verify column structure matches expected format in `cpi_components.R`
