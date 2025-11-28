library(readabs)
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(gt)

# Ensure output directory exists
if (!dir.exists("output")) {
  dir.create("output")
  message("Created missing output directory at './output'")
} else {
  message("Using existing output directory at './output'")
}

log_save_intent <- function(path, label) {
  dir_path <- dirname(path)
  can_write <- file.access(dir_path, 2) == 0
  message(
    "[", label, "] Preparing to save file: ", path,
    " | dir exists: ", dir.exists(dir_path),
    " | writable: ", can_write,
    " | absolute dir: ", normalizePath(dir_path, winslash = "/", mustWork = FALSE)
  )
}

log_save_result <- function(path, label, success) {
  if (!success) {
    message("[", label, "] Save failed; file not written. Checked path: ", path)
    return(invisible(FALSE))
  }

  if (file.exists(path)) {
    info <- file.info(path)
    message(
      "[", label, "] Save succeeded | size bytes: ", info$size,
      " | last modified: ", info$mtime,
      " | absolute path: ", normalizePath(path, winslash = "/", mustWork = FALSE)
    )
  } else {
    message(
      "[", label, "] Save reported success but file is missing at ", path,
      ". Check directory permissions or relative paths."
    )
  }
}

save_plot_with_logging <- function(plot_obj, filename, ...) {
  log_save_intent(filename, "plot")

  success <- tryCatch({
    ggsave(filename, plot_obj, ...)
    TRUE
  }, error = function(e) {
    message("[plot] Error while saving ", filename, ": ", e$message)
    FALSE
  })

  log_save_result(filename, "plot", success)
}

save_table_with_logging <- function(gt_obj, filename, ...) {
  log_save_intent(filename, "table")

  success <- tryCatch({
    gtsave(gt_obj, filename, ...)
    TRUE
  }, error = function(e) {
    message("[table] Error while saving ", filename, ": ", e$message)
    FALSE
  })

  log_save_result(filename, "table", success)
}

# ==============================================================================
# 1. Check Date
# ==============================================================================

# We need to check if today is one day after the latest CPI release.
# We'll check the latest release date from the CPI catalogue (6401.0).
# Note: This requires an internet connection.

cat("Checking latest CPI release date...\n")

# Get metadata for 6401.0 (Consumer Price Index)
# We can use read_abs_metadata or just download a small table to check dates.
# read_abs() downloads the data, so we can check the 'date' column of the latest observation.
# A more lightweight way might be checking the catalogue page, but readabs is the tool.

# We'll check the Monthly CPI Indicator (6484.0) for the latest data.
# If today is the release day, we expect the latest observation to be for the previous month.
# e.g. If today is Nov 26, we expect Oct data.

tryCatch({
  # Download just the key table for Monthly CPI to check dates quickly
  cpi_check <- read_abs("6484.0", tables = 1, show_progress_bars = FALSE)
}, error = function(e) {
  stop("Failed to check CPI date: ", e$message)
})

last_obs_date <- max(cpi_check$date, na.rm = TRUE)
today <- Sys.Date()

# Expected reference date is the 1st of last month
expected_date <- floor_date(today - months(1), "month")

cat("Latest CPI observation date:", as.character(last_obs_date), "\n")
cat("Expected observation date (if released today):", as.character(expected_date), "\n")
cat("Today's date:", as.character(today), "\n")

if (last_obs_date < expected_date) {
  message("Latest data is older than expected. Proceeding with currently available data.")
  message("Printing retrieved CPI data snapshot for verification...")
  print(utils::head(cpi_check, 10))
  message("Continuing without exiting so that charts and tables are still produced.")
} else {
  message("New CPI data detected! Proceeding with analysis.")
}

# ==============================================================================
# 2. Download Data
# ==============================================================================

message("Downloading CPI data...")

# A. Quarterly CPI Measures (6401.0)
# We need: Headline, Trimmed Mean, Weighted Median
# Table 1 has Headline.
# Table 8 has Analytical Series (Trimmed Mean, Weighted Median).

cpi_headline_raw <- read_abs("6401.0", tables = 1, show_progress_bars = FALSE)
message("Loaded ", nrow(cpi_headline_raw), " rows from CPI headline table (6401.0, Table 1)")

cpi_analytical_raw <- read_abs("6401.0", tables = 8, show_progress_bars = FALSE)
message("Loaded ", nrow(cpi_analytical_raw), " rows from CPI analytical table (6401.0, Table 8)")

# B. Monthly CPI Indicator (6484.0)
# Table 1.
cpi_monthly_raw <- read_abs("6484.0", tables = 1, show_progress_bars = FALSE)
message("Loaded ", nrow(cpi_monthly_raw), " rows from monthly CPI indicator (6484.0, Table 1)")

filter_table <- function(df, pattern, label) {
  filtered <- df %>% filter(grepl(pattern, table_title, ignore.case = TRUE))
  message(label, ": matched ", nrow(filtered), " rows using pattern '", pattern, "'")
  if (nrow(filtered) == 0) {
    message(label, ": no rows matched pattern; falling back to unfiltered data for diagnostics")
    return(df)
  }
  filtered
}

log_series_stats <- function(df, label) {
  if (nrow(df) == 0) {
    message(label, ": no data after filtering")
  } else {
    message(label, ": ", nrow(df), " rows | date range: ", min(df$date), " -> ", max(df$date))
  }
}

# ==============================================================================
# 3. Tidy and Process Data
# ==============================================================================

# Helper function to extract specific series
extract_series <- function(df, series_type_regex, measure_name) {
  df %>%
    filter(grepl(series_type_regex, series, ignore.case = TRUE)) %>%
    # Ensure we are taking the Index numbers, not percentage changes
    filter(unit == "Index Numbers") %>%
    select(date, value, series) %>%
    arrange(date) %>%
    mutate(measure = measure_name)
}

# Identify series names (inspecting 'series' column usually required, but we use regex)
# Headline: "All groups CPI"
# Trimmed mean: "Trimmed mean"
# Weighted median: "Weighted median"
# Monthly: "All groups CPI" (from 6484.0)

# Extract
d_headline <- cpi_headline_raw %>%
  filter_table("All Groups, Index Numbers", "Headline table") %>%
  extract_series("All groups CPI", "Headline CPI")

d_trimmed <- cpi_analytical_raw %>%
  filter_table("Trimmed mean", "Analytical table") %>%
  extract_series("Trimmed mean", "Trimmed Mean")

d_weighted <- cpi_analytical_raw %>%
  filter_table("Weighted median", "Analytical table") %>%
  extract_series("Weighted median", "Weighted Median")

d_monthly <- cpi_monthly_raw %>%
  filter_table("Monthly CPI Indicator: All Groups", "Monthly table") %>%
  extract_series("All groups CPI", "Monthly CPI Indicator")

log_series_stats(d_headline, "Headline series")
log_series_stats(d_trimmed, "Trimmed mean series")
log_series_stats(d_weighted, "Weighted median series")
log_series_stats(d_monthly, "Monthly CPI series")

# Combine
cpi_data <- bind_rows(d_headline, d_trimmed, d_weighted, d_monthly)

message("Combined CPI data rows: ", nrow(cpi_data))
if (nrow(cpi_data) == 0) {
  stop("No CPI data available after filtering; cannot proceed with chart generation.")
}

# ==============================================================================
# 4. Compute Annualised Rates
# ==============================================================================

# Function to calculate annualised change over m months
# annualised = (P_t / P_{t-m})^(12/m) - 1
calc_annualised <- function(x, m, freq_months) {
  # Lag is m periods. But data might be quarterly or monthly.
  # We need to handle the frequency.
  # If data is quarterly, 1-month change is NA.
  # If data is monthly, 1-month change is lag 1.
  
  # We'll handle this by grouping by measure and knowing the frequency.
  # Headline/Trimmed/Weighted are Quarterly (freq=3 months per step).
  # Monthly is Monthly (freq=1 month per step).
  
  # However, the lag 'n' in lag(x, n) depends on the row interval.
  # We will join the table to itself with a date offset.
  
  return(NA) # Placeholder, we do it via join below
}

# We'll do a robust join approach to handle gaps/frequency
cpi_calc <- cpi_data %>%
  select(measure, date, value) %>%
  distinct()

# Define lags in months
lags_m <- c(1, 3, 6, 12)

results_list <- list()

for (m in lags_m) {

  # Create a lagged version
  lagged <- cpi_calc %>%
    # Use past periods as the lag reference. Previously this pointed to future
    # dates which meant the latest observation had no matching comparison and
    # downstream tables ended up empty.
    mutate(date_join = date %m-% months(m)) %>%
    select(measure, date_join, value_lag = value)

  # Join
  joined <- cpi_calc %>%
    left_join(lagged, by = c("measure", "date" = "date_join")) %>%
    mutate(
      months = m,
      rate_annualised = (value / value_lag)^(12 / m) - 1
    ) %>%
    filter(!is.na(rate_annualised))

  message(
    "Computed ", nrow(joined), " rows for ", m, "-month annualised rate | date range: ",
    ifelse(nrow(joined) > 0, paste(min(joined$date), max(joined$date), sep = " -> "), "<none>")
  )

  results_list[[paste0("m", m)]] <- joined
}

cpi_rates <- bind_rows(results_list)

message("Computed annualised rates rows: ", nrow(cpi_rates))
if (nrow(cpi_rates) == 0) {
  stop("Annualised CPI rates are empty; cannot generate charts.")
}

# Filter for recent data for charts (e.g., last 5 years)
cpi_recent <- cpi_rates %>%
  filter(date >= (max(date) - years(5)))

message("Rows in recent CPI data window: ", nrow(cpi_recent))

# ==============================================================================
# 5. Create Charts
# ==============================================================================

# Theme setup
theme_set(theme_minimal(base_size = 12))

create_chart <- function(data, measure_name) {
  
  # Prepare data: 
  # Bars = 1-month rate (only valid for Monthly indicator usually, or quarterly if we treat quarter as 3-month block? 
  # Prompt says: "Bars = annualised one-month rate". 
  # For Quarterly series, 1-month rate doesn't exist. 
  # I will assume for Quarterly series, we might skip the bars or show the 3-month rate (quarterly) as bars?
  # Prompt says: "Annualised 1-month inflation... Create four charts... Bars = annualised one-month rate".
  # If the measure is quarterly, 1-month is impossible.
  # I will plot 1-month bars ONLY if they exist (i.e. for Monthly CPI). 
  # For Quarterly, maybe the user implies the quarterly rate (3-month) should be the bars?
  # "Bars = annualised one-month rate; Line = annualised 3-month rate".
  # If 1-month is NA (which it is for quarterly), it won't plot.
  
  # Filter for the specific measure
  d_plot <- data %>% filter(measure == measure_name)
  
  # Reshape for plotting
  # We want 1-month (bar) and 3-month (line)
  d_1m <- d_plot %>% filter(months == 1)
  d_3m <- d_plot %>% filter(months == 3)
  
  # Base plot
  p <- ggplot() +
    # RBA Target Band
    annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.02, ymax = 0.03,
             fill = "lightgrey", alpha = 0.3) +
    # Target Line
    geom_hline(yintercept = 0.025, colour = "black")
  
  # Bars (1-month)
  if (nrow(d_1m) > 0) {
    p <- p + geom_col(data = d_1m, aes(x = date, y = rate_annualised), 
                      fill = "steelblue", alpha = 0.6, width = 20) # width in days approx
  }
  
  # Line (3-month)
  if (nrow(d_3m) > 0) {
    p <- p + geom_line(data = d_3m, aes(x = date, y = rate_annualised), 
                       colour = "darkred", size = 1)
  }
  
  p <- p +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = measure_name,
         subtitle = "Bars = 1m Annualised, Line = 3m Annualised",
         x = NULL, y = "Annualised Rate")
  
  return(p)
}

measures <- unique(cpi_rates$measure)
message("Generating plots for measures: ", paste(measures, collapse = ", "))
plots <- list()

for (m in measures) {
  plots[[m]] <- create_chart(cpi_recent, m)

  # Save individual plot with detailed logging
  filename <- paste0("output/chart_", gsub(" ", "_", tolower(m)), ".png")
  message("Attempting to save chart for measure '", m, "' to ", filename)
  save_plot_with_logging(plots[[m]], filename, width = 8, height = 6)
}

# ==============================================================================
# 6. Create Heatmap Table
# ==============================================================================

# Reshape data for the table
# Rows: Measure
# Columns: 1m, 3m, 6m, 12m
# Value: Latest available rate for that measure/metric

# Get latest date for each measure
latest_data <- cpi_rates %>%
  group_by(measure) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(measure, months, rate_annualised) %>%
  mutate(period = paste0(months, "m_annualised")) %>%
  select(-months) %>%
  pivot_wider(names_from = period, values_from = rate_annualised)

# Ensure all columns exist
expected_cols <- c("1m_annualised", "3m_annualised", "6m_annualised", "12m_annualised")
for (col in expected_cols) {
  if (!col %in% names(latest_data)) latest_data[[col]] <- NA
}

# Reorder columns
latest_data <- latest_data %>%
  select(measure, all_of(expected_cols))

# Create GT table with heatmap
# Diverging scale: Blue (<2.5) - White (2.5) - Red (>2.5)
# Small deviations ±0.2 (2.3 to 2.7) nearly white.
# >= 3 deep red, <= 2 deep blue.

# We define a custom color mapping function
heatmap_col_fun <- function(x) {
  # Map x to color
  # We can use scales::col_numeric with a custom palette
  # But we need specific control points.
  # Let's use scales::rescale to map values to [0, 1] based on our domain
  # Domain: [2%, 3%] is the core.
  # Let's say range is [0, 5%] for visualization? Or dynamic?
  # Requirement: "Values ≥ 3% must be deep red; values ≤ 2% deep blue."
  # "Small deviations within ±0.2 ppts of 2.5 must be nearly white."
  
  # We'll create a palette function
  # 2.0 -> Blue
  # 2.3 -> Light Blue/White
  # 2.5 -> White
  # 2.7 -> Light Red/White
  # 3.0 -> Red
  
  scales::col_numeric(
    palette = c("darkblue", "blue", "white", "white", "red", "darkred"),
    domain = c(0, 0.02, 0.023, 0.027, 0.03, 0.05) # Approximate mapping
  )(x)
}

# Better approach with gt::data_color
# We can pass a function that returns colors.

cpi_table <- latest_data %>%
  gt() %>%
  fmt_percent(columns = -measure, decimals = 1) %>%
  cols_label(
    measure = "Measure",
    `1m_annualised` = "1-Month",
    `3m_annualised` = "3-Month",
    `6m_annualised` = "6-Month",
    `12m_annualised` = "12-Month"
  ) %>%
  tab_header(
    title = "Annualised Inflation Summary",
    subtitle = paste("Latest data as of", max(cpi_rates$date))
  ) %>%
  data_color(
    columns = -measure,
    fn = function(x) {
      # Normalize values for color mapping
      # We want 2.5% = White.
      # < 2% = Blue. > 3% = Red.
      
      # Let's define a color ramp
      # We'll clamp values for coloring purposes
      val <- pmax(0.01, pmin(0.04, x)) # Clamp between 1% and 4% for display range
      
      # Custom interpolation
      # 0.02 (2%) -> Blue
      # 0.025 (2.5%) -> White
      # 0.03 (3%) -> Red
      
      # We can use scales::col_bin or col_numeric
      # Let's construct a vector of colors
      
      col_fun <- scales::col_numeric(
        palette = c("blue", "white", "red"),
        domain = c(0.02, 0.03), # This maps 2% to Blue, 3% to Red. 2.5% is White.
        na.color = "grey90"
      )
      
      # However, we want "Deep Blue" at <= 2% and "Deep Red" at >= 3%.
      # And "Nearly white" at 2.3-2.7.
      # The linear interpolation from 2 to 3 crossing white at 2.5 works well for this.
      # 2.3 is 30% towards white from blue? No.
      # 2.0 (Blue) -- 2.5 (White) -- 3.0 (Red)
      
      col_fun(val)
    }
  )

# Save table
message("Attempting to save CPI annualised summary table to output/cpi_annualised_summary.html")
save_table_with_logging(cpi_table, "output/cpi_annualised_summary.html")
# Also save CSV
write_csv(latest_data, "output/cpi_annualised_summary.csv")

message("Analysis complete. Outputs saved to output/")
