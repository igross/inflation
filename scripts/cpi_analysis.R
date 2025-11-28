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

# ==============================================================================
# 1. Check Date
# ==============================================================================

cat("Checking latest CPI release date...\n")

cpi_check <- tryCatch(
  read_abs("6484.0", tables = 1, show_progress_bars = FALSE),
  error = function(e) stop("Failed to check CPI date: ", e$message)
)

last_obs_date <- max(cpi_check$date, na.rm = TRUE)
today <- Sys.Date()
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

cpi_headline_raw <- read_abs("6401.0", tables = 1, show_progress_bars = FALSE)
message("Loaded ", nrow(cpi_headline_raw), " rows from CPI headline table (6401.0, Table 1)")

cpi_analytical_raw <- read_abs("6401.0", tables = 8, show_progress_bars = FALSE)
message("Loaded ", nrow(cpi_analytical_raw), " rows from CPI analytical table (6401.0, Table 8)")

cpi_monthly_raw <- read_abs("6484.0", tables = 1, show_progress_bars = FALSE)
message("Loaded ", nrow(cpi_monthly_raw), " rows from monthly CPI indicator (6484.0, Table 1)")

headline_filtered <- cpi_headline_raw %>%
  filter(grepl("All Groups, Index Numbers", table_title, ignore.case = TRUE))
if (nrow(headline_filtered) == 0) headline_filtered <- cpi_headline_raw

analytical_trimmed <- cpi_analytical_raw %>%
  filter(grepl("Trimmed mean", table_title, ignore.case = TRUE))
if (nrow(analytical_trimmed) == 0) analytical_trimmed <- cpi_analytical_raw

analytical_weighted <- cpi_analytical_raw %>%
  filter(grepl("Weighted median", table_title, ignore.case = TRUE))
if (nrow(analytical_weighted) == 0) analytical_weighted <- cpi_analytical_raw

monthly_filtered <- cpi_monthly_raw %>%
  filter(grepl("Monthly CPI Indicator: All Groups", table_title, ignore.case = TRUE))
if (nrow(monthly_filtered) == 0) monthly_filtered <- cpi_monthly_raw

d_headline <- headline_filtered %>%
  filter(grepl("All groups CPI", series, ignore.case = TRUE)) %>%
  filter(unit == "Index Numbers") %>%
  select(date, value, series) %>%
  arrange(date) %>%
  mutate(measure = "Headline CPI")

d_trimmed <- analytical_trimmed %>%
  filter(grepl("Trimmed mean", series, ignore.case = TRUE)) %>%
  filter(unit == "Index Numbers") %>%
  select(date, value, series) %>%
  arrange(date) %>%
  mutate(measure = "Trimmed Mean")

d_weighted <- analytical_weighted %>%
  filter(grepl("Weighted median", series, ignore.case = TRUE)) %>%
  filter(unit == "Index Numbers") %>%
  select(date, value, series) %>%
  arrange(date) %>%
  mutate(measure = "Weighted Median")

d_monthly <- monthly_filtered %>%
  filter(grepl("All groups CPI", series, ignore.case = TRUE)) %>%
  filter(unit == "Index Numbers") %>%
  select(date, value, series) %>%
  arrange(date) %>%
  mutate(measure = "Monthly CPI Indicator")

message("Headline series: ", nrow(d_headline), " rows | date range: ", min(d_headline$date), " -> ", max(d_headline$date))
message("Trimmed mean series: ", nrow(d_trimmed), " rows | date range: ", min(d_trimmed$date), " -> ", max(d_trimmed$date))
message("Weighted median series: ", nrow(d_weighted), " rows | date range: ", min(d_weighted$date), " -> ", max(d_weighted$date))
message("Monthly CPI series: ", nrow(d_monthly), " rows | date range: ", min(d_monthly$date), " -> ", max(d_monthly$date))

cpi_data <- bind_rows(d_headline, d_trimmed, d_weighted, d_monthly)
message("Combined CPI data rows: ", nrow(cpi_data))
if (nrow(cpi_data) == 0) {
  stop("No CPI data available after filtering; cannot proceed with chart generation.")
}

# Ensure each measure/date pair is unique
cpi_data_unique <- cpi_data %>%
  group_by(measure, date) %>%
  arrange(measure, date) %>%
  summarise(value = first(value), .groups = "drop")

dup_summary <- cpi_data %>%
  count(measure, date) %>%
  filter(n > 1)
if (nrow(dup_summary) > 0) {
  message(
    "Detected ", sum(dup_summary$n - 1),
    " duplicate observations across ", nrow(dup_summary),
    " measure/date combinations; keeping the first value for each."
  )
}

# ==============================================================================
# 4. Compute Annualised Rates
# ==============================================================================

lags_m <- c(1, 3, 6, 12)
results_list <- list()

for (m in lags_m) {
  lagged <- cpi_data_unique %>%
    mutate(date_join = date %m-% months(m)) %>%
    select(measure, date_join, value_lag = value)

  joined <- cpi_data_unique %>%
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

cpi_recent <- cpi_rates %>%
  filter(date >= (max(date) - years(5)))

message("Rows in recent CPI data window: ", nrow(cpi_recent))

# ==============================================================================
# 5. Create Charts
# ==============================================================================

theme_set(theme_minimal(base_size = 12))

measures <- unique(cpi_rates$measure)
message("Generating plots for measures: ", paste(measures, collapse = ", "))
plots <- list()

for (m in measures) {
  d_plot <- cpi_recent %>% filter(measure == m)
  d_1m <- d_plot %>% filter(months == 1)
  d_3m <- d_plot %>% filter(months == 3)

  p <- ggplot() +
    annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.02, ymax = 0.03,
             fill = "lightgrey", alpha = 0.3) +
    geom_hline(yintercept = 0.025, colour = "black")

  if (nrow(d_1m) > 0) {
    p <- p + geom_col(data = d_1m, aes(x = date, y = rate_annualised),
                      fill = "steelblue", alpha = 0.6, width = 20)
  }

  if (nrow(d_3m) > 0) {
    p <- p + geom_line(data = d_3m, aes(x = date, y = rate_annualised),
                       colour = "darkred", size = 1)
  }

  p <- p +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = m,
         subtitle = "Bars = 1m Annualised, Line = 3m Annualised",
         x = NULL, y = "Annualised Rate")

  plots[[m]] <- p

  filename <- paste0("output/chart_", gsub(" ", "_", tolower(m)), ".png")
  message("Attempting to save chart for measure '", m, "' to ", filename)
  try(ggsave(filename, plots[[m]], width = 8, height = 6), silent = TRUE)
}

# ==============================================================================
# 6. Create Heatmap Table
# ==============================================================================

latest_data <- cpi_rates %>%
  group_by(measure, months) %>%
  filter(date == max(date)) %>%
  summarise(
    rate_annualised = if (all(is.na(rate_annualised))) {
      NA_real_
    } else {
      first(na.omit(rate_annualised))
    },
    .groups = "drop"
  ) %>%
  mutate(period = paste0(months, "m_annualised")) %>%
  select(-months) %>%
  pivot_wider(names_from = period, values_from = rate_annualised)

expected_cols <- c("1m_annualised", "3m_annualised", "6m_annualised", "12m_annualised")
for (col in expected_cols) {
  if (!col %in% names(latest_data)) latest_data[[col]] <- NA
}

latest_data <- latest_data %>%
  select(measure, all_of(expected_cols))

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
      val <- pmax(0.01, pmin(0.04, x))
      col_fun <- scales::col_numeric(
        palette = c("blue", "white", "red"),
        domain = c(0.02, 0.03),
        na.color = "grey90"
      )
      col_fun(val)
    }
  )

message("Attempting to save CPI annualised summary table to output/cpi_annualised_summary.html")
try(gtsave(cpi_table, "output/cpi_annualised_summary.html"), silent = TRUE)
write_csv(latest_data, "output/cpi_annualised_summary.csv")

message("Analysis complete. Outputs saved to output/")
