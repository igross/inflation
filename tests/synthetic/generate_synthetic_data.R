# Synthetic stand-ins for every external input used by the pipeline:
#
#   * readabs::read_abs("6401.0", tables = 1)  - CPI headline (All groups)
#   * readabs::read_abs("6401.0", tables = 7)  - CPI expenditure-class components
#   * readabs::read_abs("6401.0", tables = 8)  - CPI analytical series
#   * readabs::read_abs("6484.0", tables = 1)  - Monthly CPI indicator
#   * "Consumer Price Index - 2025 Weighting Pattern.xlsx" (sheet "Table 1")
#
# Everything is seeded and deterministic. The ABS tibbles use the same 12
# columns, types and series/title/unit conventions that readabs returns, so the
# production scripts run unchanged against them.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

SYNTH_SEED <- 20251126L

CITIES <- c("Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth",
            "Hobart", "Darwin", "Canberra", "Australia")

# The complete CPI moved to monthly in 2025; the monthly indicator (6484.0)
# stopped at September 2025. Mirror those shapes.
CPI_DATES <- seq(as.Date("2017-09-01"), as.Date("2026-09-01"), by = "month")
INDICATOR_DATES <- seq(as.Date("2017-09-01"), as.Date("2025-09-01"), by = "month")

# Group -> sub-group -> expenditure classes, using real ABS names so that the
# persona keyword rules in calculate_persona_weights() have something to bite on.
CPI_STRUCTURE <- list(
  "FOOD AND NON-ALCOHOLIC BEVERAGES" = list(
    "Bread and cereal products" = c("Bread", "Cakes and biscuits", "Breakfast cereals", "Other cereal products"),
    "Meat and seafoods" = c("Beef and veal", "Pork", "Lamb and goat", "Poultry", "Other meats", "Fish and other seafood"),
    "Dairy and related products" = c("Milk", "Cheese", "Ice cream and other dairy products"),
    "Fruit and vegetables" = c("Fruit", "Vegetables"),
    "Food products n.e.c." = c("Eggs", "Jams, honey and spreads"),
    "Meals out and take away foods" = c("Restaurant meals", "Take away and fast foods")
  ),
  "ALCOHOL AND TOBACCO" = list(
    "Alcoholic beverages" = c("Spirits", "Wine", "Beer"),
    "Tobacco" = c("Tobacco")
  ),
  "HOUSING" = list(
    "Rents" = c("Rents"),
    "New dwelling purchase by owner-occupiers" = c("New dwelling purchase by owner-occupiers"),
    "Other housing" = c("Property rates and charges", "Maintenance and repair of the dwelling"),
    "Utilities" = c("Electricity", "Gas and other household fuels", "Water and sewerage")
  ),
  "HEALTH" = list(
    "Medical, dental and hospital services" = c("Medical and hospital services", "Dental services")
  ),
  "TRANSPORT" = list(
    "Private motoring" = c("Motor vehicles", "Spare parts and accessories for motor vehicles",
                           "Automotive fuel", "Maintenance and repair of motor vehicles"),
    "Public transport" = c("Urban transport fares")
  ),
  "RECREATION AND CULTURE" = list(
    "Holiday travel and accommodation" = c("Domestic holiday travel and accommodation",
                                           "International holiday travel and accommodation")
  ),
  "EDUCATION" = list(
    "Education" = c("Preschool and primary education", "Tertiary education")
  ),
  "INSURANCE AND FINANCIAL SERVICES" = list(
    "Insurance" = c("Insurance")
  ),
  "SOCIAL PROTECTION" = list(
    "Child care" = c("Child care")
  )
)

#' Expenditure-class weights (percent of All groups, summing to 100).
synthetic_weights <- function(seed = SYNTH_SEED) {
  set.seed(seed)
  rows <- imap_dfr(CPI_STRUCTURE, function(subs, grp) {
    imap_dfr(subs, function(classes, sg) {
      tibble(group = grp, sub_group = sg, expenditure_class = classes)
    })
  })
  raw <- runif(nrow(rows), 0.2, 3)
  rows$weight_class <- round(raw / sum(raw) * 100, 2)
  rows
}

#' Write a workbook laid out like the ABS "2025 Weighting Pattern" release:
#' six title rows, a column-header row, then group / sub-group / class rows with
#' weights in columns D / E / F respectively.
write_synthetic_weights_xlsx <- function(path, seed = SYNTH_SEED) {
  w <- synthetic_weights(seed)
  body <- list()
  for (grp in unique(w$group)) {
    g <- w[w$group == grp, ]
    body[[length(body) + 1]] <- list(grp, NA, NA, sum(g$weight_class), NA, NA)
    for (sg in unique(g$sub_group)) {
      s <- g[g$sub_group == sg, ]
      body[[length(body) + 1]] <- list(NA, sg, NA, NA, sum(s$weight_class), NA)
      for (i in seq_len(nrow(s))) {
        body[[length(body) + 1]] <- list(NA, NA, s$expenditure_class[i], NA, NA, s$weight_class[i])
      }
    }
  }
  body_df <- as.data.frame(do.call(rbind, lapply(body, function(r) {
    as.data.frame(r, col.names = paste0("X", 1:6), stringsAsFactors = FALSE)
  })), stringsAsFactors = FALSE)
  body_df[4:6] <- lapply(body_df[4:6], as.numeric)

  header <- data.frame(
    X1 = c(NA, "Consumer Price Index, Weighting Pattern, 2025",
           "Released at 11:30am (Canberra time) 26 November 2025",
           "Table 1. CPI weights, December 2024, Weighted average of eight capital cities",
           NA, NA, "Group, sub-group and expenditure class"),
    X2 = NA, X3 = NA,
    X4 = c(NA, NA, NA, NA, NA, "2025 weights",
           "Percentage contribution to the All groups CPI in December 2024"),
    X5 = NA, X6 = NA, stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Contents")
  openxlsx::writeData(wb, "Contents", "Synthetic test fixture - not real ABS data", startRow = 1)
  openxlsx::addWorksheet(wb, "Table 1")
  openxlsx::writeData(wb, "Table 1", header, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Table 1", body_df, startRow = nrow(header) + 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(w)
}

# A smooth, noisy log-price path: ~3% a year trend with a 2022 inflation hump.
index_path <- function(dates, trend = 0.03, noise = 0.002, base = 100) {
  t <- seq_along(dates)
  yrs <- as.numeric(format(dates, "%Y")) + (as.numeric(format(dates, "%m")) - 1) / 12
  hump <- 0.04 * exp(-((yrs - 2022.6)^2) / 0.5)
  monthly <- trend / 12 + hump / 12 + rnorm(length(t), 0, noise)
  monthly[1] <- 0
  round(base * exp(cumsum(monthly)), 1)
}

abs_rows <- function(table_no, table_title, dates, series, value, series_id,
                     unit = "Index Numbers", data_type = "INDEX",
                     frequency = "Month") {
  tibble(
    table_no = table_no,
    sheet_no = "Data1",
    table_title = table_title,
    date = dates,
    series = series,
    value = value,
    series_type = "Original",
    data_type = data_type,
    collection_month = format(dates, "%m") |> as.integer() |> as.character(),
    frequency = frequency,
    series_id = series_id,
    unit = unit
  )
}

series_id_for <- function(...) {
  key <- paste(..., sep = "|")
  # Deterministic ABS-style series ID (A + 7 digits + letter)
  h <- sum(utf8ToInt(key) * seq_along(utf8ToInt(key))) %% 1e7
  paste0("A", sprintf("%07d", h), LETTERS[(nchar(key) %% 26) + 1])
}

# Index numbers + a percent-change series for each (measure, region) pair.
index_and_pct <- function(table_no, title, dates, measure, regions, sep_suffix = TRUE) {
  map_dfr(regions, function(region) {
    idx <- index_path(dates)
    pct <- c(NA, round(100 * (idx[-1] / idx[-length(idx)] - 1), 1))
    bind_rows(
      abs_rows(table_no, title, dates,
               paste0("Index Numbers ;  ", measure, " ;  ", region, " ;"),
               idx, series_id_for(table_no, measure, region, "idx")),
      abs_rows(table_no, title, dates,
               paste0("Percentage Change from Previous Period ;  ", measure, " ;  ", region, " ;"),
               pct, series_id_for(table_no, measure, region, "pct"),
               unit = "Percent", data_type = "PERCENT")
    )
  })
}

synthetic_6401_t1 <- function() {
  index_and_pct("640101", "TABLE 1. CPI: All Groups, Index Numbers and Percentage Changes",
                CPI_DATES, "All groups CPI", CITIES)
}

synthetic_6401_t8 <- function() {
  title <- "TABLE 8. CPI: Analytical Series, Weighted Average of Eight Capital Cities"
  bind_rows(
    index_and_pct("640108", title, CPI_DATES, "Trimmed Mean", "Australia"),
    index_and_pct("640108", title, CPI_DATES, "Weighted Median", "Australia"),
    index_and_pct("640108", title, CPI_DATES, "All groups CPI, seasonally adjusted", "Australia")
  )
}

synthetic_6484_t1 <- function() {
  index_and_pct("648401", "TABLE 1. Monthly CPI Indicator: All Groups, Index Numbers and Percentage Changes",
                INDICATOR_DATES, "All groups CPI", CITIES)
}

# Table 7: one index series per expenditure class (plus All groups), for the
# eight-capital weighted average and one city that should be filtered out.
# NOTE: the series label puts the component name first
# ("<class> ;  Weighted Average of Eight Capital Cities ;"), which is the layout
# scripts/cpi_components.R parses with gsub(";.*", "", series). Other ABS
# tables label series "Index Numbers ;  <name> ;  <region> ;", so this layout is
# an assumption that has not been checked against a live Table 7 download.
synthetic_6401_t7 <- function(seed = SYNTH_SEED) {
  w <- synthetic_weights(seed)
  title <- "TABLE 7. CPI: Group, Sub-group and Expenditure Class, Index Numbers by Capital City"
  comps <- c("All groups CPI", w$expenditure_class)
  map_dfr(comps, function(comp) {
    map_dfr(c("Weighted Average of Eight Capital Cities", "Sydney"), function(region) {
      idx <- index_path(CPI_DATES, trend = runif(1, 0.005, 0.06))
      bind_rows(
        abs_rows("640107", title, CPI_DATES,
                 paste0(comp, " ;  ", region, " ;"),
                 idx, series_id_for("640107", comp, region, "idx")),
        abs_rows("640107", title, CPI_DATES,
                 paste0(comp, " ;  Percentage Change from Previous Period ;  ", region, " ;"),
                 c(NA, round(100 * diff(idx) / head(idx, -1), 1)),
                 series_id_for("640107", comp, region, "pct"),
                 unit = "Percent", data_type = "PERCENT")
      )
    })
  })
}

#' Drop-in replacement for readabs::read_abs(). Deterministic per (cat_no, table).
synthetic_read_abs <- function(cat_no = NULL, tables = "all", series_id = NULL,
                               path = NULL, metadata = TRUE, show_progress_bars = TRUE,
                               retain_files = TRUE, check_local = TRUE, release_date = "latest") {
  if (!is.null(series_id)) stop("synthetic_read_abs: series_id lookups are not mocked")
  tables <- as.character(tables)
  out <- map_dfr(tables, function(tb) {
    key <- paste(cat_no, tb)
    set.seed(SYNTH_SEED + sum(utf8ToInt(key)))
    switch(key,
      "6401.0 1" = synthetic_6401_t1(),
      "6401.0 7" = synthetic_6401_t7(),
      "6401.0 8" = synthetic_6401_t8(),
      "6484.0 1" = synthetic_6484_t1(),
      stop("synthetic_read_abs: no synthetic data for catalogue ", cat_no, " table ", tb)
    )
  })
  message("[synthetic] read_abs(", cat_no, ", tables = ", paste(tables, collapse = ","),
          ") -> ", nrow(out), " rows")
  out
}

#' Lay out a sandbox mirroring the repo root: scripts/, app/ and the weights
#' workbook (synthetic). Scripts write to <dir>/output, never the real output/.
build_sandbox <- function(repo_root, dir = tempfile("cpi-sandbox-")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (sub in c("scripts", "app")) {
    dir.create(file.path(dir, sub), showWarnings = FALSE)
    file.copy(list.files(file.path(repo_root, sub), full.names = TRUE),
              file.path(dir, sub), overwrite = TRUE)
  }
  write_synthetic_weights_xlsx(file.path(dir, "Consumer Price Index - 2025 Weighting Pattern.xlsx"))
  normalizePath(dir)
}
