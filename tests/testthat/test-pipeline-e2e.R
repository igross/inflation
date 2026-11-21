# End-to-end: run every pipeline script against synthetic inputs in a temporary
# sandbox and check every export it writes.

measures <- c("Headline CPI", "Monthly CPI Indicator", "Trimmed Mean", "Weighted Median")

real_output_md5 <- tools::md5sum(list.files(file.path(repo_root(), "output"), full.names = TRUE))

test_that("synthetic weights workbook parses like the ABS release", {
  path <- file.path(pipeline_run()$sandbox, "Consumer Price Index - 2025 Weighting Pattern.xlsx")
  expect_true("Table 1" %in% readxl::excel_sheets(path))
  raw <- suppressMessages(readxl::read_excel(path, sheet = "Table 1", skip = 6, col_names = FALSE))
  expect_equal(ncol(raw), 6)
  expect_equal(raw[[1]][1], "Group, sub-group and expenditure class")
  expect_equal(sum(raw[[6]], na.rm = TRUE), 100, tolerance = 0.05)
})

test_that("synthetic read_abs is deterministic and readabs-shaped", {
  a <- synthetic_read_abs("6401.0", tables = 1)
  b <- synthetic_read_abs("6401.0", tables = 1)
  expect_identical(a, b)
  expect_named(a, c("table_no", "sheet_no", "table_title", "date", "series", "value",
                    "series_type", "data_type", "collection_month", "frequency",
                    "series_id", "unit"))
  expect_s3_class(a$date, "Date")
  expect_type(a$value, "double")
  expect_error(synthetic_read_abs("6202.0", tables = 1), "no synthetic data")
})

test_that("cpi_analysis.R runs cleanly on synthetic data", {
  run <- pipeline_run()$analysis
  expect_equal(run$status, 0, info = paste(run$log, collapse = "\n"))
  expect_true(any(grepl("[synthetic] readabs::read_abs mocked", run$log, fixed = TRUE)))
  expect_true(any(grepl("Analysis complete", run$log, fixed = TRUE)))
})

test_that("cpi_components.R runs cleanly on synthetic data", {
  run <- pipeline_run()$components
  expect_equal(run$status, 0, info = paste(run$log, collapse = "\n"))
  expect_true(any(grepl("[synthetic] readabs::read_abs mocked", run$log, fixed = TRUE)))
  expect_false(any(grepl("Matched 0 components", run$log, fixed = TRUE)))
})

test_that("one chart PNG per measure", {
  for (m in measures) {
    expect_valid_png(out_file(paste0("chart_", gsub(" ", "_", tolower(m)), ".png")))
  }
})

test_that("cpi_annualised_summary.csv has expected structure", {
  path <- out_file("cpi_annualised_summary.csv")
  expect_nonempty_file(path)
  d <- readr::read_csv(path, show_col_types = FALSE)
  expect_named(d, c("measure", "1m_annualised", "3m_annualised", "6m_annualised", "12m_annualised"))
  expect_setequal(d$measure, measures)
  expect_equal(nrow(d), length(measures))
  expect_false(anyNA(d))
  rates <- as.matrix(d[, -1])
  expect_true(all(abs(rates) < 0.5)) # synthetic paths are ~0-10% a year
})

test_that("cpi_annualised_summary.html is a valid gt table", {
  path <- out_file("cpi_annualised_summary.html")
  expect_nonempty_file(path)
  doc <- xml2::read_html(path)
  expect_length(xml2::xml_find_all(doc, "//table"), 1)
  txt <- xml2::xml_text(doc)
  expect_match(txt, "Annualised Inflation Summary", fixed = TRUE)
  for (m in measures) expect_match(txt, m, fixed = TRUE)
  headers <- xml2::xml_text(xml2::xml_find_all(doc, "//th"))
  expect_true(all(c("1-Month", "3-Month", "6-Month", "12-Month") %in% trimws(headers)))
})

test_that("cpi_data.rds holds components, weights and canonical CPI", {
  path <- out_file("cpi_data.rds")
  expect_nonempty_file(path)
  x <- readRDS(path)
  expect_named(x, c("components", "weights", "canonical_cpi"))

  expect_named(x$components, c("date", "value", "series", "series_id", "component"))
  expect_gt(nrow(x$components), 0)
  expect_false(anyNA(x$components$value))

  expect_named(x$weights, c("component", "weight"))
  expect_equal(nrow(x$weights), nrow(synthetic_weights()))
  expect_equal(sum(x$weights$weight), 1)
  expect_setequal(unique(x$components$component), x$weights$component)

  expect_named(x$canonical_cpi, c("date", "value", "measure"))
  expect_equal(nrow(x$canonical_cpi), length(CPI_DATES))
  expect_false(anyNA(x$canonical_cpi$value))
  expect_true(all(x$canonical_cpi$value > 50 & x$canonical_cpi$value < 200))
})

test_that("persona_cpi_indices.csv has every persona for every date", {
  path <- out_file("persona_cpi_indices.csv")
  expect_nonempty_file(path)
  d <- readr::read_csv(path, show_col_types = FALSE)
  expect_named(d, c("date", "value", "measure"))
  expect_setequal(unique(d$measure), paste("Persona:", c("Vegetarian", "Car_Owner", "Renter")))
  expect_equal(nrow(d), 3 * length(CPI_DATES))
  expect_false(anyNA(d))
  expect_s3_class(d$date, "Date")
})

test_that("Shiny app renders its plot and table and saves the plot", {
  skip_if_not_installed("shiny")
  sandbox <- pipeline_run()$sandbox
  old <- setwd(sandbox)
  on.exit(setwd(old), add = TRUE)
  app_env <- new.env()
  app <- suppressPackageStartupMessages(
    source(file.path("app", "personal_cpi_app.R"), local = app_env)$value
  )
  expect_s3_class(app, "shiny.appobj")

  shiny::testServer(app, {
    session$setInputs(vegetarian = TRUE, vegan = FALSE, transport = "car_free",
                      housing = "renter", traveller = FALSE, family = TRUE,
                      smoker = FALSE, energy = "high", update = 1)
    df <- combined_data()
    expect_setequal(unique(df$measure), c("Headline CPI", "Personalised CPI"))
    expect_false(anyNA(df$rate))
    plot <- output$inflation_plot
    expect_match(plot$src, "^data:image/png;base64,")
    tbl <- output$inflation_table
    expect_match(as.character(tbl$html), "Current Rate", fixed = TRUE)
  })

  expect_valid_png(file.path(sandbox, "output", "personal_vs_headline_inflation.png"))
  # gtsave(..., ".png") needs webshot2 + Chrome; the app wraps it in try().
  if (requireNamespace("webshot2", quietly = TRUE)) {
    expect_valid_png(file.path(sandbox, "output", "latest_inflation_rates_table.png"), min_px = 10)
  }
})

test_that("committed output/ was not touched", {
  after <- tools::md5sum(names(real_output_md5))
  expect_identical(unname(after), unname(real_output_md5))
})
