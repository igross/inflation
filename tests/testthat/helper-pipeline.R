# Shared helpers: locate the repo, build a sandbox with synthetic inputs and run
# the pipeline scripts in it as separate Rscript processes (as CI does).

repo_root <- function() {
  # testthat runs with the working directory set to tests/testthat
  normalizePath(file.path(testthat::test_path(), "..", ".."))
}

synthetic_dir <- function() file.path(repo_root(), "tests", "synthetic")

source(file.path(synthetic_dir(), "generate_synthetic_data.R"), local = TRUE)

run_script_in_sandbox <- function(sandbox, script) {
  rscript <- file.path(R.home("bin"), "Rscript")
  log_file <- file.path(sandbox, paste0(basename(script), ".log"))
  env <- c(
    paste0("R_PROFILE_USER=", file.path(synthetic_dir(), "mock_profile.R")),
    paste0("CPI_SYNTH_GENERATOR=", file.path(synthetic_dir(), "generate_synthetic_data.R")),
    # Any accidental network access fails fast instead of reaching ABS.
    "http_proxy=http://127.0.0.1:9", "https_proxy=http://127.0.0.1:9",
    "HTTP_PROXY=http://127.0.0.1:9", "HTTPS_PROXY=http://127.0.0.1:9",
    "no_proxy=", "NO_PROXY=",
    paste0("R_READABS_PATH=", file.path(sandbox, "readabs-cache"))
  )
  old <- setwd(sandbox)
  on.exit(setwd(old), add = TRUE)
  status <- system2(rscript, script, env = env, stdout = log_file, stderr = log_file)
  list(status = status, log = readLines(log_file, warn = FALSE))
}

# Run the whole pipeline once per test session and cache the result.
pipeline_run <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      sandbox <- build_sandbox(repo_root())
      cache <<- list(
        sandbox = sandbox,
        analysis = run_script_in_sandbox(sandbox, "scripts/cpi_analysis.R"),
        components = run_script_in_sandbox(sandbox, "scripts/cpi_components.R")
      )
    }
    cache
  }
})

out_file <- function(name) file.path(pipeline_run()$sandbox, "output", name)

expect_nonempty_file <- function(path) {
  testthat::expect_true(file.exists(path), info = path)
  testthat::expect_gt(file.size(path), 0)
}

expect_valid_png <- function(path, min_px = 100) {
  expect_nonempty_file(path)
  img <- png::readPNG(path)
  testthat::expect_gte(dim(img)[1], min_px)
  testthat::expect_gte(dim(img)[2], min_px)
  # Not a blank canvas
  testthat::expect_gt(stats::sd(as.numeric(img[, , 1])), 0)
}
