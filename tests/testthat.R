# Run with: Rscript tests/testthat.R   (from the repository root)
library(testthat)

args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
tests_dir <- if (length(script)) dirname(normalizePath(script)) else "tests"

res <- test_dir(file.path(tests_dir, "testthat"), reporter = ProgressReporter$new(show_praise = FALSE),
                stop_on_failure = TRUE)
