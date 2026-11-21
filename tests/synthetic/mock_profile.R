# Used as R_PROFILE_USER for pipeline subprocesses under test.
# When a script calls library(readabs), replace readabs::read_abs (in both the
# attached package env and the namespace) with synthetic_read_abs(), so no ABS
# download is attempted. The production scripts are run unmodified.
local({
  generator <- Sys.getenv("CPI_SYNTH_GENERATOR")
  if (!nzchar(generator) || !file.exists(generator)) {
    stop("CPI_SYNTH_GENERATOR must point at tests/synthetic/generate_synthetic_data.R")
  }
  patch <- function(...) {
    env <- new.env()
    sys.source(generator, envir = env)
    fake <- env$synthetic_read_abs
    for (target in list(as.environment("package:readabs"), asNamespace("readabs"))) {
      if (exists("read_abs", envir = target, inherits = FALSE)) {
        unlockBinding("read_abs", target)
        assign("read_abs", fake, envir = target)
        lockBinding("read_abs", target)
      }
    }
    message("[synthetic] readabs::read_abs mocked")
  }
  setHook(packageEvent("readabs", "attach"), patch)
})
