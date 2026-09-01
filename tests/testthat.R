# ─────────────────────────────────────────────────────────
# OmicsVisor - Test runner
# Author: Oliver Popp
#
# Run from the project root with:
#   Rscript tests/testthat.R
# or interactively with:
#   testthat::test_dir("tests/testthat")
# ─────────────────────────────────────────────────────────

library(testthat)

# Locate the project root (the directory containing app.R) regardless of the
# working directory the runner was invoked from.
find_root <- function(start = getwd()) {
  p <- normalizePath(start, mustWork = FALSE)
  for (i in 1:6) {
    if (file.exists(file.path(p, "app.R"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  stop("Could not locate the OmicsVisor project root (no app.R found).")
}

app_dir <- find_root()
Sys.setenv(OMICSVISOR_ROOT = app_dir)

testthat::test_dir(
  file.path(app_dir, "tests", "testthat"),
  env             = new.env(parent = globalenv()),
  stop_on_failure = TRUE
)
