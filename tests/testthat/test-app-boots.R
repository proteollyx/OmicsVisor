# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: app.R itself builds
#
# The module tests exercise each server function in isolation. This one checks
# the thing they cannot: that app.R sources cleanly, that the UI renders, and
# that every module's UI function is actually wired into the tab strip. A
# module can be perfectly healthy and still be unreachable.
# ─────────────────────────────────────────────────────────

build_app <- function() {
  env <- new.env(parent = globalenv())
  captured <- NULL
  env$shinyApp <- function(ui, server, ...) {
    captured <<- list(ui = ui, server = server)
    invisible(NULL)
  }
  withr::with_dir(OV_ROOT, sys.source(file.path(OV_ROOT, "app.R"), envir = env))
  captured
}

test_that("app.R sources without starting a server and yields a UI + server", {
  app <- build_app()
  expect_false(is.null(app))
  expect_true(is.function(app$server))
  expect_true(inherits(app$ui, "shiny.tag") || inherits(app$ui, "shiny.tag.list"))
})

test_that("the rendered UI carries the version from version.R", {
  html <- as.character(build_app()$ui)
  expect_true(grepl(paste0("v", ov_version), html, fixed = TRUE))
})

test_that("every module is reachable from the tab strip", {
  html <- as.character(build_app()$ui)
  tabs <- c("Data Overview", "Volcano Plot", "Donut Plot", "UpSet Plot",
            "Heatmap", "PCA", "Volcano Printer", "logFC Scatter Plot",
            "VennDi", "ID List Generator", "Boxplot", "Regex Tool",
            "Correlation", "GCT Export", "1D Enrichment", "Documentation",
            "Disclaimer", "About")
  for (tab in tabs)
    expect_true(grepl(tab, html, fixed = TRUE), info = paste("missing tab:", tab))
})

test_that("every www asset the app references exists on disk", {
  # tags$head() content is injected at render time, so it is not present in
  # as.character(ui); read the src/href values straight out of app.R instead.
  src <- paste(readLines(file.path(OV_ROOT, "app.R"), warn = FALSE), collapse = "\n")

  # Match the quoted asset path directly rather than using a lookbehind:
  # PCRE requires fixed-length lookbehind, so "(?<=(src|href) = \")" is
  # rejected outright on some builds (it alternates 3 vs 4 characters). It also
  # missed aligned assignments such as `src   = "..."`.
  refs <- unique(gsub('"', '', unlist(regmatches(
    src, gregexpr('"[^"]+\\.(png|ico|webmanifest)"', src)
  ))))

  expect_gt(length(refs), 0)
  for (a in refs)
    expect_true(file.exists(file.path(OV_ROOT, "www", a)),
                info = paste("referenced in app.R but missing from www/:", a))
})

test_that("images referenced by modules exist in www/", {
  src <- paste(readLines(file.path(OV_ROOT, "app.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("omics_icon3.png", src, fixed = TRUE))
  expect_true(grepl("hedgehog_1DE.png", src, fixed = TRUE))
  expect_true(file.exists(file.path(OV_ROOT, "www", "hedgehog_1DE.png")))
})

test_that("all R sources parse", {
  files <- list.files(OV_ROOT, pattern = "\\.R$", full.names = TRUE)
  for (f in files) expect_no_error(parse(f))
})

test_that("no module redefines %||% (it belongs to helper_functions.R)", {
  # A local redefinition here is what silently broke the GCT gene-column
  # dropdown; keep the operator in exactly one place.
  files <- setdiff(list.files(OV_ROOT, pattern = "\\.R$", full.names = TRUE),
                   file.path(OV_ROOT, "helper_functions.R"))
  offenders <- Filter(function(f) {
    any(grepl("^\\s*`%\\|\\|%`\\s*<-", readLines(f, warn = FALSE)))
  }, files)
  expect_length(offenders, 0)
})
