# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: version metadata consistency
#
# version.R is the single source of truth. These tests make it impossible for
# CHANGELOG.md, the git tags and the app header to drift apart again, which is
# how v1.0.0-v1.0.4 ended up documented in three different places (and tagged
# in none).
# ─────────────────────────────────────────────────────────

changelog_entries <- function() {
  path  <- file.path(OV_ROOT, "CHANGELOG.md")
  lines <- readLines(path, warn = FALSE)

  pat  <- "^##[[:space:]]*\\[([^]]+)\\][[:space:]]*-[[:space:]]*([0-9]{4}-[0-9]{2}-[0-9]{2})[[:space:]]*$"
  hits <- grep(pat, lines, value = TRUE, perl = TRUE)

  data.frame(
    version = sub(pat, "\\1", hits, perl = TRUE),
    date    = sub(pat, "\\2", hits, perl = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("version.R defines a semantic version and an ISO release date", {
  expect_true(exists("ov_version"))
  expect_true(exists("ov_release_date"))
  expect_match(ov_version, "^[0-9]+\\.[0-9]+\\.[0-9]+(-[A-Za-z0-9.]+)?$")
  expect_match(ov_release_date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  expect_false(is.na(as.Date(ov_release_date)))
})

test_that("CHANGELOG.md exists and uses Keep a Changelog headings", {
  expect_true(file.exists(file.path(OV_ROOT, "CHANGELOG.md")))
  expect_gt(nrow(changelog_entries()), 0)
})

test_that("the newest CHANGELOG entry matches version.R exactly", {
  top <- changelog_entries()[1, ]
  expect_equal(
    top$version, ov_version,
    info = paste0("CHANGELOG.md's newest entry is [", top$version,
                  "] but version.R says ", ov_version,
                  ". Update both together.")
  )
  expect_equal(
    top$date, ov_release_date,
    info = paste0("CHANGELOG.md dates ", top$version, " as ", top$date,
                  " but version.R says ", ov_release_date, ".")
  )
})

test_that("CHANGELOG versions are unique and in descending order", {
  e <- changelog_entries()
  expect_false(anyDuplicated(e$version) > 0)

  # Compare only the plain X.Y.Z releases; the pre-1.0 beta strings use a
  # four-component scheme that numeric_version cannot order against them.
  stable <- e$version[grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", e$version)]
  if (length(stable) > 1) {
    v <- numeric_version(stable)
    expect_true(all(diff(as.integer(rank(v))) < 0),
                info = "CHANGELOG entries must run newest-first.")
  }
})

test_that("README points at CHANGELOG.md rather than duplicating it", {
  readme <- readLines(file.path(OV_ROOT, "README.md"), warn = FALSE)
  # A "### v1.2.3" heading in the README is the start of a second, divergent
  # changelog — the exact drift this release cleaned up.
  expect_length(grep("^###\\s*v?[0-9]+\\.[0-9]+", readme), 0)
  expect_gt(length(grep("CHANGELOG.md", readme, fixed = TRUE)), 0)
})

test_that("a LICENSE file is present and matches what README claims", {
  lic <- file.path(OV_ROOT, "LICENSE")
  expect_true(file.exists(lic))
  txt <- paste(readLines(lic, warn = FALSE), collapse = " ")
  expect_match(txt, "MIT License")
  expect_match(txt, "Oliver Popp")
})

test_that("CITATION.cff carries the same version as version.R", {
  cff <- file.path(OV_ROOT, "CITATION.cff")
  skip_if_not(file.exists(cff), "CITATION.cff not present")
  lines <- readLines(cff, warn = FALSE)

  ver <- sub("^version:\\s*", "",
             grep("^version:", lines, value = TRUE)[1])
  ver <- gsub("[\"']", "", trimws(ver))
  expect_equal(ver, ov_version)

  rel <- sub("^date-released:\\s*", "",
             grep("^date-released:", lines, value = TRUE)[1])
  rel <- gsub("[\"']", "", trimws(rel))
  expect_equal(rel, ov_release_date)
})
