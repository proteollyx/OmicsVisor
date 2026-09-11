# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: renv.lock is actually restorable
#
# The locked-environment CI job found that renv.lock had been unrestorable for
# an unknown period. All five Bioconductor repository URLs carried the literal
# string "TRUE" where the version belongs, because R's etc/repositories
# templating produced that on the development machine and renv::snapshot()
# records getOption("repos") verbatim. renv::restore() failed outright with
# "invalid version specification 'TRUE'".
#
# Nothing detected it because no package here comes from Bioconductor, so the
# repositories were never consulted. These tests make the lockfile's usability
# checkable without a full restore, so the next occurrence fails in seconds
# rather than after a 90-minute CI job - or worse, silently.
# ─────────────────────────────────────────────────────────

lock <- function() {
  p <- testthat::test_path("..", "..", "renv.lock")
  skip_if_not(file.exists(p), "renv.lock not present")
  jsonlite::fromJSON(p, simplifyVector = FALSE)
}

test_that("every repository URL is well-formed and version-resolved", {
  l <- lock()
  urls <- vapply(l$R$Repositories, function(r) r$URL, character(1))
  expect_gt(length(urls), 0)
  for (u in urls) {
    expect_match(u, "^https?://", info = u)
    # The specific corruption: an unsubstituted or bogus version placeholder.
    expect_false(grepl("/TRUE/|/FALSE/|/NA/|%v|%bm", u), info = u)
  }
})

test_that("no repository is declared that no package comes from", {
  # A repository present but unused is where the rot hid: it was never
  # consulted, so its being broken cost nothing until a restore was attempted.
  l <- lock()
  names_declared <- vapply(l$R$Repositories, function(r) r$Name, character(1))
  used <- unique(vapply(l$Packages, function(p) p$Repository %||% "", character(1)))
  used <- used[nzchar(used)]
  expect_true(all(used %in% names_declared),
              info = paste("packages cite undeclared repositories:",
                           paste(setdiff(used, names_declared), collapse = ", ")))
  expect_setequal(names_declared, used)
})

test_that("no Bioconductor version is declared, or it is a real version", {
  # The defect the first fix missed. renv.lock carries a *top-level*
  # "Bioconductor": {"Version": ...} block, separate from the repository URLs,
  # and that is the field renv::restore() resolves. Cleaning only the URLs left
  # "Version": "TRUE" in place, so the restore failed identically the second
  # time. Nothing in the project needs Bioconductor, so the block should be
  # absent; if it is ever present it must be a version, not a flag.
  l <- lock()
  v <- l$Bioconductor$Version
  if (is.null(v)) { succeed(); return() }
  expect_match(as.character(v), "^[0-9]+\\.[0-9]+$",
               info = paste("Bioconductor$Version is", as.character(v)))
})

test_that("no lockfile field anywhere holds a bare TRUE where a version belongs", {
  # Stated as a whole-file invariant rather than field by field, because the
  # first version of this guard checked the two places I happened to think of
  # and the real one was a third.
  p <- testthat::test_path("..", "..", "renv.lock")
  skip_if_not(file.exists(p))
  txt <- readLines(p, warn = FALSE)
  offenders <- grep('"Version"\\s*:\\s*"(TRUE|FALSE|NA|NULL)"', txt, value = TRUE)
  expect_length(offenders, 0)
})

test_that("the lockfile declares an R version and a package set", {
  l <- lock()
  expect_match(l$R$Version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_gt(length(l$Packages), 50)
})

test_that("every package records a version and a source", {
  l <- lock()
  for (nm in names(l$Packages)) {
    p <- l$Packages[[nm]]
    expect_true(!is.null(p$Version) && nzchar(p$Version), info = nm)
    expect_true(!is.null(p$Source)  && nzchar(p$Source),  info = nm)
  }
})

test_that("renv can read the lockfile it is given", {
  skip_if_not_installed("renv")
  p <- testthat::test_path("..", "..", "renv.lock")
  l <- renv::lockfile_read(p)
  expect_equal(length(l$Packages), length(lock()$Packages))
  # The call that failed in CI resolves the repositories; if the lockfile
  # carries an unusable one, this is where it surfaces.
  expect_false(any(grepl("TRUE", unlist(l$R$Repositories))))
})

test_that("the test dependencies the suite needs are pinned", {
  # A locked environment that cannot run the tests is not much use.
  l <- lock()
  for (p in c("testthat", "withr", "jsonlite"))
    expect_true(p %in% names(l$Packages), info = p)
})

test_that("bioconductor.version is not set to a non-version in renv settings", {
  # The project-level half of the same defect: renv/settings.json carried
  # "bioconductor.version": "TRUE", which is what interpolated TRUE into the
  # repository URLs in the first place.
  p <- testthat::test_path("..", "..", "renv", "settings.json")
  skip_if_not(file.exists(p), "renv/settings.json not present")
  s <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  v <- s$`bioconductor.version`
  if (!is.null(v) && length(v) && nzchar(as.character(v)))
    expect_match(as.character(v), "^[0-9]+\\.[0-9]+$",
                 info = paste("bioconductor.version is", as.character(v)))
  else
    succeed()
})

test_that("the project pins CRAN explicitly rather than inheriting repos", {
  p <- testthat::test_path("..", "..", ".Rprofile")
  skip_if_not(file.exists(p), ".Rprofile not present")
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(txt, "cloud\\.r-project\\.org")
  expect_match(txt, "options\\(repos")
})
