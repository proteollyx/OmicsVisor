# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: what the About tab says about releases
#
# The About tab used to embed the whole CHANGELOG.md as raw markdown - 872
# lines carrying audit finding IDs and literal ## and ** - which accounted for
# about a fifth of every page load and told a bench scientist nothing they
# could act on. It now carries the two behaviour changes that actually alter
# results, the newest release section, and a link out for the rest.
#
# The behaviour-change list is hand-written, so these tests exist mainly to
# stop it drifting away from the changelog it summarises.
# ─────────────────────────────────────────────────────────

about_html <- function() as.character(about_ui("about"))
cl_path    <- function() {
  p <- testthat::test_path("..", "..", "CHANGELOG.md")
  skip_if_not(file.exists(p))
  p
}
changelog  <- function() readLines(cl_path(), warn = FALSE)
latest     <- function() ov_changelog_latest(cl_path())

test_that("only the newest release section is embedded", {
  latest <- latest()
  expect_type(latest, "character")
  # Exactly one version heading: the newest.
  expect_equal(length(gregexpr("^## \\[", latest)[[1]]), 1)
  expect_match(latest, "^## \\[")
})

test_that("the newest section matches the top of CHANGELOG.md", {
  cl <- changelog()
  first <- cl[grep("^## \\[", cl)[1]]
  expect_match(latest(), first, fixed = TRUE)
})

test_that("the whole changelog is no longer embedded in the page", {
  h <- about_html()
  cl <- changelog()
  heads <- grep("^## \\[", cl, value = TRUE)
  skip_if(length(heads) < 3, "not enough releases to distinguish")
  # The newest may appear; an older one must not.
  expect_false(grepl(heads[3], h, fixed = TRUE))
})

test_that("embedding the changelog no longer dominates the About tab", {
  # The reason for the change, stated as a number so it cannot quietly regress.
  cl_chars <- sum(nchar(changelog())) + length(changelog())
  expect_lt(nchar(about_html()), cl_chars)
})

test_that("both behaviour changes are stated with their versions", {
  h <- about_html()
  expect_match(h, "Changes that affect your results")
  expect_match(h, "v1\\.4\\.0")
  expect_match(h, "PCA no longer scales")
  expect_match(h, "v1\\.2\\.0")
  expect_match(h, "cutoffs are inclusive")
})

test_that("the versions named as behaviour changes exist in the changelog", {
  # The drift guard: renaming or renumbering a release must not leave the
  # About tab pointing at a version that no longer exists.
  cl <- paste(changelog(), collapse = "\n")
  for (v in c("1.4.0", "1.2.0"))
    expect_match(cl, paste0("## \\[", v, "\\]"), info = v)
})

test_that("the About tab links to the full changelog", {
  expect_match(about_html(), "CHANGELOG\\.md")
  expect_match(about_html(), "Full changelog on GitHub")
})

test_that("a missing changelog degrades to the link rather than erroring", {
  expect_null(ov_changelog_latest(file.path(withr::local_tempdir(), "nope.md")))
  expect_no_error(about_ui("about"))
})
