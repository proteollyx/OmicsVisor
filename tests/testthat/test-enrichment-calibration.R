# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: 1D enrichment framing (audit OV-ENR-05)
#
# The module runs a competitive Wilcoxon rank-sum test, which assumes
# features vary independently. They do not - co-regulation is what makes a
# set worth testing - and the resulting p-values are badly anti-conservative.
# The module now says so, quoting measured numbers.
#
# These tests exist so the claim and the measurement cannot drift apart. The
# figures printed in the UI are checked against the committed output of
# validation/competitive_null_calibration.R; changing one without rerunning
# the other fails here.
# ─────────────────────────────────────────────────────────

calib <- function() {
  p <- testthat::test_path("..", "..", "validation",
                           "competitive_null_calibration.csv")
  testthat::skip_if_not(file.exists(p), "calibration results not present")
  utils::read.csv(p, stringsAsFactors = FALSE)
}

enrich_html <- function() {
  as.character(mod_pathway_1D_ui("enrich"))
}

test_that("the committed calibration is a complete, well-formed grid", {
  g <- calib()
  expect_setequal(unique(g$rho), c(0, 0.05, 0.1, 0.3, 0.5))
  expect_setequal(unique(g$size), c(20, 50, 200))
  expect_equal(nrow(g), 15)
  expect_true(all(g$type_I >= 0 & g$type_I <= 1))
  expect_true(all(g$n_sim >= 2000))
})

test_that("the test is calibrated when features really are independent", {
  # If this failed, the simulation itself would be suspect and every other
  # number in the table with it.
  g <- calib()
  indep <- g$type_I[g$rho == 0]
  expect_true(all(abs(indep - 0.05) < 4 * g$mcse[1]))
})

test_that("even slight correlation inflates the rejection rate severalfold", {
  g <- calib()
  expect_true(all(g$type_I[g$rho == 0.05] > 0.10))
  expect_true(all(g$type_I[g$rho == 0.30] > 0.30))
})

test_that("inflation grows with both correlation and set size", {
  # Both monotonicities are what the explanation in the UI claims.
  g <- calib()
  for (sz in unique(g$size)) {
    sub <- g[g$size == sz, ][order(g$rho[g$size == sz]), ]
    expect_true(all(diff(sub$type_I) > 0),
                info = paste("not monotone in rho at size", sz))
  }
  for (rh in setdiff(unique(g$rho), 0)) {
    sub <- g[g$rho == rh, ][order(g$size[g$rho == rh]), ]
    expect_true(all(diff(sub$type_I) > 0),
                info = paste("not monotone in size at rho", rh))
  }
})

test_that("the UI warns that these p-values are not a calibrated error rate", {
  h <- enrich_html()
  expect_match(h, "competitive")
  expect_match(h, "independent")
  expect_match(h, "understate the false discovery rate")
  expect_match(h, "ranking device")
  expect_match(h, "rank_biserial")
  expect_match(h, "CAMERA")
})

test_that("the figures quoted in the UI match the committed calibration", {
  g <- calib()
  h <- enrich_html()

  # shiny renders each tags$b() on its own line, so the figure and the text
  # that identifies it are separated by markup and whitespace.
  quoted <- function(tail_text) {
    pat <- paste0("([0-9]+)\u2013([0-9]+)%</b>\\s*", tail_text)
    m <- regmatches(h, regexec(pat, h, perl = TRUE))[[1]]
    expect_length(m, 3)
    as.integer(m[2:3])
  }
  observed <- function(rho) round(100 * range(g$type_I[g$rho == rho]))

  q05 <- quoted("at a within-set correlation of only 0\\.05")
  q30 <- quoted("at 0\\.3\\.")

  expect_equal(q05, observed(0.05))
  expect_equal(q30, observed(0.30))
})

test_that("the UI points at the script that produced the figures", {
  expect_match(enrich_html(), "competitive_null_calibration")
})
