# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: upload inspection and the fail-closed gate
#
# Audit finding OV-STAT-06 asked for input validation. The policy implemented
# here deliberately distinguishes two classes:
#
#   REJECT  values that cannot be true of the data they claim to be. An
#           adjusted p-value outside [0, 1] is not a probability; the realistic
#           cause is a mis-mapped column (a t-statistic read as adj.P), and
#           that mistake otherwise produces a completely plausible volcano.
#
#   REPORT  everything merely unusual. Duplicate ids, infinite fold changes
#           from all-or-nothing quantification, and adjusted p-values of
#           exactly 0 from permutation tests all occur in legitimate exports.
#           Refusing them would block real analyses.
#
# The split was checked against 29 real result files (one per research group)
# before being adopted: none of them tripped a reject condition.
# ─────────────────────────────────────────────────────────

clean_upload <- function() {
  data.frame(
    id                   = c("P1", "P2", "P3", "P4"),
    Genes                = c("AAA", "BBB", "CCC", "DDD"),
    Imputed.WT_01        = c(10, 11, 12, 13),
    Imputed.WT_02        = c(10, 11, 12, 14),
    Imputed.KO_01        = c(12, 11, 10,  9),
    logFC_KO.over.WT     = c(2, 0, -2, 0.5),
    adj.P.Val_KO.over.WT = c(0.001, 0.9, 0.01, 0.4),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

# ── the reject condition ─────────────────────────────────────────────────────

test_that("a clean table produces no fatal findings and no warnings", {
  r <- ov_inspect_upload(clean_upload())
  expect_length(r$fatal, 0)
  expect_length(r$warnings, 0)
})

test_that("adjusted p-values outside [0, 1] are fatal", {
  df <- clean_upload()
  df$adj.P.Val_KO.over.WT <- c(-4.2, 3.1, 0.5, 8.0)   # a t-statistic column
  r  <- ov_inspect_upload(df)
  expect_length(r$fatal, 1)
  expect_match(r$fatal[[1]], "KO.over.WT")
  expect_match(r$fatal[[1]], "0.*1|\\[0, 1\\]")
})

test_that("p-values at exactly 0 and exactly 1 are accepted, not rejected", {
  # Inclusive bounds: 0 arises from permutation tests, 1 from BH adjustment.
  df <- clean_upload()
  df$adj.P.Val_KO.over.WT <- c(0, 1, 0.5, 0.5)
  r  <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
})

test_that("only the offending comparison is named when several are present", {
  df <- clean_upload()
  df$logFC_B.over.A     <- c(1, 2, 3, 4)
  df$adj.P.Val_B.over.A <- c(0.1, 0.2, 0.3, 0.4)      # fine
  df$adj.P.Val_KO.over.WT <- c(-4.2, 3.1, 0.5, 8.0)   # broken
  r <- ov_inspect_upload(df)
  expect_length(r$fatal, 1)
  expect_match(r$fatal[[1]], "KO.over.WT")
  expect_false(grepl("B.over.A", r$fatal[[1]], fixed = TRUE))
})

# ── the report conditions: unusual, but never blocking ───────────────────────

test_that("duplicate ids warn but do not block", {
  df <- clean_upload()
  df$id[2] <- "P1"
  r <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
  expect_equal(r$id$n_duplicate, 1)
  expect_true(any(grepl("duplicate", r$warnings, ignore.case = TRUE)))
})

test_that("missing ids warn but do not block", {
  df <- clean_upload()
  df$id[3] <- NA_character_
  r <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
  expect_equal(r$id$n_missing, 1)
})

test_that("a missing id column warns but does not block", {
  df <- clean_upload()
  df$id <- NULL
  r <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
  expect_false(r$id$present)
  expect_true(any(grepl("id", r$warnings)))
})

test_that("infinite logFC warns but does not block", {
  # All-or-nothing quantification legitimately yields Inf after log-ratioing.
  df <- clean_upload()
  df$logFC_KO.over.WT[1] <- Inf
  r <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
  expect_true(any(grepl("infinite", r$warnings, ignore.case = TRUE)))
})

test_that("adjusted p-values of exactly zero are reported as a caveat", {
  df <- clean_upload()
  df$adj.P.Val_KO.over.WT[1] <- 0
  r <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
  expect_true(any(grepl("zero|exactly 0", r$warnings, ignore.case = TRUE)))
})

test_that("text in a numeric statistics column is coerced and reported", {
  df <- clean_upload()
  df$logFC_KO.over.WT <- c("2", "n.d.", "-2", "0.5")
  r  <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
  expect_true("logFC_KO.over.WT" %in% r$coerced)
  expect_true(any(grepl("coerce|numeric|text", r$warnings, ignore.case = TRUE)))
})

# ── what the panel reports ───────────────────────────────────────────────────

test_that("the report states the table shape and comparison count", {
  r <- ov_inspect_upload(clean_upload())
  expect_equal(r$rows, 4)
  expect_equal(r$cols, 7)
  expect_equal(r$n_comparisons, 1)
  expect_equal(r$comparisons$comparison, "KO.over.WT")
})

test_that("the report gives the observed adjusted p range per comparison", {
  r <- ov_inspect_upload(clean_upload())
  expect_equal(r$comparisons$padj_min, 0.001)
  expect_equal(r$comparisons$padj_max, 0.9)
})

test_that("intensity columns are counted and their missingness measured", {
  df <- clean_upload()
  df$Imputed.WT_01[1] <- NA
  r <- ov_inspect_upload(df, "^Imputed")
  expect_equal(r$intensity$n, 3)
  expect_equal(round(r$intensity$pct_missing, 2), round(100 / 12, 2))
})

test_that("when the intensity regex matches nothing, a prefix is suggested", {
  # The commonest real failure: an export using a prefix other than "Imputed".
  df <- data.frame(
    id = c("P1", "P2", "P3"),
    Ori.WT_01 = 1:3, Ori.WT_02 = 2:4, Ori.KO_01 = 3:5, Ori.KO_02 = 4:6,
    logFC_KO.over.WT = c(1, -1, 0),
    adj.P.Val_KO.over.WT = c(0.01, 0.2, 0.5),
    check.names = FALSE
  )
  r <- ov_inspect_upload(df, "^Imputed")
  expect_equal(r$intensity$n, 0)
  expect_true("Ori." %in% r$intensity$candidates)
  expect_length(r$fatal, 0)          # no intensity columns is not fatal
})

test_that("an invalid intensity regex is survived rather than thrown", {
  expect_silent(r <- ov_inspect_upload(clean_upload(), "^Imputed["))
  expect_equal(r$intensity$n, 0)
})

test_that("inspection works when id is not the first column", {
  df <- clean_upload()[, c(2, 3, 1, 4:7)]
  r  <- ov_inspect_upload(df)
  expect_true(r$id$present)
  expect_equal(r$id$n_unique, 4)
  expect_length(r$fatal, 0)
})

test_that("a table with no comparisons at all is described, not rejected", {
  df <- data.frame(id = c("P1", "P2"), Imputed.A_01 = 1:2, check.names = FALSE)
  r  <- ov_inspect_upload(df, "^Imputed")
  expect_length(r$fatal, 0)
  expect_equal(r$n_comparisons, 0)
  expect_null(r$comparisons)
})

test_that("an all-NA adjusted p column is described without error", {
  df <- clean_upload()
  df$adj.P.Val_KO.over.WT <- NA_real_
  r  <- ov_inspect_upload(df)
  expect_length(r$fatal, 0)
  expect_true(is.na(r$comparisons$padj_min))
})

# ── the panel ────────────────────────────────────────────────────────────────

test_that("the Data Overview panel reports shape, ids and comparisons", {
  df  <- clean_upload()
  rep <- reactiveVal(ov_inspect_upload(df, "^Imputed"))
  shiny::testServer(
    data_overview_server,
    args = list(data = reactive(list(data = df)), report = rep),
    {
      html <- as.character(output$upload_diagnosis$html)
      expect_match(html, "What was loaded")
      expect_match(html, "4 rows")
      expect_match(html, "4 unique")
      expect_match(html, "3 matched")
    }
  )
})

test_that("the panel surfaces warn-level findings as a visible list", {
  df <- clean_upload()
  df$id[2] <- "P1"
  df$logFC_KO.over.WT[1] <- Inf
  rep <- reactiveVal(ov_inspect_upload(df, "^Imputed"))
  shiny::testServer(
    data_overview_server,
    args = list(data = reactive(list(data = df)), report = rep),
    {
      html <- as.character(output$upload_diagnosis$html)
      expect_match(html, "1 duplicate")
      expect_match(html, "infinite", ignore.case = TRUE)
    }
  )
})

test_that("the panel states the two things the file cannot tell us", {
  # OV-REP-04 / OV-NUM-08: the app cannot verify the fold-change base or know
  # which test produced the p-values. Saying so is part of the contract.
  rep <- reactiveVal(ov_inspect_upload(clean_upload(), "^Imputed"))
  shiny::testServer(
    data_overview_server,
    args = list(data = reactive(list(data = clean_upload())), report = rep),
    {
      html <- as.character(output$upload_diagnosis$html)
      expect_match(html, "assumed log2")
      expect_match(html, "not supplied by the file")
    }
  )
})

test_that("the panel is absent rather than broken when no report is supplied", {
  shiny::testServer(
    data_overview_server,
    args = list(data = reactive(list(data = clean_upload()))),
    expect_null(output$upload_diagnosis)
  )
})
