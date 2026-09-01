# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: the app-level data pipeline in app.R
#
# app.R ends in shinyApp(), so it cannot be sourced in a test. These tests
# exercise the same logic through ov_read_upload() / ov_detect_columns(),
# which app.R now calls, guaranteeing the two stay in step.
# ─────────────────────────────────────────────────────────

test_that("xlsx, csv, tsv and txt uploads all yield the same table", {
  d <- sim_omics(n_features = 25)

  xlsx <- sim_write_xlsx(d)
  csv  <- tempfile(fileext = ".csv"); utils::write.csv(d, csv, row.names = FALSE)
  tsv  <- tempfile(fileext = ".tsv")
  utils::write.table(d, tsv, sep = "\t", row.names = FALSE, quote = FALSE)
  txt  <- tempfile(fileext = ".txt")
  utils::write.table(d, txt, sep = "\t", row.names = FALSE, quote = FALSE)

  for (p in c(xlsx, csv, tsv, txt)) {
    got <- ov_read_upload(p, basename(p))
    expect_equal(names(got), names(d), info = p)
    expect_equal(nrow(got), nrow(d),   info = p)
    expect_equal(got$id, d$id,         info = p)
    expect_equal(got$logFC_KO.over.WT, d$logFC_KO.over.WT,
                 tolerance = 1e-8, info = p)
  }
})

test_that("an unsupported extension is rejected with a readable message", {
  p <- tempfile(fileext = ".docx"); writeLines("x", p)
  expect_error(ov_read_upload(p, basename(p)), "Unsupported file type")
})

test_that("empty strings in delimited files are read as NA", {
  d <- sim_omics(n_features = 10)
  d$Genes[2] <- ""
  p <- tempfile(fileext = ".tsv")
  utils::write.table(d, p, sep = "\t", row.names = FALSE, quote = FALSE,
                     na = "")
  got <- ov_read_upload(p, basename(p))
  expect_true(is.na(got$Genes[2]))
})

test_that("column detection finds logFC, adj.P and intensity columns", {
  d   <- sim_omics(n_features = 10)
  got <- ov_detect_columns(d, "^Imputed")
  expect_length(got$logFC_cols, 3)
  expect_length(got$adjP_cols,  3)
  expect_length(got$intensity_cols, 9)
})

test_that("column detection returns empty vectors, not NULL, when nothing matches", {
  got <- ov_detect_columns(sim_no_comparisons(), "^Imputed")
  expect_length(got$logFC_cols, 0)
  expect_length(got$adjP_cols,  0)
  expect_identical(got$logFC_cols, character(0))

  got2 <- ov_detect_columns(sim_no_intensity(), "^Imputed")
  expect_length(got2$intensity_cols, 0)
})

test_that("column detection tolerates an invalid user regex", {
  d <- sim_omics(n_features = 10)
  expect_no_error(ov_detect_columns(d, "^Imputed["))   # unbalanced bracket
  expect_length(ov_detect_columns(d, "^Imputed[")$intensity_cols, 0)
})

test_that("column detection matches FDR and q-value naming variants", {
  d <- sim_omics(n_features = 10, comparisons = "KO.over.WT")
  names(d)[names(d) == "adj.P.Val_KO.over.WT"] <- "FDR_KO.over.WT"
  expect_true("FDR_KO.over.WT" %in% ov_detect_columns(d, "^Imputed")$adjP_cols)
})

test_that("the swap round-trip through the download handler preserves the data", {
  d   <- sim_omics(n_features = 20)
  out <- swapFC(d, groups = "KO.over.WT")

  p <- tempfile(fileext = ".txt")
  utils::write.table(out, p, sep = "\t", quote = FALSE, row.names = FALSE)
  back <- ov_read_upload(p, basename(p))

  expect_equal(back$logFC_WT.over.KO, -d$logFC_KO.over.WT, tolerance = 1e-6)
  expect_equal(back$id, d$id)
})

test_that("comparisons offered for swapping are the x.over.y ones only", {
  d <- sim_omics(n_features = 10,
                 comparisons = c("KO.over.WT", "TRT.over.WT"))
  d$logFC_Contrast <- 1                    # not an x.over.y name
  comps <- sub("^logFC_", "", grep("^logFC_", names(d), value = TRUE))
  comps <- sort(comps[grepl("\\.over\\.", comps)])
  expect_equal(comps, c("KO.over.WT", "TRT.over.WT"))
})
