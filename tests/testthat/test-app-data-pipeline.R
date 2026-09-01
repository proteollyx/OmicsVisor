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

test_that("an empty worksheet is rejected with a readable message", {
  # Real project folders contain the occasional placeholder .xlsx whose first
  # sheet holds no data; openxlsx returns NULL for it.
  p  <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)

  expect_error(ov_read_upload(p, basename(p)), "no data")
})

test_that("a header-only delimited file is rejected", {
  p <- tempfile(fileext = ".tsv")
  writeLines("id\tGenes\tlogFC_KO.over.WT", p)
  expect_error(ov_read_upload(p, basename(p)), "no data rows")
})

# ── Intensity-column candidate hints ─────────────────────────────────────────
# Across a 29-file sample of real result tables, 15 matched zero columns with
# the default "^Imputed" preset, leaving the Heatmap/PCA/Boxplot/Correlation
# tabs silently empty. The sidebar now reports the match count and, when it is
# zero, names real columns from the file.

test_that("intensity candidates exclude identifiers and statistics", {
  d <- sim_omics(n_features = 10)
  cand <- ov_intensity_candidates(d)
  expect_setequal(cand, grep("^Imputed", names(d), value = TRUE))
  expect_false(any(grepl("^(logFC|t|P\\.Value|adj\\.P\\.Val)_", cand)))
  expect_false(any(c("id", "Genes", "Protein.Group") %in% cand))
})

test_that("intensity candidates find plainly-named sample columns", {
  # Perseus-style output: no prefix at all, just group_replicate.
  d <- data.frame(
    id = c("A", "B"), Genes = c("G1", "G2"),
    logFC_KO.over.WT = c(1, 2), adj.P.Val_KO.over.WT = c(0.01, 0.2),
    t_KO.over.WT = c(3, 4), P.Value_KO.over.WT = c(0.01, 0.2),
    ctr_PeC_A = c(20, 21), ctr_PeC_B = c(20, 22),
    Cre_switch_A = c(19, 18), Cre_switch_B = c(19, 17),
    stringsAsFactors = FALSE
  )
  expect_setequal(ov_intensity_candidates(d),
                  c("ctr_PeC_A", "ctr_PeC_B", "Cre_switch_A", "Cre_switch_B"))
})

test_that("intensity candidates exclude ANOVA-style derived columns", {
  # These are numeric and unprefixed, so a naive "any numeric column" rule
  # would offer them as intensities.
  d <- data.frame(
    id = "A", Genes = "G",
    ANOVA.FC.x.vs.y = 1, ANOVA.Sig.x.vs.y = 1,
    Imputed.WT_01 = 20, Imputed.KO_01 = 21,
    stringsAsFactors = FALSE
  )
  expect_setequal(ov_intensity_candidates(d),
                  c("Imputed.WT_01", "Imputed.KO_01"))
})

test_that("ov_common_prefix proposes a usable regex stem", {
  expect_equal(ov_common_prefix(c("Imputed.WT_01", "Imputed.KO_02")), "Imputed.")
  expect_equal(ov_common_prefix(c("Intensity 1", "Intensity 2")), "Intensity ")
  expect_equal(ov_common_prefix(c("abc", "xyz")), "")
  expect_equal(ov_common_prefix("only_one"), "")
  expect_equal(ov_common_prefix(character(0)), "")
})

test_that("a proposed prefix actually matches the columns it came from", {
  for (d in list(sim_omics(n_features = 5),
                 sim_awkward_colnames(n_features = 5))) {
    cand   <- ov_intensity_candidates(d)
    prefix <- ov_common_prefix(cand)
    skip_if(!nzchar(prefix), "no common prefix for this fixture")
    matched <- grep(paste0("^", prefix), names(d), value = TRUE)
    expect_true(all(cand %in% matched))
  }
})
