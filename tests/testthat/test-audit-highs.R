# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: the three High-severity audit findings
#   OV-CORR-01  swapFC() destroyed statistics for every comparison
#   OV-VIZ-02   UpSet presented fold-change-only sets as hits
#   OV-NUM-03   volcano -log10() on 0 / invalid adjusted p-values
# ─────────────────────────────────────────────────────────

# ══ OV-CORR-01 ═════════════════════════════════════════════════════════════

two_comparisons <- function() data.frame(
  id = c("P1", "P2"),
  logFC_A.over.B = c(1, -2), t_A.over.B = c(3, -6),
  P.Value_A.over.B = c(0.01, 0.02), adj.P.Val_A.over.B = c(0.02, 0.04),
  logFC_C.over.D = c(2, 5), t_C.over.D = c(4, 10),
  P.Value_C.over.D = c(0.03, 0.05), adj.P.Val_C.over.D = c(0.04, 0.06),
  stringsAsFactors = FALSE)

test_that("swapFC negates logFC and t, and leaves both p-values alone", {
  x <- two_comparisons()
  y <- swapFC(x, groups = "A.over.B")
  expect_equal(y$logFC_B.over.A,     -x$logFC_A.over.B)
  expect_equal(y$t_B.over.A,         -x$t_A.over.B)
  # a two-sided p-value is invariant under reversal of the contrast
  expect_equal(y$P.Value_B.over.A,    x$P.Value_A.over.B)
  expect_equal(y$adj.P.Val_B.over.A,  x$adj.P.Val_A.over.B)
})

test_that("swapFC leaves unselected comparisons byte-identical", {
  x <- two_comparisons()
  y <- swapFC(x, groups = "A.over.B")
  keep <- grep("C\\.over\\.D", names(x), value = TRUE)
  expect_true(all(keep %in% names(y)))
  expect_identical(y[keep], x[keep])
})

test_that("swapFC no longer deletes t_ and P.Value_ columns globally", {
  # the defect: swapping one comparison removed these for every comparison
  y <- swapFC(two_comparisons(), groups = "A.over.B")
  expect_true("t_C.over.D"       %in% names(y))
  expect_true("P.Value_C.over.D" %in% names(y))
  expect_true("t_B.over.A"       %in% names(y))
  expect_true("P.Value_B.over.A" %in% names(y))
})

test_that("swapFC is its own inverse", {
  x <- two_comparisons()
  expect_identical(swapFC(swapFC(x, "A.over.B"), "B.over.A"), x)
})

test_that("swapFC refuses to collide with an existing reverse comparison", {
  z <- data.frame(id = "P1", logFC_A.over.B = 1, logFC_B.over.A = 9)
  expect_error(swapFC(z, "A.over.B"), "collide")
})

test_that("swapFC reverses every comparison when groups is NULL", {
  y <- swapFC(two_comparisons(), groups = NULL)
  expect_true(all(c("logFC_B.over.A", "logFC_D.over.C") %in% names(y)))
  expect_length(grep("A\\.over\\.B|C\\.over\\.D", names(y)), 0)
})

test_that("swapFC ignores comparisons that are not x.over.y", {
  x <- data.frame(id = "P1", logFC_Contrast = 1, t_Contrast = 2)
  expect_identical(swapFC(x, groups = "Contrast"), x)
})


# ══ OV-VIZ-02 / OV-UX-14 ═══════════════════════════════════════════════════

no_partner <- function() data.frame(
  id = c("P1", "P2", "P3"),
  logFC_A.over.B = c(2, 1.5, 0),                      # no adj.P.Val partner
  logFC_C.over.D = c(2, 0, -2),
  adj.P.Val_C.over.D = c(0.01, 0.2, 0.01),
  stringsAsFactors = FALSE)

us <- function(session, ...) {
  d <- list(direction = "both", logfc_cutoff = 1, adjp_cutoff = 0.05,
            n_intersects = 40, min_set_size = 1, allow_fc_only = FALSE)
  do.call(session$setInputs, utils::modifyList(d, list(...)))
}

test_that("UpSet refuses to build sets when an adjusted-P partner is missing", {
  testServer(upset_plot_server, args = list(data = reactive(list(data = no_partner()))), {
    us(session)
    expect_error(membership_data(), class = "shiny.silent.error")
    # and the refusal names the offending column
    err <- tryCatch(membership_data(), shiny.silent.error = function(e) conditionMessage(e))
    expect_match(err, "adj.P.Val_", fixed = TRUE)
    expect_match(err, "logFC_A.over.B", fixed = TRUE)
  })
})

test_that("UpSet produces fold-change-only sets only when explicitly allowed", {
  testServer(upset_plot_server, args = list(data = reactive(list(data = no_partner()))), {
    us(session, allow_fc_only = TRUE)
    mem <- membership_data()
    # P1 and P2 clear |logFC| >= 1 with no significance test applied
    expect_setequal(mem$id[mem$logFC_A.over.B], c("P1", "P2"))
  })
})

test_that("UpSet requires a named, non-missing, unique id column", {
  base <- data.frame(logFC_A.over.B = c(2, 2), adj.P.Val_A.over.B = c(0.01, 0.01),
                     logFC_C.over.D = c(2, 2), adj.P.Val_C.over.D = c(0.01, 0.01),
                     stringsAsFactors = FALSE)

  no_id <- cbind(feature = c("P1", "P2"), base)         # first column, but not 'id'
  testServer(upset_plot_server, args = list(data = reactive(list(data = no_id))), {
    us(session); expect_error(membership_data(), class = "shiny.silent.error")
  })

  dup <- cbind(id = c("P1", "P1"), base)
  testServer(upset_plot_server, args = list(data = reactive(list(data = dup))), {
    us(session)
    err <- tryCatch(membership_data(), shiny.silent.error = function(e) conditionMessage(e))
    expect_match(err, "unique")
  })

  miss <- cbind(id = c("P1", NA), base)
  testServer(upset_plot_server, args = list(data = reactive(list(data = miss))), {
    us(session)
    err <- tryCatch(membership_data(), shiny.silent.error = function(e) conditionMessage(e))
    expect_match(err, "missing")
  })
})


# ══ OV-NUM-03 ══════════════════════════════════════════════════════════════

test_that("ov_neglog10_padj floors zero instead of returning Inf", {
  r <- ov_neglog10_padj(c(0, 1e-300, 0.05, 1))
  expect_true(all(is.finite(r$y)))
  expect_equal(r$n_invalid, 0L)
  expect_gt(r$y[1], r$y[2])                    # zero is still the most extreme
  expect_equal(r$y[4], 0)                      # p = 1 sits at zero
})

test_that("ov_neglog10_padj rejects values outside [0, 1] and counts them", {
  r <- ov_neglog10_padj(c(-0.01, 1.2, Inf, NaN, 0.05))
  expect_equal(r$n_invalid, 3L)                # negative, >1, Inf ... NaN is NA
  expect_true(is.na(r$y[1]) && is.na(r$y[2]) && is.na(r$y[3]))
  expect_true(is.finite(r$y[5]))
})

test_that("ov_neglog10_padj passes NA through without counting it invalid", {
  r <- ov_neglog10_padj(c(NA, 0.05))
  expect_true(is.na(r$y[1]))
  expect_equal(r$n_invalid, 0L)
})

test_that("a zero adjusted p-value still appears in the volcano", {
  # the defect: -log10(0) = Inf, and plotly drops non-finite points, so the
  # single most significant feature vanished from the plot
  d <- data.frame(id = c("TOP", "MID", "LOW"), Genes = c("A", "B", "C"),
                  logFC_KO.over.WT = c(4, 2, 0.1),
                  adj.P.Val_KO.over.WT = c(0, 1e-4, 0.9),
                  stringsAsFactors = FALSE)
  b <- ov_bundle(d)

  testServer(volcano_printer_server, args = list(data = reactive(b)), {
    session$setInputs(comparison_name = "KO.over.WT", pval_cutoff = 0.05,
                      logfc_cutoff = 1, label_columns = character(0),
                      label_only_sig = TRUE, id_selection = "", manual_axes = FALSE)
    pd <- plot_data()
    expect_true(is.finite(pd$.neglog10_padj[pd$id == "TOP"]))
    expect_equal(sum(is.finite(pd$.neglog10_padj)), 3L)
    # and it remains the highest point on the plot
    expect_equal(which.max(pd$.neglog10_padj), which(pd$id == "TOP"))
  })
})

test_that("invalid adjusted p-values are excluded from the volcano, not plotted", {
  d <- data.frame(id = c("NEG", "BIG", "OK"), Genes = c("A", "B", "C"),
                  logFC_KO.over.WT = c(2, 2, 2),
                  adj.P.Val_KO.over.WT = c(-0.01, 1.2, 0.001),
                  stringsAsFactors = FALSE)
  b <- ov_bundle(d)
  testServer(volcano_printer_server, args = list(data = reactive(b)), {
    session$setInputs(comparison_name = "KO.over.WT", pval_cutoff = 0.05,
                      logfc_cutoff = 1, label_columns = character(0),
                      label_only_sig = TRUE, id_selection = "", manual_axes = FALSE)
    pd <- plot_data()
    expect_true(is.na(pd$.neglog10_padj[pd$id == "NEG"]))
    expect_true(is.na(pd$.neglog10_padj[pd$id == "BIG"]))
    expect_true(is.finite(pd$.neglog10_padj[pd$id == "OK"]))
    # nor may they be counted as hits
    expect_false(any(pd$significant[pd$id %in% c("NEG", "BIG")]))
  })
})

test_that("no volcano module calls -log10 on an adjusted p-value directly", {
  # line by line: R's "." spans newlines, so a joined-string regex produces
  # false positives against the cutoff line -log10(input$pval_cutoff), which is
  # a legitimate use on a user-supplied threshold rather than on the data.
  for (f in c("volcano_plot_module.R", "volcano_printer_module.R")) {
    lines <- sub("#.*$", "", readLines(file.path(OV_ROOT, f), warn = FALSE))
    offending <- grep("-log10\\(", lines, value = TRUE)
    offending <- offending[!grepl("input\\$pval_cutoff", offending)]
    expect_length(offending, 0)
  }
})


# ══ A6 · OV-REP-04 — UMAP reproducibility ══════════════════════════════════

test_that("UMAP is reproducible for a fixed seed", {
  d <- sim_omics(n_features = 120, seed = 5); b <- ov_bundle(d)
  run <- function(seed) {
    out <- NULL
    testServer(pca_server, args = list(data = reactive(b)), {
      session$setInputs(dr_method = "UMAP", row_selection = "all", id_selection = "",
                        intensity_columns = b$intensity_cols, color_scheme = "combined",
                        point_size = 3, label_size = 3, pdf_width = 8, pdf_height = 6,
                        umap_n_neighbors = 3, umap_min_dist = 0.1,
                        umap_n_components = 2, umap_seed = seed)
      out <<- umap_results()
    })
    out
  }
  a <- run(42); b2 <- run(42)
  expect_equal(a$UMAP1, b2$UMAP1)
  expect_equal(a$UMAP2, b2$UMAP2)
})

test_that("the UMAP seed is recorded in the exported coordinates", {
  d <- sim_omics(n_features = 120, seed = 6); b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    session$setInputs(dr_method = "UMAP", row_selection = "all", id_selection = "",
                      intensity_columns = b$intensity_cols, color_scheme = "combined",
                      point_size = 3, label_size = 3, pdf_width = 8, pdf_height = 6,
                      umap_n_neighbors = 3, umap_min_dist = 0.1,
                      umap_n_components = 2, umap_seed = 7)
    csv <- utils::read.csv(output$download_coords)
    expect_true("umap_seed" %in% names(csv))
    expect_true(all(csv$umap_seed == 7))
  })
})


# ══ A7 · OV-NUM-08 — scaled PCA with a constant feature ════════════════════

test_that("a constant feature no longer takes down a scaled PCA", {
  # prcomp(scale. = TRUE) errors on a constant column; single-value imputation
  # upstream produces such features routinely
  d <- sim_omics(n_features = 60, seed = 8); b <- ov_bundle(d)
  d[1, b$intensity_cols] <- 17           # constant across every sample
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    session$setInputs(dr_method = "PCA", row_selection = "all", id_selection = "",
                      intensity_columns = b$intensity_cols, pca_center = TRUE,
                      pca_scale = TRUE, color_scheme = "combined", point_size = 3,
                      label_size = 3, pdf_width = 8, pdf_height = 6, loadings_top_n = 20)
    expect_no_error(pca_results())
    expect_equal(nrow(pca_results()$df), length(b$intensity_cols))
  })
})

test_that("an unscaled PCA still keeps constant features", {
  d <- sim_omics(n_features = 60, seed = 9); b <- ov_bundle(d)
  d[1, b$intensity_cols] <- 17
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    session$setInputs(dr_method = "PCA", row_selection = "all", id_selection = "",
                      intensity_columns = b$intensity_cols, pca_center = TRUE,
                      pca_scale = FALSE, color_scheme = "combined", point_size = 3,
                      label_size = 3, pdf_width = 8, pdf_height = 6, loadings_top_n = 20)
    expect_no_error(pca_results())
  })
})


# ══ A8 · OV-NUM-09 — heatmap z-score of a constant row ═════════════════════

test_that("a constant row z-scores to zero rather than NaN", {
  d <- sim_omics(n_features = 30, seed = 10); b <- ov_bundle(d)
  d[1, b$intensity_cols] <- 12           # constant
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    session$setInputs(id_selection = "", intensity_columns = b$intensity_cols,
                      row_label_columns = character(0), cluster_columns = FALSE,
                      cluster_rows = FALSE, scale_rows = TRUE,
                      use_custom_limits = FALSE, color_min = -1, color_max = 1,
                      pdf_width = 8, pdf_height = 6, fontsize_row = 8, fontsize_col = 8)
    m <- final_heatmap_data()$matrix
    expect_false(any(is.nan(m)))
    expect_true(all(m[1, ] == 0))
    # the varying rows are still standardised
    expect_true(all(abs(rowMeans(m[-1, , drop = FALSE])) < 1e-8))
  })
})

test_that("clustering still runs when a constant row is present", {
  d <- sim_omics(n_features = 30, seed = 11); b <- ov_bundle(d)
  d[1, b$intensity_cols] <- 12
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    session$setInputs(id_selection = "", intensity_columns = b$intensity_cols,
                      row_label_columns = character(0), cluster_columns = TRUE,
                      cluster_rows = TRUE, scale_rows = TRUE,
                      use_custom_limits = FALSE, color_min = -1, color_max = 1,
                      pdf_width = 8, pdf_height = 6, fontsize_row = 8, fontsize_col = 8)
    expect_no_error(output$heatmap_plot)
  })
})


# ══ A9 · OV-UX-18 — gene queries must match exactly ════════════════════════

test_that("regex metacharacters in a gene name match themselves and nothing else", {
  # the defect: "A+B" was compiled as a regex, matching AB and AAB while
  # missing A+B itself
  vec <- c("A+B", "AB", "AAB")
  expect_equal(unname(unlist(find_genes("A+B", vec))), 1L)

  for (g in c("A+B", "A.B", "A(B)", "A[B]", "A*B", "A?B", "A|B", "A^B", "A$B")) {
    v <- c(g, "OTHER", paste0("X;", g, ";Y"))
    hits <- unlist(find_genes(g, v))
    expect_setequal(unname(hits), c(1L, 3L))
  }
})

test_that("exact matching still respects token boundaries and separators", {
  vec <- c("TP53", "TP53BP1", "AAA;TP53;BBB", "BBB;TP53", "TP53;CCC")
  expect_setequal(unname(unlist(find_genes("TP53", vec))), c(1L, 3L, 4L, 5L))
})

test_that("exact matching still honours ignore.case and reports misses", {
  expect_true(is.na(unlist(find_genes("tp53", c("TP53")))))
  expect_equal(unname(unlist(find_genes("tp53", c("TP53"), ignore.case = TRUE))), 1L)
  expect_true(all(is.na(find_genes("NOPE", c("TP53", "EGFR"))[["NOPE"]])))
})

test_that("the ID List Generator finds identifiers containing metacharacters", {
  d <- sim_omics(n_features = 10, seed = 12)
  d$Genes[3] <- "A+B"
  b <- ov_bundle(d)
  testServer(id_list_generator_server, args = list(data = reactive(b)), {
    session$setInputs(gene_list = "A+B", search_column = "Genes",
                      remove_na = FALSE, ignore_case = FALSE)
    expect_equal(matched_ids(), d$id[3])
  })
})


# ══ A10 / A11 · wording and legacy .xls ════════════════════════════════════

test_that("the heatmap no longer describes z-scoring as normalisation", {
  html <- as.character(heatmap_ui("heatmap_module"))
  expect_false(grepl("normalize intensities", html, fixed = TRUE))
  expect_true(grepl("visualization only", html, fixed = TRUE))
  expect_true(grepl("should not replace", html, fixed = TRUE))
})

test_that("legacy .xls is rejected with an actionable message", {
  p <- tempfile(fileext = ".xls"); writeLines("not really an xls", p)
  expect_error(ov_read_upload(p, basename(p)), "not supported")
  expect_error(ov_read_upload(p, basename(p)), "save it as .xlsx", fixed = TRUE)
})

test_that(".xls is no longer offered in the file picker", {
  src <- paste(readLines(file.path(OV_ROOT, "app.R"), warn = FALSE), collapse = "\n")
  accept <- regmatches(src, regexpr('accept = c\\([^)]*\\)', src))
  expect_false(grepl('"\\.xls"', accept))
  expect_true(grepl('"\\.xlsx"', accept))
})
