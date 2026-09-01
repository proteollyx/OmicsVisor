# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: gct_export_module.R
# ─────────────────────────────────────────────────────────

gct_inputs <- function(session, metric_cols, ...) {
  defaults <- list(
    gene_col = "Genes", metric_cols = metric_cols, desc_col = "<none>",
    gene_clean_regex = ";.*$", method = "absmax",
    collapse_mode = "per_column", require_gene_nonempty = TRUE,
    outfile = "export_logFC_matrix.gct"
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("gct export collapses to one row per unique gene", {
  d <- sim_omics(n_features = 50)
  b <- ov_bundle(d)
  lf <- grep("^logFC_", names(d), value = TRUE)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, lf)
    cdf <- collapse_df()
    expect_equal(nrow(cdf), length(unique(d$Genes)))
    expect_true(all(lf %in% names(cdf)))
  })
})

test_that("gct export absmax keeps the value with the largest magnitude", {
  d <- sim_omics(n_features = 6, comparisons = "KO.over.WT")
  d$Genes <- c("A", "A", "A", "B", "B", "C")
  d$logFC_KO.over.WT <- c(1, -5, 2, 0.5, 0.2, 3)
  b <- ov_bundle(d)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, "logFC_KO.over.WT", method = "absmax")
    cdf <- collapse_df()
    expect_equal(cdf$logFC_KO.over.WT[cdf$gene == "A"], -5)
    expect_equal(cdf$logFC_KO.over.WT[cdf$gene == "B"],  0.5)
  })
})

test_that("gct export median and mean aggregate as advertised", {
  d <- sim_omics(n_features = 4, comparisons = "KO.over.WT")
  d$Genes <- c("A", "A", "A", "B")
  d$logFC_KO.over.WT <- c(1, 2, 6, 9)
  b <- ov_bundle(d)

  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, "logFC_KO.over.WT", method = "median")
    expect_equal(collapse_df()$logFC_KO.over.WT[1], 2)
  })
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, "logFC_KO.over.WT", method = "mean")
    expect_equal(collapse_df()$logFC_KO.over.WT[1], 3)
  })
})

test_that("gct export single_row mode takes every column from one protein", {
  d <- sim_omics(n_features = 4, comparisons = c("KO.over.WT", "TRT.over.WT"))
  d$Genes <- c("A", "A", "B", "B")
  d$logFC_KO.over.WT  <- c(1, -8, 0, 0)
  d$logFC_TRT.over.WT <- c(9,  0.5, 0, 0)
  b  <- ov_bundle(d)
  lf <- c("logFC_KO.over.WT", "logFC_TRT.over.WT")

  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, lf, collapse_mode = "single_row")
    a <- collapse_df()[collapse_df()$gene == "A", ]
    # Row 2 has the larger mean |value| (4.25 vs 5.0 -> row 1 wins at 5.0)
    expect_equal(unname(unlist(a[lf])), c(1, 9))
  })
})

test_that("gct export cleans gene names with the supplied regex", {
  d <- sim_omics(n_features = 3, comparisons = "KO.over.WT")
  d$Genes <- c("AAA;BBB", "AAA;CCC", "DDD")
  b <- ov_bundle(d)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, "logFC_KO.over.WT")
    expect_setequal(collapse_df()$gene, c("AAA", "DDD"))
  })
})

test_that("gct export drops rows with empty gene names when asked", {
  d <- sim_omics(n_features = 4, comparisons = "KO.over.WT")
  d$Genes <- c("AAA", "", NA, "BBB")
  b <- ov_bundle(d)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, "logFC_KO.over.WT", require_gene_nonempty = TRUE)
    expect_setequal(collapse_df()$gene, c("AAA", "BBB"))
  })
})

test_that("gct export drops rows with no finite value in any metric column", {
  # Inf is not finite, so an infinite-only row is dropped alongside NA-only
  # rows — a GCT matrix has no meaningful representation for either.
  d <- sim_omics(n_features = 4, comparisons = "KO.over.WT")
  d$Genes <- c("A", "B", "C", "D")
  d$logFC_KO.over.WT <- c(1, NA, Inf, 4)
  b <- ov_bundle(d)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, "logFC_KO.over.WT")
    expect_setequal(collapse_df()$gene, c("A", "D"))
  })
})

test_that("gct export writes a well-formed GCT v1.2 file", {
  d <- sim_omics(n_features = 40)
  b <- ov_bundle(d)
  lf <- grep("^logFC_", names(d), value = TRUE)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, lf)
    f <- output$dl_gct
    lines <- readLines(f)

    expect_equal(lines[1], "#1.2")
    dims <- as.integer(strsplit(lines[2], "\t")[[1]])
    expect_equal(dims, c(nrow(collapse_df()), length(lf)))

    hdr <- strsplit(lines[3], "\t")[[1]]
    expect_equal(hdr[1:2], c("Name", "Description"))
    expect_equal(hdr[-(1:2)], lf)
    expect_equal(length(lines), 3L + dims[1])
  })
})

test_that("gct export round-trips through the 1D enrichment GCT reader", {
  d <- sim_omics(n_features = 60)
  b <- ov_bundle(d)
  lf <- grep("^logFC_", names(d), value = TRUE)
  gct_path <- tempfile(fileext = ".gct")
  testServer(gct_export_server, args = list(data = reactive(b)), {
    gct_inputs(session, lf)
    file.copy(output$dl_gct, gct_path, overwrite = TRUE)
  })

  # Read it back with the parser used by the 1D Enrichment module.
  testServer(mod_pathway_1D_server, args = list(), {
    parsed <- read_gct(gct_path)
    expect_equal(colnames(parsed$data), lf)
    expect_true(is.numeric(parsed$data))
    expect_gt(nrow(parsed$data), 0)
  })
})

test_that("gene column dropdown offers only gene-like columns", {
  # The dropdown is built with `gene_like %||% nms`; a %||% that mis-handles a
  # multi-element left-hand side silently falls through to every column.
  d <- sim_omics(n_features = 10)
  d$PG.Genes <- d$Genes             # two gene-like columns, as in DIA-NN output
  b <- ov_bundle(d)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    ui  <- output$gene_col_ui
    txt <- paste(as.character(ui), collapse = " ")
    expect_true(grepl("PG.Genes", txt, fixed = TRUE))
    expect_false(grepl("Protein.Group", txt, fixed = TRUE))
  })
})

test_that("gct export reports a clear error when no logFC/t columns exist", {
  d <- sim_no_comparisons()
  b <- ov_bundle(d)
  testServer(gct_export_server, args = list(data = reactive(b)), {
    expect_error(output$metric_col_ui, class = "shiny.silent.error")
  })
})
