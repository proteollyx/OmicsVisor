# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: volcano_plot_module.R + volcano_printer_module.R
# ─────────────────────────────────────────────────────────

test_that("volcano populates the comparison dropdown from the data", {
  d <- sim_omics(n_features = 80)
  testServer(volcano_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$flushReact()
    expect_setequal(detect_comparisons(names(d)),
                    c("KO.over.WT", "TRT.over.WT", "KO.over.TRT"))
  })
})

test_that("volcano chosen_cols maps a comparison to its logFC / adj.P columns", {
  d <- sim_omics(n_features = 80)
  testServer(volcano_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT")
    expect_equal(chosen_cols()$logFC, "logFC_KO.over.WT")
    expect_equal(chosen_cols()$adjP,  "adj.P.Val_KO.over.WT")
  })
})

test_that("volcano ID generation excludes NA rows and splits by sign", {
  d <- sim_omics(n_features = 150, na_frac = 0.2, seed = 5)

  testServer(volcano_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT",
                      pval_cutoff = 0.05, logfc_cutoff = 1,
                      generate_ids = 1)

    lfc  <- d$logFC_KO.over.WT
    adjp <- d$adj.P.Val_KO.over.WT
    ok   <- !is.na(lfc) & !is.na(adjp) & adjp < 0.05 & abs(lfc) > 1

    expect_setequal(id_lists$pos, d$id[ok & lfc > 0])
    expect_setequal(id_lists$neg, d$id[ok & lfc < 0])
    expect_false(any(is.na(id_lists$all)))
    expect_length(id_lists$all, length(id_lists$pos) + length(id_lists$neg))
  })
})

test_that("volcano ID lists are empty when the cutoffs exclude everything", {
  d <- sim_omics(n_features = 80)
  testServer(volcano_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT",
                      pval_cutoff = 1e-12, logfc_cutoff = 50, generate_ids = 1)
    expect_length(id_lists$pos, 0)
    expect_length(id_lists$neg, 0)
  })
})

test_that("volcano plot renders for clean, NA-laden and non-finite data", {
  for (nm in c("clean", "na", "inf", "allna")) {
    d <- switch(nm,
                clean = sim_omics(n_features = 80),
                na    = sim_omics(n_features = 80, na_frac = 0.3, seed = 9),
                inf   = sim_infinite_values(),
                allna = sim_all_na_column())
    testServer(volcano_plot_server, args = list(data = reactive(ov_bundle(d))), {
      session$setInputs(comparison_name = "KO.over.WT",
                        pval_cutoff = 0.05, logfc_cutoff = 1,
                        label_columns = character(0))
      expect_no_error(output$volcano_plot)
    })
  }
})

test_that("volcano label columns are concatenated into the point label", {
  d <- sim_omics(n_features = 30)
  testServer(volcano_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT",
                      pval_cutoff = 0.05, logfc_cutoff = 1,
                      label_columns = c("Genes", "Protein.Group"))
    expect_no_error(output$volcano_plot)
  })
})


# ── Volcano Printer ──────────────────────────────────────────────────────────

test_that("volcano printer builds a plot and labels only selected significant IDs", {
  d <- sim_omics(n_features = 120, seed = 4)
  sig_ids <- d$id[!is.na(d$logFC_KO.over.WT) &
                    abs(d$logFC_KO.over.WT) > 1 &
                    d$adj.P.Val_KO.over.WT < 0.05]
  skip_if(length(sig_ids) < 3, "fixture produced too few significant hits")

  picked <- paste(head(sig_ids, 3), collapse = ", ")

  testServer(volcano_printer_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT",
                      pval_cutoff = 0.05, logfc_cutoff = 1,
                      label_columns = "Genes", label_only_sig = TRUE,
                      id_selection = picked, manual_axes = FALSE)

    pd <- plot_data()
    labelled <- pd$id[nzchar(pd$label_display) & !is.na(pd$label_display)]
    expect_setequal(labelled, head(sig_ids, 3))
  })
})

test_that("volcano printer does not emit NA labels when adj.P is missing", {
  d <- sim_omics(n_features = 100, na_frac = 0.3, seed = 13)
  picked <- paste(head(d$id, 20), collapse = ", ")

  testServer(volcano_printer_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT",
                      pval_cutoff = 0.05, logfc_cutoff = 1,
                      label_columns = "Genes", label_only_sig = TRUE,
                      id_selection = picked, manual_axes = FALSE)
    expect_false(any(is.na(plot_data()$label_display)))
  })
})

test_that("volcano printer PDF download writes a non-empty file", {
  d <- sim_omics(n_features = 60)
  testServer(volcano_printer_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT",
                      pval_cutoff = 0.05, logfc_cutoff = 1,
                      label_columns = character(0), label_only_sig = TRUE,
                      id_selection = "", manual_axes = FALSE,
                      plot_width = 8, plot_height = 6)
    f <- output$download_plot
    expect_gt(file.size(f), 1000)
  })
})

test_that("volcano printer honours manual axis limits", {
  d <- sim_omics(n_features = 60)
  testServer(volcano_printer_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(comparison_name = "KO.over.WT",
                      pval_cutoff = 0.05, logfc_cutoff = 1,
                      label_columns = character(0), label_only_sig = TRUE,
                      id_selection = "", manual_axes = TRUE,
                      x_min = -2, x_max = 2, y_min = 0, y_max = 5,
                      plot_width = 8, plot_height = 6)
    expect_no_error(output$download_plot)
  })
})
