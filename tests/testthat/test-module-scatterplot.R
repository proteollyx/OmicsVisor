# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: scatterplot_module.R (logFC Scatter Plot)
# ─────────────────────────────────────────────────────────

sp_inputs <- function(session, ...) {
  defaults <- list(
    x_logfc = "logFC_KO.over.WT", y_logfc = "logFC_TRT.over.WT",
    highlight_ids = "", label_all_ids = FALSE,
    pval_cutoff = 0.05, logfc_cutoff = 1,
    point_size = 2, label_size = 3,
    plot_width = 8, plot_height = 6, lock_aspect = FALSE
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("scatterplot classifies significance in each of the four categories", {
  d <- sim_omics(n_features = 200, seed = 21)
  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session)
    sd <- scatter_data()

    x_sig <- !is.na(d$adj.P.Val_KO.over.WT)  & d$adj.P.Val_KO.over.WT  < 0.05 &
             !is.na(d$logFC_KO.over.WT)      & abs(d$logFC_KO.over.WT) > 1
    y_sig <- !is.na(d$adj.P.Val_TRT.over.WT) & d$adj.P.Val_TRT.over.WT < 0.05 &
             !is.na(d$logFC_TRT.over.WT)     & abs(d$logFC_TRT.over.WT) > 1

    expect_setequal(unique(sd$Significance),
                    intersect(c("None", "Exp1", "Exp2", "Both"),
                              unique(sd$Significance)))
    expect_equal(sum(sd$Significance == "Both"), sum(x_sig & y_sig))
    expect_equal(sum(sd$Significance == "None"), sum(!x_sig & !y_sig))
  })
})

test_that("scatterplot survives NA-laden logFC / adj.P columns", {
  d <- sim_omics(n_features = 150, na_frac = 0.35, seed = 22)
  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session)
    sd <- scatter_data()
    expect_false(any(is.na(sd$Significance)))
    expect_no_error(output$scatter_plot)
  })
})

test_that("scatterplot treats NA-comparison rows as non-significant", {
  d <- sim_omics(n_features = 60, seed = 23)
  d$adj.P.Val_KO.over.WT[1:10] <- NA_real_
  d$logFC_KO.over.WT[11:20]    <- NA_real_

  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session)
    sd <- scatter_data()
    # Rows with a missing x-comparison must never be labelled Exp1 or Both.
    expect_false(any(sd$Significance[1:20] %in% c("Exp1", "Both")))
  })
})

test_that("scatterplot highlights only the requested IDs", {
  d <- sim_omics(n_features = 80)
  picked <- d$id[c(1, 5, 9)]
  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session, highlight_ids = paste(picked, collapse = ", "))
    sd <- scatter_data()
    expect_setequal(sd$id[nzchar(sd$Label)], picked)
  })
})

test_that("scatterplot renders with the same column on both axes", {
  d <- sim_omics(n_features = 60)
  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session, y_logfc = "logFC_KO.over.WT")
    expect_no_error(output$scatter_plot)
  })
})

test_that("scatterplot renders with a locked 1:1 aspect ratio", {
  d <- sim_omics(n_features = 60)
  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session, lock_aspect = TRUE)
    expect_no_error(output$scatter_plot)
  })
})

test_that("scatterplot PDF export writes a non-empty file", {
  d <- sim_omics(n_features = 60)
  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session)
    expect_gt(file.size(output$download_plot), 1000)
  })
})

test_that("scatterplot survives an all-NA comparison column", {
  d <- sim_all_na_column(n_features = 60)
  testServer(scatterplot_server, args = list(data = reactive(ov_bundle(d))), {
    sp_inputs(session)
    expect_no_error(scatter_data())
  })
})
