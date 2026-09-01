# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: heatmap_module.R
# ─────────────────────────────────────────────────────────

hm_inputs <- function(session, int_cols, ...) {
  defaults <- list(
    id_selection = "", intensity_columns = int_cols,
    row_label_columns = character(0),
    cluster_columns = TRUE, cluster_rows = TRUE, scale_rows = FALSE,
    use_custom_limits = FALSE, color_min = -1, color_max = 1,
    pdf_width = 8, pdf_height = 6, fontsize_row = 8, fontsize_col = 8
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("heatmap builds a matrix of the expected shape", {
  d  <- sim_omics(n_features = 40)
  b  <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols)
    m <- final_heatmap_data()$matrix
    expect_equal(dim(m), c(40L, length(b$intensity_cols)))
    expect_true(is.numeric(m))
    expect_equal(rownames(m), d$id)
  })
})

test_that("heatmap filters rows by the pasted ID list", {
  d <- sim_omics(n_features = 40)
  b <- ov_bundle(d)
  picked <- d$id[c(2, 4, 6)]
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols,
              id_selection = paste(picked, collapse = ", "))
    expect_equal(rownames(final_heatmap_data()$matrix), picked)
  })
})

test_that("heatmap ID filter tolerates padded and unknown IDs", {
  d <- sim_omics(n_features = 40)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols,
              id_selection = sprintf("  %s , NOT_AN_ID ,%s ", d$id[1], d$id[3]))
    expect_setequal(rownames(final_heatmap_data()$matrix), d$id[c(1, 3)])
  })
})

test_that("heatmap z-score scaling centres each row on zero", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols, scale_rows = TRUE)
    m <- final_heatmap_data()$matrix
    expect_true(all(abs(rowMeans(m)) < 1e-8))
  })
})

test_that("heatmap coerces character intensity columns to numeric", {
  # Upstream exports sometimes write "NaN"/"Filtered" into the matrix, which
  # makes as.matrix() produce a character matrix. scale() then errors with
  # "'x' must be numeric or complex" outside of any tryCatch.
  d <- sim_character_intensities(n_features = 30)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols, scale_rows = TRUE,
              cluster_rows = FALSE, cluster_columns = FALSE)
    m <- final_heatmap_data()$matrix
    expect_true(is.numeric(m))
  })
})

test_that("heatmap clustering degrades gracefully with missing values", {
  d <- sim_omics(n_features = 30, int_na_frac = 0.6, seed = 31)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols)
    expect_no_error(final_heatmap_data())
    expect_no_error(output$heatmap_plot)
  })
})

test_that("heatmap renders with custom colour limits and rejects min >= max", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols, scale_rows = TRUE,
              use_custom_limits = TRUE, color_min = -2, color_max = 2)
    expect_length(color_breaks(), 101)

    session$setInputs(color_min = 2, color_max = -2)
    expect_error(color_breaks(), class = "shiny.silent.error")
  })
})

test_that("heatmap uses combined label columns for row names", {
  d <- sim_omics(n_features = 20)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols,
              row_label_columns = c("Genes", "Protein.Group"))
    expect_equal(rownames(final_heatmap_data()$matrix),
                 paste0(d$Genes, "_", d$Protein.Group))
  })
})

test_that("heatmap data download writes a matrix with row and column names", {
  d <- sim_omics(n_features = 20)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols)
    f   <- output$download_data
    tab <- utils::read.delim(f, check.names = FALSE, row.names = 1)
    expect_equal(ncol(tab), length(b$intensity_cols))
    expect_equal(nrow(tab), 20L)
  })
})

test_that("heatmap PDF download writes a non-empty file", {
  d <- sim_omics(n_features = 20)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols)
    expect_gt(file.size(output$download_pdf), 1000)
  })
})

test_that("heatmap reports a useful message when the ID filter matches nothing", {
  d <- sim_omics(n_features = 20)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols, id_selection = "NO_SUCH_ID")
    expect_equal(nrow(final_heatmap_data()$matrix), 0L)
    expect_error(output$heatmap_plot, class = "shiny.silent.error")
  })
})

test_that("heatmap handles a single selected feature", {
  d <- sim_omics(n_features = 20)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols, id_selection = d$id[1],
              cluster_rows = FALSE, cluster_columns = FALSE)
    expect_equal(nrow(final_heatmap_data()$matrix), 1L)
    expect_no_error(output$heatmap_plot)
  })
})

test_that("heatmap handles a single selected intensity column", {
  d <- sim_omics(n_features = 20)
  b <- ov_bundle(d)
  testServer(heatmap_server, args = list(data = reactive(b)), {
    hm_inputs(session, b$intensity_cols[1],
              cluster_rows = FALSE, cluster_columns = FALSE)
    expect_equal(ncol(final_heatmap_data()$matrix), 1L)
    expect_no_error(output$heatmap_plot)
  })
})
