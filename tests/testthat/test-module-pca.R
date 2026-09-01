# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: pca_module.R (PCA / UMAP)
# ─────────────────────────────────────────────────────────

pca_inputs <- function(session, int_cols, ...) {
  defaults <- list(
    dr_method = "PCA", row_selection = "all", id_selection = "",
    intensity_columns = int_cols,
    pca_center = TRUE, pca_scale = TRUE,
    color_scheme = "combined",
    pdf_width = 8, pdf_height = 6, point_size = 3, label_size = 3,
    loadings_top_n = 20,
    umap_n_neighbors = 3, umap_min_dist = 0.1, umap_n_components = 2
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("PCA returns one row per sample and variance sums to ~100%", {
  d <- sim_omics(n_features = 100)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    res <- pca_results()
    expect_equal(nrow(res$df), length(b$intensity_cols))
    expect_equal(sum(res$var_explained), 100, tolerance = 0.5)
  })
})

test_that("PCA sample labels match the selected intensity column names", {
  # dr_data() rebuilds the frame with as.data.frame(lapply(...)), which applies
  # make.names() and silently renames non-syntactic columns.
  d <- sim_awkward_colnames(n_features = 60)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    expect_setequal(pca_results()$df$Sample, b$intensity_cols)
  })
})

test_that("PCA drops features with missing values and still runs", {
  d <- sim_omics(n_features = 100, int_na_frac = 0.3, seed = 41)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    expect_lt(nrow(dr_data()), 100)
    expect_no_error(pca_results())
  })
})

test_that("PCA refuses to run with too few complete features", {
  d <- sim_omics(n_features = 2)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    expect_error(dr_data(), class = "shiny.silent.error")
  })
})

test_that("PCA refuses to run with a single sample", {
  d <- sim_omics(n_features = 50)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols[1])
    expect_error(pca_results(), class = "shiny.silent.error")
  })
})

test_that("PCA group annotations align with the samples on the plot", {
  d <- sim_omics(n_features = 80)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    # "Imputed.WT_01" splits to c("Imputed","WT","01","","") -> component 2 = group
    session$setInputs(group_component_2 = TRUE)
    res <- pca_results()
    expect_length(res$df$Group, length(b$intensity_cols))
    expect_setequal(unique(res$df$Group), c("WT", "KO", "TRT"))
  })
})

test_that("PCA group annotations survive combining several components", {
  d <- sim_omics(n_features = 80)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    session$setInputs(group_component_1 = TRUE, group_component_2 = TRUE,
                      group_component_3 = TRUE, group_component_4 = TRUE,
                      group_component_5 = TRUE)
    expect_no_error(group_annotations())
    expect_length(group_annotations(), length(b$intensity_cols))
  })
})

test_that("PCA plot, scree plot and loadings all render", {
  d <- sim_omics(n_features = 100)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    session$setInputs(pca_x_pc = "PC1", pca_y_pc = "PC2")
    expect_no_error(output$pca_plot)
    expect_no_error(output$scree_plot)
    expect_no_error(output$loadings_table)
  })
})

test_that("PCA loadings rows line up with the retained feature IDs", {
  d <- sim_omics(n_features = 100, int_na_frac = 0.2, seed = 43)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    session$setInputs(pca_x_pc = "PC1", pca_y_pc = "PC2")
    ld <- loadings_data()
    expect_equal(nrow(ld), nrow(dr_data()))
    expect_false(any(is.na(ld$Feature)))
    expect_true(all(ld$Feature %in% d$id))
  })
})

test_that("PCA coordinate export writes a CSV with one row per sample", {
  d <- sim_omics(n_features = 60)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    session$setInputs(pca_x_pc = "PC1", pca_y_pc = "PC2")
    csv <- utils::read.csv(output$download_coords)
    expect_equal(nrow(csv), length(b$intensity_cols))
    expect_true("Method" %in% names(csv))
  })
})

test_that("PCA PDF export writes a non-empty file", {
  d <- sim_omics(n_features = 60)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols)
    session$setInputs(pca_x_pc = "PC1", pca_y_pc = "PC2")
    expect_gt(file.size(output$download_pdf), 1000)
  })
})

test_that("PCA row selection by ID subsets the feature matrix", {
  d <- sim_omics(n_features = 100)
  b <- ov_bundle(d)
  picked <- d$id[1:20]
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols, row_selection = "selected",
               id_selection = paste(picked, collapse = ", "))
    expect_equal(nrow(dr_data()), 20L)
    expect_setequal(feature_ids(), picked)
  })
})

test_that("UMAP runs and produces two components", {
  d <- sim_omics(n_features = 100)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols, dr_method = "UMAP")
    u <- umap_results()
    expect_true(all(c("UMAP1", "UMAP2", "Sample", "Group") %in% names(u)))
    expect_equal(nrow(u), length(b$intensity_cols))
  })
})

test_that("UMAP rejects n_neighbors >= number of samples", {
  d <- sim_omics(n_features = 100)
  b <- ov_bundle(d)
  testServer(pca_server, args = list(data = reactive(b)), {
    pca_inputs(session, b$intensity_cols, dr_method = "UMAP",
               umap_n_neighbors = 99)
    expect_error(umap_results(), class = "shiny.silent.error")
  })
})

test_that("every colour palette produces a renderable plot", {
  d <- sim_omics(n_features = 60)
  b <- ov_bundle(d)
  for (scheme in c("combined", "ggplot", "set1", "set2", "okabe")) {
    testServer(pca_server, args = list(data = reactive(b)), {
      pca_inputs(session, b$intensity_cols, color_scheme = scheme)
      session$setInputs(pca_x_pc = "PC1", pca_y_pc = "PC2")
      expect_no_error(output$pca_plot)
    })
  }
})
