# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: correlation_module.R
# ─────────────────────────────────────────────────────────

corr_inputs <- function(session, int_cols, ref, ...) {
  defaults <- list(
    ref_feature = ref, intensity_columns = int_cols,
    corr_method = "spearman", label_col = "Genes",
    r_threshold = 0.7, adjp_threshold = 0.1,
    point_size = 1.5, label_size = 2.5,
    pdf_width = 10, pdf_height = 6,
    run_corr = 1
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("correlation returns one row per feature with the reference flagged", {
  d <- sim_omics(n_features = 60)
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    res <- corr_results()
    expect_equal(nrow(res$results), 60L)
    expect_equal(sum(res$results$is_reference), 1L)
    expect_equal(res$ref_id, d$id[1])
  })
})

test_that("correlation of the reference against itself is exactly 1", {
  d <- sim_omics(n_features = 60)
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    res <- corr_results()$results
    expect_equal(res$r[res$is_reference], 1)
  })
})

test_that("correlation recovers a planted perfectly-correlated feature", {
  d <- sim_omics(n_features = 60, seed = 61)
  b <- ov_bundle(d)
  # Make feature 2 a monotone transform of feature 1 (Spearman r == 1).
  d[2, b$intensity_cols] <- d[1, b$intensity_cols] * 2 + 5
  b <- ov_bundle(d)

  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    res <- corr_results()$results
    expect_equal(res$r[res$id == d$id[2]], 1)
  })
})

test_that("correlation rejects a reference ID that is missing or ambiguous", {
  d <- sim_omics(n_features = 40)
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, "NO_SUCH_ID")
    expect_error(corr_results(), class = "shiny.silent.error")
  })

  dd <- sim_duplicate_ids(n_features = 20)
  bb <- ov_bundle(dd)
  testServer(correlation_server, args = list(data = reactive(bb)), {
    corr_inputs(session, bb$intensity_cols, dd$id[1])
    expect_error(corr_results(), class = "shiny.silent.error")
  })
})

test_that("correlation rejects a zero-variance reference feature", {
  d <- sim_omics(n_features = 40)
  b <- ov_bundle(d)
  d[1, b$intensity_cols] <- 17
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    expect_error(corr_results(), class = "shiny.silent.error")
  })
})

test_that("correlation requires at least three intensity columns", {
  d <- sim_omics(n_features = 40)
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols[1:2], d$id[1])
    expect_error(corr_results(), class = "shiny.silent.error")
  })
})

test_that("correlation handles a single-row dataset without crashing", {
  # apply(as.matrix(df[, cols]), 2, as.numeric) drops to a vector when the
  # frame has one row, so rownames(mat) <- df$id then errors.
  d <- sim_single_row()
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    # Either a clean validate() message or a result — never an uncaught error.
    res <- tryCatch(corr_results(),
                    shiny.silent.error = function(e) NULL)
    expect_true(is.null(res) || nrow(res$results) == 1L)
  })
})

test_that("correlation tolerates constant and all-NA features in the background", {
  d <- sim_omics(n_features = 60, seed = 62)
  b <- ov_bundle(d)
  d[5, b$intensity_cols] <- 12          # zero variance
  d[6, b$intensity_cols] <- NA_real_    # all missing
  b <- ov_bundle(d)

  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    res <- corr_results()$results
    expect_true(is.na(res$r[res$id == d$id[5]]))
    expect_true(is.na(res$r[res$id == d$id[6]]))
  })
})

test_that("correlation runs with both Spearman and Pearson", {
  d <- sim_omics(n_features = 50)
  b <- ov_bundle(d)
  for (m in c("spearman", "pearson")) {
    testServer(correlation_server, args = list(data = reactive(b)), {
      corr_inputs(session, b$intensity_cols, d$id[1], corr_method = m)
      expect_equal(corr_results()$method, m)
    })
  }
})

test_that("correlation results are sorted by descending |r| with NAs last", {
  d <- sim_omics(n_features = 60, seed = 63)
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    r <- abs(corr_results()$results$r)
    expect_equal(r[!is.na(r)], sort(r[!is.na(r)], decreasing = TRUE))
    expect_equal(corr_results()$results$rank, seq_len(60))
  })
})

test_that("correlation plot, table and downloads all work", {
  d <- sim_omics(n_features = 60)
  b <- ov_bundle(d)
  testServer(correlation_server, args = list(data = reactive(b)), {
    corr_inputs(session, b$intensity_cols, d$id[1])
    expect_no_error(output$corr_plot)
    expect_no_error(output$results_table)
    expect_gt(file.size(output$download_pdf), 1000)
    csv <- utils::read.csv(output$download_csv)
    expect_equal(nrow(csv), 60L)
    expect_false("is_reference" %in% names(csv))
  })
})
