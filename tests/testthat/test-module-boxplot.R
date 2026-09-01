# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: boxplot_module.R (Box / Violin / Crossbar)
# ─────────────────────────────────────────────────────────

bp_inputs <- function(session, int_cols, id, ...) {
  defaults <- list(
    id_selection = id, intensity_columns = int_cols,
    plot_type = "box", error_type = "sd",
    show_jitter = FALSE, show_beeswarm = FALSE,
    manual_y = FALSE, y_min = 0, y_max = 30,
    pdf_width = 8, pdf_height = 6
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("boxplot pivots one feature into long form across the samples", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    bp_inputs(session, b$intensity_cols, d$id[1])
    long <- selected_data()
    expect_equal(nrow(long), length(b$intensity_cols))
    expect_setequal(long$Sample, b$intensity_cols)
    expect_true(all(long$id == d$id[1]))
  })
})

test_that("boxplot rejects more than one pasted ID", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    bp_inputs(session, b$intensity_cols,
              paste(d$id[1:2], collapse = ", "))
    expect_null(selected_ids())
  })
})

test_that("boxplot validates when the pasted ID is not in the data", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    bp_inputs(session, b$intensity_cols, "NO_SUCH_ID")
    expect_error(selected_data(), class = "shiny.silent.error")
  })
})

test_that("boxplot grouping works for any number of selected components", {
  # group_annotations() uses sapply(), which simplifies to a matrix once every
  # component returns a same-length vector. do.call(cbind, <matrix>) then fails
  # with "second argument must be a list".
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)

  for (k in 1:5) {
    testServer(plot_server, args = list(data = reactive(b)), {
      bp_inputs(session, b$intensity_cols, d$id[1])
      args <- stats::setNames(as.list(rep(TRUE, k)),
                              paste0("group_component_", seq_len(k)))
      do.call(session$setInputs, args)

      ann <- group_annotations()
      expect_length(ann, length(b$intensity_cols))
      expect_equal(nrow(selected_data()), length(b$intensity_cols))
    })
  }
})

test_that("boxplot groups replicates by the sample-name component", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    bp_inputs(session, b$intensity_cols, d$id[1])
    session$setInputs(group_component_2 = TRUE)   # "Imputed.WT_01" -> "WT"
    expect_setequal(unique(selected_data()$Group), c("WT", "KO", "TRT"))
  })
})

test_that("boxplot renders every plot type", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  for (pt in c("box", "violin", "cross")) {
    testServer(plot_server, args = list(data = reactive(b)), {
      # group_component_2 pools the three replicates of each condition, which
      # every plot type (violin included) needs.
      bp_inputs(session, b$intensity_cols, d$id[1], plot_type = pt)
      session$setInputs(group_component_2 = TRUE)
      expect_no_error(output$protein_plot)
    })
  }
})

test_that("violin explains itself instead of failing when groups are size 1", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    # No grouping selected -> Group == Sample -> one observation per group.
    bp_inputs(session, b$intensity_cols, d$id[1], plot_type = "violin")
    expect_error(output$protein_plot, class = "shiny.silent.error")
  })
})

test_that("boxplot crossbar renders for both SD and SEM", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  for (et in c("sd", "sem")) {
    testServer(plot_server, args = list(data = reactive(b)), {
      bp_inputs(session, b$intensity_cols, d$id[1],
                plot_type = "cross", error_type = et)
      session$setInputs(group_component_2 = TRUE)
      expect_no_error(output$protein_plot)
    })
  }
})

test_that("boxplot renders with jitter and with beeswarm overlays", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  for (ov in c("show_jitter", "show_beeswarm")) {
    testServer(plot_server, args = list(data = reactive(b)), {
      bp_inputs(session, b$intensity_cols, d$id[1])
      do.call(session$setInputs, stats::setNames(list(TRUE), ov))
      session$setInputs(group_component_2 = TRUE)
      expect_no_error(output$protein_plot)
    })
  }
})

test_that("boxplot handles a feature whose values are all NA", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  d[1, b$intensity_cols] <- NA_real_
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    bp_inputs(session, b$intensity_cols, d$id[1])
    expect_no_error(selected_data())
  })
})

test_that("boxplot PDF download writes a non-empty file", {
  d <- sim_omics(n_features = 30)
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    bp_inputs(session, b$intensity_cols, d$id[1])
    session$setInputs(group_component_2 = TRUE)
    expect_gt(file.size(output$download_plot), 1000)
  })
})

test_that("boxplot copes with duplicated IDs in the data", {
  d <- sim_duplicate_ids(n_features = 20)
  b <- ov_bundle(d)
  testServer(plot_server, args = list(data = reactive(b)), {
    bp_inputs(session, b$intensity_cols, d$id[1])
    session$setInputs(group_component_2 = TRUE)
    expect_no_error(selected_data())
  })
})
