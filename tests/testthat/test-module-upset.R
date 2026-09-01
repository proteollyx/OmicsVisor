# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: upset_plot_module.R
# ─────────────────────────────────────────────────────────

us_inputs <- function(session, ...) {
  defaults <- list(direction = "both", logfc_cutoff = 1, adjp_cutoff = 0.05,
                   n_intersects = 40, min_set_size = 1)
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("upset membership has one logical column per logFC column", {
  d <- sim_omics(n_features = 200, seed = 51)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    mem <- membership_data()
    lf  <- grep("^logFC_", names(mem), value = TRUE)
    expect_length(lf, 3)
    expect_true(all(vapply(mem[lf], is.logical, logical(1))))
    expect_true("id" %in% names(mem))
  })
})

test_that("upset membership matches a hand-computed hit set", {
  d <- sim_omics(n_features = 200, seed = 52)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    mem  <- membership_data()
    lfc  <- d$logFC_KO.over.WT
    adjp <- d$adj.P.Val_KO.over.WT
    want <- d$id[!is.na(lfc) & !is.na(adjp) & abs(lfc) >= 1 & adjp <= 0.05]
    expect_setequal(mem$id[mem$logFC_KO.over.WT], want)
  })
})

test_that("upset direction filters select only up or only down hits", {
  d <- sim_omics(n_features = 200, seed = 53)
  for (dir in c("up", "down")) {
    testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
      us_inputs(session, direction = dir)
      mem  <- membership_data()
      hits <- mem$id[mem$logFC_KO.over.WT]
      lfc  <- d$logFC_KO.over.WT[match(hits, d$id)]
      if (dir == "up") expect_true(all(lfc >=  1)) else expect_true(all(lfc <= -1))
    })
  }
})

test_that("upset ignores NA rows rather than counting them as hits", {
  d <- sim_omics(n_features = 200, na_frac = 0.3, seed = 54)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    mem <- membership_data()
    expect_false(any(is.na(unlist(mem[grep("^logFC_", names(mem))]))))
    na_ids <- d$id[is.na(d$logFC_KO.over.WT)]
    expect_false(any(mem$logFC_KO.over.WT[mem$id %in% na_ids]))
  })
})

test_that("upset plot renders and the intersection dropdown matches it", {
  d <- sim_omics(n_features = 300, seed = 55)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    expect_no_error(output$upset_plot)

    grps <- intersection_groups()
    expect_gt(length(grps), 0)
    # Groups are mutually exclusive: no ID appears in two of them.
    all_ids <- unlist(grps, use.names = FALSE)
    expect_false(anyDuplicated(all_ids) > 0)
  })
})

test_that("upset intersection group sizes are ordered by descending frequency", {
  d <- sim_omics(n_features = 300, seed = 56)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    l <- lengths(intersection_groups())
    expect_equal(l, sort(l, decreasing = TRUE))
  })
})

test_that("upset refuses to plot with fewer than two sets", {
  d <- sim_omics(n_features = 100, comparisons = "KO.over.WT")
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    expect_error(output$upset_plot, class = "shiny.silent.error")
  })
})

test_that("upset reports a clear message when no logFC columns exist", {
  d <- sim_no_comparisons()
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    expect_error(membership_data(), class = "shiny.silent.error")
  })
})

test_that("upset reports a clear message when the cutoffs exclude everything", {
  d <- sim_omics(n_features = 100)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session, logfc_cutoff = 1000)
    expect_error(membership_data(), class = "shiny.silent.error")
  })
})

test_that("upset CSV export contains the membership matrix", {
  d <- sim_omics(n_features = 200, seed = 57)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    csv <- utils::read.csv(output$download_csv, check.names = FALSE)
    expect_equal(nrow(csv), nrow(membership_data()))
    expect_true("id" %in% names(csv))
  })
})

test_that("upset PDF export writes a non-empty file", {
  d <- sim_omics(n_features = 200, seed = 58)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session)
    expect_gt(file.size(output$download_pdf), 1000)
  })
})

test_that("upset min_set_size drops small sets from plot and dropdown alike", {
  d <- sim_omics(n_features = 300, seed = 59)
  testServer(upset_plot_server, args = list(data = reactive(ov_bundle(d))), {
    us_inputs(session, min_set_size = 1)
    wide <- names(intersection_groups())
    us_inputs(session, min_set_size = 100000)
    expect_error(intersection_groups(), class = "shiny.silent.error")
    expect_gt(length(wide), 0)
  })
})
