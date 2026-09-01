# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: donut_plot_module.R
# ─────────────────────────────────────────────────────────

test_that("donut_plot builds for a normal dataset", {
  d <- sim_omics(n_features = 100)
  ids  <- d$id
  up   <- ids[d$logFC_KO.over.WT >  1 & d$adj.P.Val_KO.over.WT < 0.05]
  down <- ids[d$logFC_KO.over.WT < -1 & d$adj.P.Val_KO.over.WT < 0.05]
  build_gg(donut_plot(ids, down, up, "KO over WT"))
})

test_that("donut counts exclude NA logFC / adj.P values (regression, v1.0.4)", {
  # NA < 0.05 is NA; subsetting with NA inserts NA elements that length() counts.
  # Prior to v1.0.4 this inflated each donut by 2x the number of NA rows.
  d <- sim_omics(n_features = 100, na_frac = 0.2, seed = 7)

  testServer(donut_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(logfc_cutoff = 1, pval_cutoff = 0.05, apply_cutoff = 1)

    lfc  <- d$logFC_KO.over.WT
    adjp <- d$adj.P.Val_KO.over.WT
    valid <- !is.na(lfc) & !is.na(adjp)

    session$setInputs(select_up_1 = TRUE, select_down_1 = FALSE)
    expected_up <- d$id[valid & lfc > 1 & adjp < 0.05]

    got <- selected_ids()
    expect_false(any(is.na(got)))
    expect_setequal(got, expected_up)
  })
})

test_that("donut selection uses AND across comparisons and OR within one", {
  d <- sim_omics(n_features = 150, seed = 11)

  testServer(donut_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(logfc_cutoff = 1, pval_cutoff = 0.05, apply_cutoff = 1)

    sig <- function(cmp, dir) {
      lfc  <- d[[paste0("logFC_", cmp)]]
      adjp <- d[[paste0("adj.P.Val_", cmp)]]
      ok   <- !is.na(lfc) & !is.na(adjp) & adjp < 0.05
      if (dir == "up") d$id[ok & lfc > 1] else d$id[ok & lfc < -1]
    }

    # OR within comparison 1
    session$setInputs(select_up_1 = TRUE, select_down_1 = TRUE)
    expect_setequal(selected_ids(),
                    union(sig("KO.over.WT", "up"), sig("KO.over.WT", "down")))

    # AND across comparisons 1 and 2
    session$setInputs(select_up_1 = TRUE, select_down_1 = FALSE,
                      select_up_2 = TRUE, select_down_2 = FALSE)
    expect_setequal(selected_ids(),
                    intersect(sig("KO.over.WT",  "up"),
                              sig("TRT.over.WT", "up")))
  })
})

test_that("donut returns nothing when no checkbox is ticked", {
  d <- sim_omics(n_features = 50)
  testServer(donut_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(logfc_cutoff = 1, pval_cutoff = 0.05, apply_cutoff = 1)
    expect_length(selected_ids(), 0)
    expect_match(output$selected_ids_output, "No IDs meet")
    expect_match(output$selected_ids_count,  "Total Count: 0")
  })
})

test_that("donut copes with an all-NA comparison column", {
  d <- sim_all_na_column(n_features = 40)
  testServer(donut_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(logfc_cutoff = 1, pval_cutoff = 0.05, apply_cutoff = 1,
                      select_up_1 = TRUE)
    expect_length(selected_ids(), 0)
  })
})

test_that("donut copes with a dataset that has no comparisons at all", {
  d <- sim_no_comparisons()
  testServer(donut_plot_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(logfc_cutoff = 1, pval_cutoff = 0.05, apply_cutoff = 1)
    expect_length(selected_ids(), 0)
  })
})

test_that("donut segment counts sum to the total feature count", {
  d    <- sim_omics(n_features = 100, na_frac = 0.15, seed = 3)
  ids  <- d$id
  lfc  <- d$logFC_KO.over.WT
  adjp <- d$adj.P.Val_KO.over.WT
  ok   <- !is.na(lfc) & !is.na(adjp)
  up   <- ids[ok & lfc >  1 & adjp < 0.05]
  down <- ids[ok & lfc < -1 & adjp < 0.05]

  p  <- donut_plot(ids, down, up, "t")
  bd <- ggplot_build(p)
  expect_equal(length(up) + length(down) + (length(ids) - length(up) - length(down)),
               length(ids))
  expect_true(all(bd$data[[1]]$y >= 0))   # no negative "Other" slice
})
