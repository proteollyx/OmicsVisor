# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: one definition of "hit", used everywhere
#
# Audit finding OV-VIZ-07. Each module used to implement its own threshold
# check and they disagreed: UpSet used >= / <= while Volcano, Volcano Printer,
# Donut and the logFC Scatter used > / <. A feature sitting exactly on a cutoff
# was a hit in one view and not in another.
#
# The oracle test at the bottom is the one that matters: it compares every
# module's hit set against ov_is_hit() on the same data at cutoff - eps,
# cutoff, and cutoff + eps. Fixing the operators alone would work today and
# silently diverge the next time a module is added.
# ─────────────────────────────────────────────────────────

test_that("ov_is_hit is inclusive at the boundary by default", {
  # the interface says "adj.P <= 0.05", so the code must agree
  expect_true(ov_is_hit(1,   0.05, 1, 0.05))
  expect_true(ov_is_hit(-1,  0.05, 1, 0.05))
  expect_false(ov_is_hit(0.999, 0.05, 1, 0.05))
  expect_false(ov_is_hit(1, 0.0501, 1, 0.05))
})

test_that("ov_is_hit can be exclusive when asked", {
  expect_false(ov_is_hit(1, 0.05,  1, 0.05, inclusive = FALSE))
  expect_true(ov_is_hit(1.001, 0.049, 1, 0.05, inclusive = FALSE))
})

test_that("ov_is_hit never treats invalid input as a hit", {
  # NA, non-finite, and probabilities outside [0,1] are not hits, and the
  # result is never NA - callers index vectors with it
  bad_p    <- ov_is_hit(rep(2, 6), c(NA, NaN, Inf, -0.01, 1.4, 0.01), 1, 0.05)
  expect_equal(bad_p, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  bad_fc   <- ov_is_hit(c(NA, NaN, Inf, -Inf, 2), rep(0.01, 5), 1, 0.05)
  expect_equal(bad_fc, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_false(anyNA(ov_is_hit(c(NA, 1), c(NA, 0.01), 1, 0.05)))
})

test_that("ov_is_hit respects direction", {
  expect_equal(ov_is_hit(c(2, -2), c(.01, .01), 1, .05, direction = "up"),   c(TRUE, FALSE))
  expect_equal(ov_is_hit(c(2, -2), c(.01, .01), 1, .05, direction = "down"), c(FALSE, TRUE))
  expect_equal(ov_is_hit(c(2, -2), c(.01, .01), 1, .05, direction = "both"), c(TRUE, TRUE))
})

test_that("ov_is_hit handles empty and scalar-recycled input", {
  expect_length(ov_is_hit(numeric(0), numeric(0), 1, 0.05), 0)
  expect_equal(ov_is_hit(c(2, 0), 0.01, 1, 0.05), c(TRUE, FALSE))
})


# ── The oracle: every module must agree with ov_is_hit() ────────────────────

boundary_fixture <- function(fc_cut = 1, p_cut = 0.05, eps = 1e-6) {
  # one feature per interesting position relative to both cutoffs
  grid <- expand.grid(
    lfc  = c(fc_cut - eps, fc_cut, fc_cut + eps,
             -(fc_cut - eps), -fc_cut, -(fc_cut + eps), 0),
    padj = c(p_cut - eps, p_cut, p_cut + eps, 1e-12),
    KEEP.OUT.ATTRS = FALSE
  )
  d <- data.frame(
    id    = sprintf("P%03d", seq_len(nrow(grid))),
    Genes = sprintf("G%03d", seq_len(nrow(grid))),
    stringsAsFactors = FALSE
  )
  d$logFC_KO.over.WT     <- grid$lfc
  d$adj.P.Val_KO.over.WT <- grid$padj
  # a second, identical comparison so UpSet has the two sets it requires
  d$logFC_TRT.over.WT     <- grid$lfc
  d$adj.P.Val_TRT.over.WT <- grid$padj
  d
}

test_that("Volcano, Volcano Printer, Donut, Scatter and UpSet agree at the cutoff", {
  d  <- boundary_fixture()
  b  <- ov_bundle(d)
  fc <- 1; pc <- 0.05

  expected      <- d$id[ov_is_hit(d$logFC_KO.over.WT, d$adj.P.Val_KO.over.WT, fc, pc)]
  expected_up   <- d$id[ov_is_hit(d$logFC_KO.over.WT, d$adj.P.Val_KO.over.WT, fc, pc, direction = "up")]
  expected_down <- d$id[ov_is_hit(d$logFC_KO.over.WT, d$adj.P.Val_KO.over.WT, fc, pc, direction = "down")]

  # the fixture must actually exercise the boundary, or the test proves nothing
  expect_true(length(expected) > 0 && length(expected) < nrow(d))

  # --- Volcano: exported ID lists ---
  testServer(volcano_plot_server, args = list(data = reactive(b)), {
    session$setInputs(comparison_name = "KO.over.WT", pval_cutoff = pc,
                      logfc_cutoff = fc, label_columns = character(0),
                      generate_ids = 1)
    expect_setequal(id_lists$all, expected)
  })

  # --- Volcano Printer: the significance flag ---
  testServer(volcano_printer_server, args = list(data = reactive(b)), {
    session$setInputs(comparison_name = "KO.over.WT", pval_cutoff = pc,
                      logfc_cutoff = fc, label_columns = character(0),
                      label_only_sig = TRUE, id_selection = "", manual_axes = FALSE)
    pd <- plot_data()
    expect_setequal(pd$id[pd$significant], expected)
  })

  # --- Donut: up and down selections ---
  testServer(donut_plot_server, args = list(data = reactive(b)), {
    session$setInputs(logfc_cutoff = fc, pval_cutoff = pc, apply_cutoff = 1,
                      select_up_1 = TRUE, select_down_1 = FALSE)
    expect_setequal(selected_ids(), expected_up)
    session$setInputs(select_up_1 = FALSE, select_down_1 = TRUE)
    expect_setequal(selected_ids(), expected_down)
  })

  # --- logFC Scatter: the Significance factor ---
  testServer(scatterplot_server, args = list(data = reactive(b)), {
    session$setInputs(x_logfc = "logFC_KO.over.WT", y_logfc = "logFC_TRT.over.WT",
                      highlight_ids = "", label_all_ids = FALSE,
                      pval_cutoff = pc, logfc_cutoff = fc, point_size = 2,
                      label_size = 3, plot_width = 8, plot_height = 6,
                      lock_aspect = FALSE)
    sd <- scatter_data()
    expect_setequal(sd$id[sd$Significance != "None"], expected)
  })

  # --- UpSet: membership matrix (was the sole inclusive module) ---
  testServer(upset_plot_server, args = list(data = reactive(b)), {
    session$setInputs(direction = "both", logfc_cutoff = fc, adjp_cutoff = pc,
                      n_intersects = 40, min_set_size = 1)
    mem <- membership_data()
    expect_setequal(mem$id[mem$logFC_KO.over.WT], expected)
  })
})

test_that("UpSet up/down directions agree with ov_is_hit too", {
  d <- boundary_fixture(); b <- ov_bundle(d)
  for (dir in c("up", "down")) {
    want <- d$id[ov_is_hit(d$logFC_KO.over.WT, d$adj.P.Val_KO.over.WT,
                           1, 0.05, direction = dir)]
    testServer(upset_plot_server, args = list(data = reactive(b)), {
      session$setInputs(direction = dir, logfc_cutoff = 1, adjp_cutoff = 0.05,
                        n_intersects = 40, min_set_size = 1)
      mem <- membership_data()
      expect_setequal(mem$id[mem$logFC_KO.over.WT], want)
    })
  }
})

test_that("no module implements its own threshold comparison any more", {
  # a raw cutoff comparison outside helper_functions.R means a module has
  # drifted away from the shared rule again
  for (f in c("volcano_plot_module.R", "volcano_printer_module.R",
              "donut_plot_module.R", "scatterplot_module.R",
              "upset_plot_module.R")) {
    lines <- readLines(file.path(OV_ROOT, f), warn = FALSE)
    lines <- sub("#.*$", "", lines)          # live code only, not comments
    src   <- paste(lines, collapse = "\n")
    expect_false(grepl("input\\$pval_cutoff\\s*(<|<=)", src), info = f)
    expect_false(grepl("(>|>=)\\s*input\\$logfc_cutoff", src), info = f)
  }
})
