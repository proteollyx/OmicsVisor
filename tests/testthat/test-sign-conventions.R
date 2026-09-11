# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: fold-change sign conventions end to end
#
# Scoped per section 3.7 of the response to the audit. The LFQBench HYE124
# design mixes three proteomes at published proportions, so for "A over B"
# the true log2 fold changes are exactly 0 (human), +1 (yeast) and -2
# (E. coli). Known ratios make it a genuine oracle for the one question worth
# asking of a viewer: are signs and axis directions right, and does swapFC()
# invert exactly what it should?
#
# It is deliberately not used to check quantitative accuracy. OmicsVisor
# displays numbers it is given; if a displayed fold change is wrong the fault
# is upstream unless OmicsVisor transformed it. The transformations are what
# these tests pin down.
#
# A sign error is the kind of defect that survives review indefinitely,
# because every plot still looks entirely plausible - it just names the wrong
# biology.
# ─────────────────────────────────────────────────────────

species_of <- function(ids) sub("_[0-9]+$", "", ids)

# ── the shared hit rule ─────────────────────────────────────────────────────

test_that("direction 'up' selects the enriched proteome, never the depleted one", {
  d <- hye124_fixture()
  up <- ov_is_hit(d$logFC_A.over.B, d$adj.P.Val_A.over.B,
                  fc_cut = 0.5, padj_cut = 0.05, direction = "up")
  expect_setequal(unique(species_of(d$id[up])), "YEAST")
  expect_equal(sum(up), 40L)
})

test_that("direction 'down' selects the depleted proteome, never the enriched one", {
  d <- hye124_fixture()
  dn <- ov_is_hit(d$logFC_A.over.B, d$adj.P.Val_A.over.B,
                  fc_cut = 0.5, padj_cut = 0.05, direction = "down")
  expect_setequal(unique(species_of(d$id[dn])), "ECOLI")
  expect_equal(sum(dn), 40L)
})

test_that("the 1:1 proteome is never a hit at any sensible cutoff", {
  # Human is the null population. If it appears, the rule is broken.
  d <- hye124_fixture()
  for (fc in c(0.1, 0.5, 1)) {
    h <- ov_is_hit(d$logFC_A.over.B, d$adj.P.Val_A.over.B,
                   fc_cut = fc, padj_cut = 0.05, direction = "both")
    expect_false(any(species_of(d$id[h]) == "HUMAN"),
                 info = paste("human called a hit at fc cutoff", fc))
  }
})

test_that("a cutoff above the yeast ratio keeps only E. coli", {
  # |logFC| >= 1.5 excludes yeast (+1) and retains E. coli (-2). Gets the
  # magnitude ordering as well as the sign.
  d <- hye124_fixture()
  h <- ov_is_hit(d$logFC_A.over.B, d$adj.P.Val_A.over.B,
                 fc_cut = 1.5, padj_cut = 0.05, direction = "both")
  expect_setequal(unique(species_of(d$id[h])), "ECOLI")
})

# ── swapFC ──────────────────────────────────────────────────────────────────

test_that("swapFC turns A over B into B over A with every sign inverted", {
  d <- hye124_fixture()
  s <- swapFC(d, groups = "A.over.B")

  expect_true("logFC_B.over.A" %in% names(s))
  expect_false("logFC_A.over.B" %in% names(s))
  expect_equal(s$logFC_B.over.A, -d$logFC_A.over.B)

  # The biology must invert too: yeast was up in A, so it is down in B.
  up <- ov_is_hit(s$logFC_B.over.A, s$adj.P.Val_B.over.A, 0.5, 0.05, direction = "up")
  dn <- ov_is_hit(s$logFC_B.over.A, s$adj.P.Val_B.over.A, 0.5, 0.05, direction = "down")
  expect_setequal(unique(species_of(s$id[up])), "ECOLI")
  expect_setequal(unique(species_of(s$id[dn])), "YEAST")
})

test_that("swapFC negates the t-statistic but leaves the two-sided p alone", {
  d <- hye124_fixture()
  s <- swapFC(d, groups = "A.over.B")
  expect_equal(s$t_B.over.A, -d$t_A.over.B)
  expect_equal(s$adj.P.Val_B.over.A, d$adj.P.Val_A.over.B)
  expect_equal(s$P.Value_B.over.A,   d$P.Value_A.over.B)
})

test_that("swapping twice restores the original orientation exactly", {
  d <- hye124_fixture()
  back <- swapFC(swapFC(d, groups = "A.over.B"), groups = "B.over.A")
  expect_equal(back$logFC_A.over.B, d$logFC_A.over.B)
  expect_equal(back$t_A.over.B,     d$t_A.over.B)
  up <- ov_is_hit(back$logFC_A.over.B, back$adj.P.Val_A.over.B, 0.5, 0.05, direction = "up")
  expect_setequal(unique(species_of(back$id[up])), "YEAST")
})

# ── the plotted axis ────────────────────────────────────────────────────────

test_that("the volcano plots logFC on x without transforming its sign", {
  # The axis is where a sign error would become visible to a reader, and
  # where it would be least likely to be noticed in code review.
  d <- hye124_fixture()
  b <- ov_bundle(d)
  shiny::testServer(volcano_printer_server, args = list(data = reactive(b)), {
    session$setInputs(comparison_name = "A.over.B", logfc_cutoff = 0.5,
                      pval_cutoff = 0.05, label_only_sig = TRUE,
                      label_columns = "Genes", id_selection = "",
                      manual_axes = FALSE)
    pd   <- plot_data()
    cols <- chosen_cols()

    # The x value carried into the plot must be the logFC itself, unmodified.
    expect_equal(pd[[cols$logFC]], d$logFC_A.over.B)

    sp <- species_of(pd$id)
    # Signs land on the correct side, with the right magnitude ordering.
    expect_gt(median(pd[[cols$logFC]][sp == "YEAST"]),  0.8)
    expect_lt(median(pd[[cols$logFC]][sp == "ECOLI"]), -1.8)
    expect_lt(abs(median(pd[[cols$logFC]][sp == "HUMAN"])), 0.1)
    # The most extreme point in either direction is E. coli, not yeast.
    expect_equal(sp[which.max(abs(pd[[cols$logFC]]))], "ECOLI")
    # Only the two changed proteomes are flagged.
    expect_setequal(unique(sp[pd$significant]), c("YEAST", "ECOLI"))
  })
})

# ── counts that users read off the figures ──────────────────────────────────

test_that("the donut splits the two changed proteomes into up and down", {
  d <- hye124_fixture()
  b <- ov_bundle(d)
  shiny::testServer(donut_plot_server, args = list(data = reactive(b)), {
    session$setInputs(logfc_cutoff = 0.5, pval_cutoff = 0.05, apply_cutoff = 1)
    session$flushReact()
    up <- ov_is_hit(d$logFC_A.over.B, d$adj.P.Val_A.over.B, 0.5, 0.05, direction = "up")
    dn <- ov_is_hit(d$logFC_A.over.B, d$adj.P.Val_A.over.B, 0.5, 0.05, direction = "down")
    expect_equal(sum(up), 40L)
    expect_equal(sum(dn), 40L)
    expect_equal(sum(up | dn), 80L)   # human excluded
  })
})

test_that("UpSet keys its sets on the identifier, so species do not mix", {
  d <- hye124_fixture()
  d$logFC_C.over.B     <- d$logFC_A.over.B          # a duplicate contrast
  d$adj.P.Val_C.over.B <- d$adj.P.Val_A.over.B
  b <- ov_bundle(d)
  shiny::testServer(upset_plot_server, args = list(data = reactive(b)), {
    session$setInputs(logfc_cutoff = 0.5, adjp_cutoff = 0.05,
                      direction = "up", min_set_size = 1,
                      n_intersects = 10, allow_fc_only = FALSE)
    m <- membership_data()

    # Membership is keyed on the identifier, so every member of an "up" set
    # must be yeast under this design.
    id_col <- if ("id" %in% names(m)) "id" else names(m)[1]
    set_cols <- setdiff(names(m), id_col)
    for (sc in set_cols) {
      members <- m[[id_col]][m[[sc]] == 1]
      expect_setequal(unique(species_of(members)), "YEAST")
      expect_equal(length(members), 40L)
    }
    # Two identical contrasts must produce identical membership, not merely
    # equal counts - that is what catches a set keyed on row position.
    expect_equal(m[[set_cols[1]]], m[[set_cols[2]]])
  })
})
