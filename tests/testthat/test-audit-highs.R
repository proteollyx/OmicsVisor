# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: the three High-severity audit findings
#   OV-CORR-01  swapFC() destroyed statistics for every comparison
#   OV-VIZ-02   UpSet presented fold-change-only sets as hits
#   OV-NUM-03   volcano -log10() on 0 / invalid adjusted p-values
# ─────────────────────────────────────────────────────────

# ══ OV-CORR-01 ═════════════════════════════════════════════════════════════

two_comparisons <- function() data.frame(
  id = c("P1", "P2"),
  logFC_A.over.B = c(1, -2), t_A.over.B = c(3, -6),
  P.Value_A.over.B = c(0.01, 0.02), adj.P.Val_A.over.B = c(0.02, 0.04),
  logFC_C.over.D = c(2, 5), t_C.over.D = c(4, 10),
  P.Value_C.over.D = c(0.03, 0.05), adj.P.Val_C.over.D = c(0.04, 0.06),
  stringsAsFactors = FALSE)

test_that("swapFC negates logFC and t, and leaves both p-values alone", {
  x <- two_comparisons()
  y <- swapFC(x, groups = "A.over.B")
  expect_equal(y$logFC_B.over.A,     -x$logFC_A.over.B)
  expect_equal(y$t_B.over.A,         -x$t_A.over.B)
  # a two-sided p-value is invariant under reversal of the contrast
  expect_equal(y$P.Value_B.over.A,    x$P.Value_A.over.B)
  expect_equal(y$adj.P.Val_B.over.A,  x$adj.P.Val_A.over.B)
})

test_that("swapFC leaves unselected comparisons byte-identical", {
  x <- two_comparisons()
  y <- swapFC(x, groups = "A.over.B")
  keep <- grep("C\\.over\\.D", names(x), value = TRUE)
  expect_true(all(keep %in% names(y)))
  expect_identical(y[keep], x[keep])
})

test_that("swapFC no longer deletes t_ and P.Value_ columns globally", {
  # the defect: swapping one comparison removed these for every comparison
  y <- swapFC(two_comparisons(), groups = "A.over.B")
  expect_true("t_C.over.D"       %in% names(y))
  expect_true("P.Value_C.over.D" %in% names(y))
  expect_true("t_B.over.A"       %in% names(y))
  expect_true("P.Value_B.over.A" %in% names(y))
})

test_that("swapFC is its own inverse", {
  x <- two_comparisons()
  expect_identical(swapFC(swapFC(x, "A.over.B"), "B.over.A"), x)
})

test_that("swapFC refuses to collide with an existing reverse comparison", {
  z <- data.frame(id = "P1", logFC_A.over.B = 1, logFC_B.over.A = 9)
  expect_error(swapFC(z, "A.over.B"), "collide")
})

test_that("swapFC reverses every comparison when groups is NULL", {
  y <- swapFC(two_comparisons(), groups = NULL)
  expect_true(all(c("logFC_B.over.A", "logFC_D.over.C") %in% names(y)))
  expect_length(grep("A\\.over\\.B|C\\.over\\.D", names(y)), 0)
})

test_that("swapFC ignores comparisons that are not x.over.y", {
  x <- data.frame(id = "P1", logFC_Contrast = 1, t_Contrast = 2)
  expect_identical(swapFC(x, groups = "Contrast"), x)
})


# ══ OV-VIZ-02 / OV-UX-14 ═══════════════════════════════════════════════════

no_partner <- function() data.frame(
  id = c("P1", "P2", "P3"),
  logFC_A.over.B = c(2, 1.5, 0),                      # no adj.P.Val partner
  logFC_C.over.D = c(2, 0, -2),
  adj.P.Val_C.over.D = c(0.01, 0.2, 0.01),
  stringsAsFactors = FALSE)

us <- function(session, ...) {
  d <- list(direction = "both", logfc_cutoff = 1, adjp_cutoff = 0.05,
            n_intersects = 40, min_set_size = 1, allow_fc_only = FALSE)
  do.call(session$setInputs, utils::modifyList(d, list(...)))
}

test_that("UpSet refuses to build sets when an adjusted-P partner is missing", {
  testServer(upset_plot_server, args = list(data = reactive(list(data = no_partner()))), {
    us(session)
    expect_error(membership_data(), class = "shiny.silent.error")
    # and the refusal names the offending column
    err <- tryCatch(membership_data(), shiny.silent.error = function(e) conditionMessage(e))
    expect_match(err, "adj.P.Val_", fixed = TRUE)
    expect_match(err, "logFC_A.over.B", fixed = TRUE)
  })
})

test_that("UpSet produces fold-change-only sets only when explicitly allowed", {
  testServer(upset_plot_server, args = list(data = reactive(list(data = no_partner()))), {
    us(session, allow_fc_only = TRUE)
    mem <- membership_data()
    # P1 and P2 clear |logFC| >= 1 with no significance test applied
    expect_setequal(mem$id[mem$logFC_A.over.B], c("P1", "P2"))
  })
})

test_that("UpSet requires a named, non-missing, unique id column", {
  base <- data.frame(logFC_A.over.B = c(2, 2), adj.P.Val_A.over.B = c(0.01, 0.01),
                     logFC_C.over.D = c(2, 2), adj.P.Val_C.over.D = c(0.01, 0.01),
                     stringsAsFactors = FALSE)

  no_id <- cbind(feature = c("P1", "P2"), base)         # first column, but not 'id'
  testServer(upset_plot_server, args = list(data = reactive(list(data = no_id))), {
    us(session); expect_error(membership_data(), class = "shiny.silent.error")
  })

  dup <- cbind(id = c("P1", "P1"), base)
  testServer(upset_plot_server, args = list(data = reactive(list(data = dup))), {
    us(session)
    err <- tryCatch(membership_data(), shiny.silent.error = function(e) conditionMessage(e))
    expect_match(err, "unique")
  })

  miss <- cbind(id = c("P1", NA), base)
  testServer(upset_plot_server, args = list(data = reactive(list(data = miss))), {
    us(session)
    err <- tryCatch(membership_data(), shiny.silent.error = function(e) conditionMessage(e))
    expect_match(err, "missing")
  })
})


# ══ OV-NUM-03 ══════════════════════════════════════════════════════════════

test_that("ov_neglog10_padj floors zero instead of returning Inf", {
  r <- ov_neglog10_padj(c(0, 1e-300, 0.05, 1))
  expect_true(all(is.finite(r$y)))
  expect_equal(r$n_invalid, 0L)
  expect_gt(r$y[1], r$y[2])                    # zero is still the most extreme
  expect_equal(r$y[4], 0)                      # p = 1 sits at zero
})

test_that("ov_neglog10_padj rejects values outside [0, 1] and counts them", {
  r <- ov_neglog10_padj(c(-0.01, 1.2, Inf, NaN, 0.05))
  expect_equal(r$n_invalid, 3L)                # negative, >1, Inf ... NaN is NA
  expect_true(is.na(r$y[1]) && is.na(r$y[2]) && is.na(r$y[3]))
  expect_true(is.finite(r$y[5]))
})

test_that("ov_neglog10_padj passes NA through without counting it invalid", {
  r <- ov_neglog10_padj(c(NA, 0.05))
  expect_true(is.na(r$y[1]))
  expect_equal(r$n_invalid, 0L)
})

test_that("a zero adjusted p-value still appears in the volcano", {
  # the defect: -log10(0) = Inf, and plotly drops non-finite points, so the
  # single most significant feature vanished from the plot
  d <- data.frame(id = c("TOP", "MID", "LOW"), Genes = c("A", "B", "C"),
                  logFC_KO.over.WT = c(4, 2, 0.1),
                  adj.P.Val_KO.over.WT = c(0, 1e-4, 0.9),
                  stringsAsFactors = FALSE)
  b <- ov_bundle(d)

  testServer(volcano_printer_server, args = list(data = reactive(b)), {
    session$setInputs(comparison_name = "KO.over.WT", pval_cutoff = 0.05,
                      logfc_cutoff = 1, label_columns = character(0),
                      label_only_sig = TRUE, id_selection = "", manual_axes = FALSE)
    pd <- plot_data()
    expect_true(is.finite(pd$.neglog10_padj[pd$id == "TOP"]))
    expect_equal(sum(is.finite(pd$.neglog10_padj)), 3L)
    # and it remains the highest point on the plot
    expect_equal(which.max(pd$.neglog10_padj), which(pd$id == "TOP"))
  })
})

test_that("invalid adjusted p-values are excluded from the volcano, not plotted", {
  d <- data.frame(id = c("NEG", "BIG", "OK"), Genes = c("A", "B", "C"),
                  logFC_KO.over.WT = c(2, 2, 2),
                  adj.P.Val_KO.over.WT = c(-0.01, 1.2, 0.001),
                  stringsAsFactors = FALSE)
  b <- ov_bundle(d)
  testServer(volcano_printer_server, args = list(data = reactive(b)), {
    session$setInputs(comparison_name = "KO.over.WT", pval_cutoff = 0.05,
                      logfc_cutoff = 1, label_columns = character(0),
                      label_only_sig = TRUE, id_selection = "", manual_axes = FALSE)
    pd <- plot_data()
    expect_true(is.na(pd$.neglog10_padj[pd$id == "NEG"]))
    expect_true(is.na(pd$.neglog10_padj[pd$id == "BIG"]))
    expect_true(is.finite(pd$.neglog10_padj[pd$id == "OK"]))
    # nor may they be counted as hits
    expect_false(any(pd$significant[pd$id %in% c("NEG", "BIG")]))
  })
})

test_that("no volcano module calls -log10 on an adjusted p-value directly", {
  # line by line: R's "." spans newlines, so a joined-string regex produces
  # false positives against the cutoff line -log10(input$pval_cutoff), which is
  # a legitimate use on a user-supplied threshold rather than on the data.
  for (f in c("volcano_plot_module.R", "volcano_printer_module.R")) {
    lines <- sub("#.*$", "", readLines(file.path(OV_ROOT, f), warn = FALSE))
    offending <- grep("-log10\\(", lines, value = TRUE)
    offending <- offending[!grepl("input\\$pval_cutoff", offending)]
    expect_length(offending, 0)
  }
})
