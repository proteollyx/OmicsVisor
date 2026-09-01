# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: helper_functions.R
# ─────────────────────────────────────────────────────────

test_that("detect_comparisons pairs logFC_ with adj.P.Val_", {
  nms <- c("id", "Genes",
           "logFC_A.over.B", "t_A.over.B", "P.Value_A.over.B", "adj.P.Val_A.over.B",
           "logFC_C.over.D", "adj.P.Val_C.over.D",
           "logFC_E.over.F")                       # no adj.P.Val partner
  expect_equal(sort(detect_comparisons(nms)), c("A.over.B", "C.over.D"))
})

test_that("detect_comparisons returns character(0) when nothing matches", {
  expect_length(detect_comparisons(c("id", "Genes", "Imputed.WT_01")), 0)
})

test_that("detect_comparisons ignores non-.over. comparison names", {
  # These are legal names; the app must surface them, not silently drop them.
  nms <- c("logFC_ContrastA", "adj.P.Val_ContrastA")
  expect_equal(detect_comparisons(nms), "ContrastA")
})


# ── swapFC ───────────────────────────────────────────────────────────────────

test_that("swapFC negates logFC and flips the comparison name", {
  df <- sim_omics(n_features = 10, comparisons = "KO.over.WT")
  orig <- df$logFC_KO.over.WT

  out <- swapFC(df, groups = "KO.over.WT")

  expect_true("logFC_WT.over.KO"     %in% names(out))
  expect_true("adj.P.Val_WT.over.KO" %in% names(out))
  expect_false("logFC_KO.over.WT"    %in% names(out))
  expect_equal(out$logFC_WT.over.KO, -orig)
})

test_that("swapFC drops t_ and P.Value_ columns (invalidated by the flip)", {
  df  <- sim_omics(n_features = 10, comparisons = "KO.over.WT")
  out <- swapFC(df, groups = "KO.over.WT")
  expect_length(grep("^t_",       names(out)), 0)
  expect_length(grep("^P\\.Value_", names(out)), 0)
})

test_that("swapFC leaves unselected comparisons untouched", {
  df  <- sim_omics(n_features = 10,
                   comparisons = c("KO.over.WT", "TRT.over.WT"))
  out <- swapFC(df, groups = "KO.over.WT")

  expect_true("logFC_WT.over.KO"  %in% names(out))
  expect_true("logFC_TRT.over.WT" %in% names(out))
  expect_equal(out$logFC_TRT.over.WT, df$logFC_TRT.over.WT)
})

test_that("swapFC preserves adj.P.Val values (only the name changes)", {
  df  <- sim_omics(n_features = 10, comparisons = "KO.over.WT")
  out <- swapFC(df, groups = "KO.over.WT")
  expect_equal(out$adj.P.Val_WT.over.KO, df$adj.P.Val_KO.over.WT)
})

test_that("swapFC is a no-op when there are no stat columns", {
  df <- data.frame(id = letters[1:3], Genes = LETTERS[1:3])
  expect_identical(swapFC(df, groups = "anything"), df)
})

test_that("swapFC is a no-op for comparisons without .over.", {
  df  <- sim_non_over_comparisons(n_features = 5)
  out <- swapFC(df, groups = "ContrastA")
  expect_identical(out, df)
})

test_that("swapFC preserves NA values rather than turning them into 0", {
  df <- sim_omics(n_features = 10, comparisons = "KO.over.WT")
  df$logFC_KO.over.WT[c(2, 5)] <- NA_real_
  out <- swapFC(df, groups = "KO.over.WT")
  expect_true(all(is.na(out$logFC_WT.over.KO[c(2, 5)])))
})

test_that("swapFC applied twice returns the original logFC values", {
  df <- sim_omics(n_features = 10, comparisons = "KO.over.WT")
  orig <- df$logFC_KO.over.WT
  once  <- swapFC(df,   groups = "KO.over.WT")
  twice <- swapFC(once, groups = "WT.over.KO")
  expect_equal(twice$logFC_KO.over.WT, orig)
})


# ── find_genes ───────────────────────────────────────────────────────────────

test_that("find_genes matches whole tokens in semicolon-separated fields", {
  vec <- c("TP53", "TP53BP1", "AAA;TP53;BBB", "BBB;TP53", "TP53;CCC")
  idx <- unlist(find_genes("TP53", vec))
  expect_equal(sort(unname(idx)), c(1L, 3L, 4L, 5L))   # TP53BP1 must NOT match
})

test_that("find_genes returns NA for genes with no match", {
  res <- find_genes(c("TP53", "NOTAGENE"), c("TP53", "EGFR"))
  expect_true(all(is.na(res[["NOTAGENE"]])))
  expect_equal(unname(unlist(res[["TP53"]])), 1L)
})

test_that("find_genes honours ignore.case", {
  expect_true(is.na(unlist(find_genes("tp53", c("TP53")))))
  expect_equal(unname(unlist(find_genes("tp53", c("TP53"), ignore.case = TRUE))), 1L)
})


# ── read_gmt ─────────────────────────────────────────────────────────────────

test_that("read_gmt parses set names and drops the description field", {
  p    <- sim_write_gmt()
  sets <- read_gmt(p)
  expect_named(sets, c("SET_ALPHA", "SET_BETA", "SET_TINY"))
  expect_length(sets$SET_ALPHA, 30)
  expect_false(any(grepl("^desc of", unlist(sets))))
})


# ── parse_input_list (venndi_module.R) ───────────────────────────────────────

test_that("parse_input_list splits on every supported delimiter", {
  expected <- c("A", "B", "C")
  expect_equal(parse_input_list("A,B,C"),        expected)
  expect_equal(parse_input_list("A B C"),        expected)
  expect_equal(parse_input_list("A\nB\nC"),      expected)
  expect_equal(parse_input_list("A\r\nB\r\nC"),  expected)
  expect_equal(parse_input_list("A\tB\tC"),      expected)
  expect_equal(parse_input_list("A;B;C"),        expected)
  expect_equal(parse_input_list("A,,B  C"),      expected)
  expect_equal(parse_input_list("  A  ,  B  ,C"), expected)
})

test_that("parse_input_list handles empty and NULL input", {
  expect_length(parse_input_list(""),    0)
  expect_length(parse_input_list(NULL),  0)
  expect_length(parse_input_list("   "), 0)
})

test_that("parse_input_list de-duplicates", {
  expect_equal(parse_input_list("A,B,A,B,C"), c("A", "B", "C"))
})


# ── %||% ─────────────────────────────────────────────────────────────────────
# The app relies on %||% for defaulting. A vector-valued left-hand side must be
# returned as-is, not swapped for the fallback.

test_that("%||% returns the left-hand side when it is a non-empty vector", {
  expect_equal(c("Genes", "PG.Genes") %||% c("a", "b", "c"),
               c("Genes", "PG.Genes"))
  expect_equal("Genes" %||% "fallback", "Genes")
  expect_equal(40      %||% 99,          40)
  expect_equal(c(1, 2) %||% 99,          c(1, 2))
})

test_that("%||% falls back for NULL, empty and empty-string input", {
  expect_equal(NULL           %||% "fallback", "fallback")
  expect_equal(character(0)   %||% "fallback", "fallback")
  expect_equal(""             %||% "fallback", "fallback")
})
