# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: GCT 1.2 / 1.3 conformance (audit OV-ENR-12)
#
# The previous reader carried the comment "should theoretically support #1.3 -
# to be tested properly". It did not: column metadata rows sit between the
# column header and the data in GCT 1.3, and the reader took the data first.
# A 1.3 file that actually used column metadata therefore had those metadata
# rows parsed as features - named after the metadata fields, with all-NA
# scores - while an equal number of real features fell off the end. Because
# the enrichment code filters non-finite scores, the phantom features simply
# vanished and the loss was nearly invisible.
#
# The fixture list below is the one the audit specified.
# ─────────────────────────────────────────────────────────

# ── 1.2 ─────────────────────────────────────────────────────────────────────

test_that("GCT 1.2 without metadata reads with the right shape and values", {
  p <- write_gct_12(gct_path(), n_row = 6L, n_col = 3L)
  g <- ov_read_gct(p)
  expect_equal(g$version, "#1.2")
  expect_equal(dim(g$data), c(6L, 3L))
  expect_equal(rownames(g$data), sprintf("GENE%02d", 1:6))
  expect_equal(colnames(g$data), c("S1", "S2", "S3"))
  expect_equal(g$data[1, 1], 0.1)
  expect_equal(g$data[6, 3], 1.8)
  expect_length(g$warnings, 0)
})

test_that("GCT 1.2 keeps the Description column as row metadata", {
  g <- ov_read_gct(write_gct_12(gct_path()))
  expect_true("Description" %in% names(g$row_meta))
  expect_match(g$row_meta$Description[1], "^desc of")
})

# ── 1.3, all four metadata combinations ─────────────────────────────────────

test_that("GCT 1.3 with no metadata reads correctly", {
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 5L, n_col = 4L))
  expect_equal(g$version, "#1.3")
  expect_equal(dim(g$data), c(5L, 4L))
  expect_null(g$row_meta)
  expect_null(g$col_meta)
  expect_equal(g$data[1, 1], 0.1)
})

test_that("GCT 1.3 with row metadata only reads correctly", {
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 5L, n_col = 3L,
                                row_meta = c("Description", "chr")))
  expect_equal(dim(g$data), c(5L, 3L))
  expect_equal(names(g$row_meta), c("Description", "chr"))
  expect_equal(g$row_meta$chr[2], "chr_2")
  expect_null(g$col_meta)
  expect_equal(rownames(g$data), sprintf("GENE%02d", 1:5))
})

test_that("GCT 1.3 with column metadata only reads correctly", {
  # The case the old reader got wrong: without row metadata there is nothing
  # to misalign, but the metadata rows still precede the data.
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 5L, n_col = 3L,
                                col_meta = c("condition", "batch")))
  expect_equal(dim(g$data), c(5L, 3L))
  expect_equal(rownames(g$data), sprintf("GENE%02d", 1:5))
  expect_equal(names(g$col_meta), c("condition", "batch"))
  expect_equal(g$col_meta$condition, paste0("condition_", 1:3))
  expect_null(g$row_meta)
})

test_that("GCT 1.3 with both row and column metadata reads correctly", {
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 4L, n_col = 3L,
                                row_meta = c("Description", "chr"),
                                col_meta = c("condition", "batch")))
  expect_equal(dim(g$data), c(4L, 3L))
  expect_equal(rownames(g$data), sprintf("GENE%02d", 1:4))
  expect_equal(names(g$row_meta), c("Description", "chr"))
  expect_equal(names(g$col_meta), c("condition", "batch"))
  expect_equal(unname(g$data[1, ]), c(0.1, 0.5, 0.9))
})

test_that("column metadata rows are never mistaken for features", {
  # The regression guard for the defect itself, stated as an invariant.
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 4L, n_col = 3L,
                                row_meta = "Description",
                                col_meta = c("condition", "batch")))
  expect_false(any(c("condition", "batch") %in% rownames(g$data)))
  expect_equal(nrow(g$data), 4L)
  expect_false(any(is.na(g$data)))
})

# ── the malformed cases ─────────────────────────────────────────────────────

test_that("duplicate row identifiers warn and explain the consequence", {
  ids <- c("GENE01", "GENE01", "GENE03", "GENE04")
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 4L, n_col = 3L, ids = ids))
  expect_length(g$warnings, 1)
  expect_match(g$warnings[[1]], "duplicate row identifier")
  expect_match(g$warnings[[1]], "first occurrence")
  expect_equal(nrow(g$data), 4L)     # reported, not silently dropped
})

test_that("non-numeric score fields become NA and are counted", {
  v <- matrix(sprintf("%.3f", 1:12 / 10), 4, 3)
  v[2, 2] <- "n.d."; v[3, 1] <- "NaN-ish"
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 4L, n_col = 3L, values = v))
  expect_true(any(grepl("non-numeric", g$warnings)))
  expect_match(paste(g$warnings, collapse = " "), "2 non-numeric")
  expect_true(is.na(g$data[2, 2]))
  expect_true(is.na(g$data[3, 1]))
  expect_false(is.na(g$data[1, 1]))
})

test_that("a declared row count larger than the file is rejected", {
  p <- write_gct_13(gct_path(), n_row = 4L, n_col = 3L, declared_rows = 10L)
  expect_error(ov_read_gct(p), "declares 10 data row")
})

test_that("a declared row count smaller than the file is rejected", {
  p <- write_gct_13(gct_path(), n_row = 6L, n_col = 3L, declared_rows = 2L)
  expect_error(ov_read_gct(p), "declares 2 data row")
})

test_that("blank sample names are named rather than left empty", {
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 4L, n_col = 3L,
                                sample_names = c("S1", "", "S3")))
  expect_true(any(grepl("blank", g$warnings)))
  expect_true(all(nzchar(colnames(g$data))))
  expect_equal(colnames(g$data)[2], "sample_2")
})

test_that("duplicate sample names are made unique and reported", {
  g <- ov_read_gct(write_gct_13(gct_path(), n_row = 4L, n_col = 3L,
                                sample_names = c("S1", "S1", "S3")))
  expect_true(any(grepl("Duplicate sample name", g$warnings)))
  expect_false(anyDuplicated(colnames(g$data)) > 0)
})

test_that("a column header inconsistent with the declared dimensions is rejected", {
  p <- gct_path()
  writeLines(c("#1.3", "2\t3\t0\t0", "id\tS1\tS2",      # 2 samples, 3 declared
               "GENE01\t1\t2\t3", "GENE02\t4\t5\t6"), p)
  expect_error(ov_read_gct(p), "column header has")
})

test_that("a short data row is rejected with its line number", {
  p <- gct_path()
  writeLines(c("#1.3", "2\t3\t0\t0", "id\tS1\tS2\tS3",
               "GENE01\t1\t2\t3", "GENE02\t4\t5"), p)
  expect_error(ov_read_gct(p), "data row 2 has 3 fields but 4")
})

# ── version handling ────────────────────────────────────────────────────────

test_that("an unsupported or missing version header is rejected clearly", {
  p <- gct_path(); writeLines(c("#1.4", "2\t2\t0\t0", "id\tA\tB",
                               "G1\t1\t2", "G2\t3\t4"), p)
  expect_error(ov_read_gct(p), "expected #1.2 or #1.3")

  q <- gct_path(); writeLines(c("Name\tA\tB", "G1\t1\t2"), q)
  expect_error(ov_read_gct(q), "expected #1.2 or #1.3")
})

test_that("a version line with trailing tabs is still recognised", {
  # Real writers pad the version line; an exact string comparison used to
  # send such a file down the wrong branch entirely.
  p <- gct_path()
  writeLines(c("#1.2\t\t", "2\t2", "Name\tDescription\tA\tB",
               "G1\td1\t1\t2", "G2\td2\t3\t4"), p)
  g <- ov_read_gct(p)
  expect_equal(g$version, "#1.2")
  expect_equal(dim(g$data), c(2L, 2L))
})

test_that("an empty or truncated file is rejected rather than mis-parsed", {
  p <- gct_path(); file.create(p)
  expect_error(ov_read_gct(p), "Empty GCT file")

  q <- gct_path(); writeLines(c("#1.3", "4\t3\t0\t0"), q)
  expect_error(ov_read_gct(q), "Truncated GCT file")
})

test_that("a matrix with no readable numbers is rejected", {
  v <- matrix("n.d.", 4, 3)
  p <- write_gct_13(gct_path(), n_row = 4L, n_col = 3L, values = v)
  expect_error(ov_read_gct(p), "No numeric values")
})

test_that("trailing blank lines do not count as data rows", {
  p <- write_gct_13(gct_path(), n_row = 4L, n_col = 3L)
  cat("\n\n", file = p, append = TRUE)
  expect_equal(nrow(ov_read_gct(p)$data), 4L)
})
