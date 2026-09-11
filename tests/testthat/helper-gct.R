# ─────────────────────────────────────────────────────────
# GCT fixture writers (audit OV-ENR-12)
#
# The audit specified the fixture list: 1.2 without metadata; 1.3 with none,
# row-only, column-only and both; duplicate row IDs; non-numeric fields;
# declared-versus-actual dimension mismatch; blank sample names. These build
# each of those against the GenePattern reference layout.
# ─────────────────────────────────────────────────────────

gct_path <- function() withr::local_tempfile(fileext = ".gct", .local_envir = parent.frame())

# GCT 1.2:  #1.2 / nrow ncol / Name Description <samples> / rows
write_gct_12 <- function(path, n_row = 6L, n_col = 3L, ids = NULL,
                         values = NULL, sample_names = NULL) {
  ids <- ids %||% sprintf("GENE%02d", seq_len(n_row))
  samples <- sample_names %||% sprintf("S%d", seq_len(n_col))
  values <- values %||% matrix(sprintf("%.3f", seq_len(n_row * n_col) / 10),
                               n_row, n_col)
  writeLines(c(
    "#1.2",
    paste(n_row, n_col, sep = "\t"),
    paste(c("Name", "Description", samples), collapse = "\t"),
    apply(cbind(ids, paste0("desc of ", ids), values), 1,
          paste, collapse = "\t")
  ), path)
  path
}

# GCT 1.3:  #1.3 / nrow ncol nrmeta ncmeta / id <rmeta> <samples>
#           <ncmeta lines of column metadata, BEFORE the data>
#           <nrow data lines>
write_gct_13 <- function(path, n_row = 6L, n_col = 3L,
                         row_meta = character(0), col_meta = character(0),
                         ids = NULL, values = NULL, sample_names = NULL,
                         declared_rows = NULL) {
  ids     <- ids %||% sprintf("GENE%02d", seq_len(n_row))
  samples <- sample_names %||% sprintf("S%d", seq_len(n_col))
  values  <- values %||% matrix(sprintf("%.3f", seq_len(n_row * n_col) / 10),
                                n_row, n_col)
  n_rm <- length(row_meta); n_cm <- length(col_meta)

  header <- paste(c("id", row_meta, samples), collapse = "\t")

  cmeta_lines <- character(0)
  if (n_cm) cmeta_lines <- vapply(seq_along(col_meta), function(k) paste(c(
      col_meta[k], rep("", n_rm),
      paste0(col_meta[k], "_", seq_len(n_col))), collapse = "\t"),
      character(1))

  rmeta_block <- if (n_rm)
    vapply(seq_len(n_row), function(i)
      paste(paste0(row_meta, "_", i), collapse = "\t"), character(1))
  else rep("", n_row)

  data_lines <- vapply(seq_len(n_row), function(i) {
    bits <- c(ids[i], if (n_rm) rmeta_block[i] else NULL, values[i, ])
    paste(bits, collapse = "\t")
  }, character(1))

  writeLines(c(
    "#1.3",
    paste(declared_rows %||% n_row, n_col, n_rm, n_cm, sep = "\t"),
    header, cmeta_lines, data_lines
  ), path)
  path
}
