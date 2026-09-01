# ─────────────────────────────────────────────────────────
# OmicsVisor - Helper Functions
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

# Default-value operator.
#
# Returns `b` only when `a` carries no usable value: NULL, a zero-length
# vector, or a single empty string. A multi-element vector is a usable value
# and must be returned unchanged — an earlier version tested
# `!isTRUE(nzchar(a))`, which is FALSE for any vector of length > 1 and so
# silently swapped real selections for the fallback.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L) return(b)
  if (length(a) == 1L && is.character(a) && !is.na(a) && !nzchar(a)) return(b)
  a
}

# Define the two color vectors
woco <- c("#C1A172", "#FDA70D", "#F85414", "#677F6E", "#B25F00", "#007FB2", "pink",
  "#77B04B", "#9CC7D4", "#FF7F71", "#5F4B7B", "#88BDE6", "#FBB258", "#90CD97",
  "#F6AAC9", "#BFA554", "#BC99C7", "#EDDD46", "#F07E6E")

distcols3 <- c("#88BDE6", "#FBB258", "#90CD97", "#F6AAC9", "#BFA554", "#BC99C7",
  "#EDDD46", "#F07E6E", "#E6194B", "#3CB44B", "#4363D8", "#F58231", "#911EB4",
  "#33B0B0", "#F032E6", "#FABEBE", "#008080", "#E6BEFF", "#9A6324", "#FFFAC8",
  "#800000", "#AAFFC3", "#808000", "#FFD8B1", "#000075", "#808080", "#FFE119",
  "#000000", "#008000", "#000080", "#800080", "#7F7F7F", "#804000", "#408000",
  "#008040", "#004080", "#400080", "#800040", "#666666", "#999999", "#FF0000",
  "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF", "#4C4C4C", "#B3B3B3",
  "#FF8000", "#80FF00", "#00FF80", "#0080FF", "#8000FF", "#FF0080", "#333333",
  "#CCCCCC", "#FF6666", "#FFFF66", "#66FF66", "#66FFFF", "#6666FF", "#FF66FF",
  "#191919", "#E6E6E6", "#FFCC66", "#CCFF66", "#66FFCC", "#66CCFF", "#CC66FF",
  "#B25F00", "#007FB2")

# Combine the vectors into one
combined_colors <- c(woco, distcols3)


swapFC <- function(df, groups = NULL) {
  stopifnot(is.data.frame(df))

  stat_pat <- "^(logFC|t|P\\.Value|adj\\.P\\.Val)_"
  stat_ix  <- grep(stat_pat, names(df))
  if (!length(stat_ix)) return(df)

  nms   <- names(df)[stat_ix]
  comps <- sub(stat_pat, "", nms)

  # restrict to x.over.y comparisons
  keep     <- grepl("\\.over\\.", comps)
  stat_ix  <- stat_ix[keep]
  nms      <- nms[keep]
  comps    <- comps[keep]
  if (!length(stat_ix)) return(df)

  sel <- if (is.null(groups)) rep(TRUE, length(comps)) else comps %in% trimws(groups)
  if (!any(sel)) return(df)

  target_ix <- stat_ix[sel]
  prefix    <- sub("_.*$", "", names(df)[target_ix])

  # rename x.over.y → y.over.x
  flipped           <- sub("^(.+)\\.over\\.(.+)$", "\\2.over.\\1", comps[sel])
  names(df)[target_ix] <- paste0(prefix, "_", flipped)

  # negate logFC only
  logfc_ix <- target_ix[prefix == "logFC"]
  if (length(logfc_ix)) {
    num <- vapply(logfc_ix, function(i) is.numeric(df[[i]]), logical(1))
    df[, logfc_ix[num]] <- -df[, logfc_ix[num], drop = FALSE]
  }

  # drop t_ and P.Value_ columns (invalidated by direction swap)
  drop_ix <- grep("^(t|P\\.Value)_", names(df))
  if (length(drop_ix)) df <- df[, -drop_ix, drop = FALSE]

  df
}

find_genes <- function(strings, vector, split = ";", ignore.case = FALSE) {
  sapply(strings, function(string) {
    grepper <- paste(c(paste0("^", string, "$"), paste0("^", string, split),
      paste0(split, string, split), paste0(split, string, "$")), collapse = "|")
    indices <- grep(grepper, vector, ignore.case = ignore.case)
    if (length(indices) == 0) {
      return(NA)
    } else {
      return(indices)
    }
  })
}

# 1) Helper function to detect comparisons (logFC_ / adj.P.Val_)
detect_comparisons <- function(col_names) {
  # Find columns starting with 'logFC_' and 'adj.P.Val_'
  logFC_cols <- grep("^logFC_", col_names, value = TRUE)
  adj_cols <- grep("^adj\\.P\\.Val_", col_names, value = TRUE)

  # Remove the prefix to isolate the comparison name
  logFC_names <- sub("^logFC_", "", logFC_cols)
  adj_names <- sub("^adj\\.P\\.Val_", "", adj_cols)

  # The intersection is the set of comparisons with both columns present
  intersect(logFC_names, adj_names)
}


# ── Upload handling ──────────────────────────────────────────────────────────

#' Read an uploaded results table.
#'
#' Kept out of app.R so the upload path is testable without starting a server.
#'
#' @param path      path on disk (Shiny's `input$upload$datapath`)
#' @param file_name original file name, used only to pick the parser
#' @return a plain data.frame
ov_read_upload <- function(path, file_name = path) {
  ext <- tolower(tools::file_ext(file_name))

  df <- switch(
    ext,
    "xlsx" = openxlsx::read.xlsx(path, sheet = 1),
    "xls"  = openxlsx::read.xlsx(path, sheet = 1),
    "txt"  = ,
    "tsv"  = as.data.frame(data.table::fread(
      path, sep = "\t", quote = "", na.strings = c("", "NA", "NaN"))),
    "csv"  = as.data.frame(data.table::fread(
      path, sep = ",", quote = "\"", na.strings = c("", "NA", "NaN"))),
    stop(sprintf(
      "Unsupported file type: .%s (expected .xlsx, .xls, .txt, .tsv or .csv)",
      ext), call. = FALSE)
  )

  if (is.list(df) && !is.data.frame(df)) df <- as.data.frame(do.call(cbind, df))
  as.data.frame(df, stringsAsFactors = FALSE)
}

#' Classify the columns of a results table.
#'
#' @param df        the results table
#' @param int_regex user-supplied intensity-column regex
#' @return list(logFC_cols, adjP_cols, intensity_cols) — always character
#'   vectors, never NULL.
ov_detect_columns <- function(df, int_regex = "^Intensity") {
  nms <- names(df)

  # A user-typed regex can be syntactically invalid mid-edit (e.g. "^Imputed[").
  # grep() would then abort the whole `data` reactive and take every module
  # down with it, so fall back to "no match" instead.
  safe_grep <- function(pattern) {
    tryCatch(grep(pattern, nms, value = TRUE, ignore.case = TRUE),
             error   = function(e) character(0),
             warning = function(w) character(0))
  }

  list(
    logFC_cols     = safe_grep("logFC"),
    adjP_cols      = safe_grep("adj\\.?p|fdr|q\\.?val"),
    intensity_cols = safe_grep(int_regex %||% "^Intensity")
  )
}

#' Stretch a fixed colour vector to cover `n` groups.
#'
#' scale_*_manual() aborts with "Insufficient values in manual scale" as soon
#' as there are more groups than colours — which a real experiment reaches
#' easily (the Okabe-Ito palette holds 8, and one group per sample is common).
#' Interpolating keeps the requested look and never errors.
ov_expand_palette <- function(cols, n) {
  cols <- cols[!is.na(cols)]
  if (length(cols) == 0L) cols <- "#4C72B0"
  if (n <= length(cols)) return(cols[seq_len(n)])
  grDevices::colorRampPalette(cols)(n)
}

#' Open a PDF graphics device that can render the app's UTF-8 plot labels.
#'
#' The default `pdf()` device is limited to a single-byte encoding, so labels
#' containing "≤", "≥" or "—" are silently transliterated with a warning.
#' cairo_pdf handles them, and is available in every build that reports
#' `capabilities("cairo")`.
ov_pdf_device <- function() {
  if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
}

read_gmt <- function(path) {
  lines <- readLines(path, warn = FALSE)
  gene_sets <- lapply(lines, function(line) {
    fields <- strsplit(line, "\t")[[1]]
    genes <- fields[-c(1, 2)]  # Remove gene set name + description/URL
    return(genes)
  })
  names(gene_sets) <- sapply(lines, function(line) strsplit(line, "\t")[[1]][1])
  return(gene_sets)
}

