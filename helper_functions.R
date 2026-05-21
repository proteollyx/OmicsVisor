# ─────────────────────────────────────────────────────────
# OmicsVisor - Helper Functions
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

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

