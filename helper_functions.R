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


#' Reverse the direction of selected pairwise comparisons.
#'
#' For each selected `x.over.y` comparison this renames the suffix to
#' `y.over.x` and flips the sign of the quantities that are direction
#' dependent. Columns belonging to comparisons that were not selected are left
#' exactly as they were.
#'
#' Which quantities change, and why:
#'
#'   * `logFC` negates - the effect is measured in the opposite direction.
#'   * `t`     negates - the test statistic follows the effect.
#'   * `P.Value`, `adj.P.Val` are unchanged - a two-sided p-value is invariant
#'     under reversal of the contrast.
#'
#' An earlier version deleted every `t_` and `P.Value_` column in the table,
#' not merely those of the selected comparisons, on the grounds that they were
#' "invalidated by the direction swap". That reasoning was wrong for two-sided
#' tests, and because the deletion was global it destroyed statistics belonging
#' to comparisons the user had not touched - including in the processed table
#' offered for download (audit finding OV-CORR-01).
#'
#' @param df      results table
#' @param groups  comparison names to reverse, e.g. "KO.over.WT". NULL reverses
#'                every `x.over.y` comparison found.
#' @return the table with the selected comparisons reversed
swapFC <- function(df, groups = NULL) {
  stopifnot(is.data.frame(df))

  # sign applied to each statistic family when the contrast is reversed
  transforms <- c(logFC = -1, t = -1, P.Value = 1, adj.P.Val = 1)
  stat_pat   <- "^(logFC|t|P\\.Value|adj\\.P\\.Val)_"

  stat_ix <- grep(stat_pat, names(df))
  if (!length(stat_ix)) return(df)

  comps <- sub(stat_pat, "", names(df)[stat_ix])
  keep  <- grepl("\\.over\\.", comps)          # only x.over.y can be reversed
  if (!any(keep)) return(df)

  present <- unique(comps[keep])
  targets <- if (is.null(groups)) present else intersect(present, trimws(groups))
  if (!length(targets)) return(df)

  for (old_cmp in targets) {
    parts   <- strsplit(old_cmp, ".over.", fixed = TRUE)[[1]]
    if (length(parts) != 2L) next
    new_cmp <- paste(parts[2L], parts[1L], sep = ".over.")

    # refuse to overwrite a comparison that already exists in the other direction
    clash <- paste0(names(transforms), "_", new_cmp)
    clash <- clash[clash %in% names(df) & !clash %in% paste0(names(transforms), "_", old_cmp)]
    if (length(clash))
      stop("Reversing '", old_cmp, "' would collide with existing column(s): ",
           paste(clash, collapse = ", "), call. = FALSE)

    for (prefix in names(transforms)) {
      old_col <- paste0(prefix, "_", old_cmp)
      if (!old_col %in% names(df)) next
      if (transforms[[prefix]] == -1 && is.numeric(df[[old_col]]))
        df[[old_col]] <- -df[[old_col]]
      names(df)[names(df) == old_col] <- paste0(prefix, "_", new_cmp)
    }
  }
  df
}

#' Find which rows contain a given gene symbol.
#'
#' Fields may hold several identifiers separated by `split` (protein groups
#' commonly look like "AAA;BBB;CCC"), so a match must be against a whole token
#' rather than a substring.
#'
#' This used to build a regular expression out of the query. Any identifier
#' containing a regex metacharacter was then interpreted rather than matched:
#' searching for a gene literally named "A+B" returned "AB" and "AAB" and
#' missed "A+B" itself — a false positive and a false negative at once, in the
#' module that produces the ID lists every other view consumes (audit
#' OV-UX-18). Splitting and comparing exactly removes the whole class.
#'
#' @param strings     character vector of queries
#' @param vector      character vector of fields to search
#' @param split       token separator within a field
#' @param ignore.case compare case-insensitively
#' @return named list, one element per query: the matching indices, or NA
find_genes <- function(strings, vector, split = ";", ignore.case = FALSE) {
  vec <- as.character(vector)
  tokens <- strsplit(vec, split, fixed = TRUE)
  tokens <- lapply(tokens, trimws)
  if (ignore.case) tokens <- lapply(tokens, tolower)

  out <- lapply(strings, function(query) {
    q <- trimws(as.character(query))
    if (ignore.case) q <- tolower(q)
    hit <- vapply(tokens, function(tk) any(!is.na(tk) & tk == q), logical(1))
    if (any(hit)) which(hit) else NA
  })
  names(out) <- strings
  out
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


# ── Hit calling ──────────────────────────────────────────────────────────────

#' Decide which features count as hits.
#'
#' The single definition of "significant" used by every module. Before this
#' existed each module implemented its own threshold check, and they disagreed:
#' UpSet used >= and <= while Volcano, Volcano Printer, Donut and the logFC
#' Scatter used > and <. A feature sitting exactly on a cutoff was therefore a
#' hit in one view and not in another (audit finding OV-VIZ-07).
#'
#' Boundaries are inclusive by default: "adj.P <= 0.05" is how the cutoffs are
#' described in the interface and printed in plot subtitles, so the code now
#' matches the wording.
#'
#' Missing, infinite and out-of-range values never count as hits. An adjusted
#' p-value outside [0, 1] is not a probability, so it is treated as invalid
#' rather than silently compared.
#'
#' @param logfc      numeric vector of log fold changes
#' @param padj       numeric vector of adjusted p-values
#' @param fc_cut     absolute log fold-change threshold
#' @param padj_cut   adjusted p-value threshold
#' @param inclusive  TRUE for >= / <=, FALSE for > / <
#' @param direction  "both", "up" or "down"
#' @return logical vector, never NA
ov_is_hit <- function(logfc, padj, fc_cut, padj_cut,
                      inclusive = TRUE,
                      direction = c("both", "up", "down")) {
  direction <- match.arg(direction)

  logfc <- suppressWarnings(as.numeric(logfc))
  padj  <- suppressWarnings(as.numeric(padj))

  if (length(logfc) == 0L || length(padj) == 0L) return(logical(0))
  if (length(padj) == 1L)  padj  <- rep(padj,  length(logfc))
  if (length(logfc) == 1L) logfc <- rep(logfc, length(padj))

  # a p-value outside [0, 1] is not a probability; refuse to threshold on it
  valid <- is.finite(logfc) & is.finite(padj) & padj >= 0 & padj <= 1

  fc_ok <- switch(
    direction,
    both = if (inclusive) abs(logfc) >= fc_cut else abs(logfc) > fc_cut,
    up   = if (inclusive) logfc >=  fc_cut     else logfc >  fc_cut,
    down = if (inclusive) logfc <= -fc_cut     else logfc < -fc_cut
  )
  p_ok <- if (inclusive) padj <= padj_cut else padj < padj_cut

  out <- valid & fc_ok & p_ok
  out[is.na(out)] <- FALSE
  out
}

#' -log10 of an adjusted p-value, safe for plotting.
#'
#' `-log10()` applied straight to an adjusted p-value column misbehaves in three
#' ways that all occur in real exported tables: `0` gives `Inf`, a negative
#' value gives `NaN`, and a value above 1 gives a negative ordinate. Plotting
#' libraries then discard the non-finite points, so the single most significant
#' feature silently disappears from the volcano rather than appearing at the top
#' (audit finding OV-NUM-03).
#'
#' Zero adjusted p-values are not hypothetical - they arise from numerical
#' underflow and from rounding in exported tables.
#'
#' The 1D Enrichment module already used this flooring; this makes it shared.
#'
#' @param p       numeric vector of adjusted p-values
#' @return list(y = numeric vector safe to plot, n_invalid = count of values
#'   outside [0, 1] or non-finite, which are returned as NA)
ov_neglog10_padj <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  invalid <- !is.na(p) & (!is.finite(p) | p < 0 | p > 1)
  p[invalid] <- NA_real_
  # floor at the smallest representable double so an exact zero plots at a
  # finite maximum instead of vanishing
  list(y = -log10(pmax(p, .Machine$double.xmin)),
       n_invalid = sum(invalid))
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

  # openxlsx warns "No data found on worksheet." and then returns NULL. We turn
  # that into a clear error below, so muffle the warning rather than show the
  # user both.
  read_xlsx_quiet <- function(p) {
    withCallingHandlers(
      openxlsx::read.xlsx(p, sheet = 1),
      warning = function(w) {
        if (grepl("No data found on worksheet", conditionMessage(w), fixed = TRUE))
          invokeRestart("muffleWarning")
      }
    )
  }

  df <- switch(
    ext,
    "xlsx" = read_xlsx_quiet(path),
    # Legacy binary .xls (BIFF8) is a different format that openxlsx cannot
    # read, so accepting it only produced a confusing failure at read time
    # (audit OV-IO-11). Reject it with something actionable instead.
    "xls"  = stop("Legacy .xls files are not supported. Please open the file ",
                  "and save it as .xlsx, or export as .csv or .tsv.",
                  call. = FALSE),
    "txt"  = ,
    "tsv"  = as.data.frame(data.table::fread(
      path, sep = "\t", quote = "", na.strings = c("", "NA", "NaN"))),
    "csv"  = as.data.frame(data.table::fread(
      path, sep = ",", quote = "\"", na.strings = c("", "NA", "NaN"))),
    stop(sprintf(
      "Unsupported file type: .%s (expected .xlsx, .xls, .txt, .tsv or .csv)",
      ext), call. = FALSE)
  )

  # openxlsx returns NULL for a worksheet with no data; say so plainly rather
  # than handing back a 0x0 frame for the caller to puzzle over.
  if (is.null(df))
    stop("The file contains no data (the first worksheet is empty).", call. = FALSE)

  if (is.list(df) && !is.data.frame(df)) df <- as.data.frame(do.call(cbind, df))
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if (nrow(df) == 0L || ncol(df) == 0L)
    stop("The file contains no data rows.", call. = FALSE)

  df
}

#' Inspect an uploaded results table.
#'
#' One pass over the table producing everything both the upload gate and the
#' diagnosis panel need. The gate blocks on `$fatal`; the panel reports the
#' whole structure.
#'
#' The policy, deliberately narrow (audit finding OV-STAT-06):
#'
#'   * **Fatal** - only what cannot be true. An adjusted p-value outside
#'     `[0, 1]` is not a probability. The realistic cause is not a corrupt file
#'     but a mis-mapped column: if a t-statistic column is renamed to
#'     `adj.P.Val_*`, `-log10()` of it still produces a plausible-looking
#'     volcano with hits called and exported, and nothing signals that the
#'     y-axis is meaningless. That failure is invisible without this check.
#'   * **Warning** - everything merely unusual. Duplicate identifiers, infinite
#'     fold changes and adjusted p-values of exactly zero all occur in
#'     legitimate exports; blocking them would reject real files to solve
#'     problems that affect one module at most.
#'
#' Naming alone cannot establish that a `logFC_` column is really log2, or that
#' an `adj.P.Val_` column was produced by Benjamini-Hochberg. Those are reported
#' as unverifiable rather than assumed.
#'
#' @param df        the uploaded table
#' @param int_regex intensity-column regex, for reporting only
#' @return list(rows, cols, id, comparisons, intensity, coerced, fatal, warnings)
ov_inspect_upload <- function(df, int_regex = "^Imputed") {
  fatal <- character(0); warn <- character(0)
  nms <- names(df)

  # ── identifier ────────────────────────────────────────────────────────────
  id <- list(present = "id" %in% nms, n_unique = NA_integer_,
             n_missing = NA_integer_, n_duplicate = NA_integer_)
  if (!id$present) {
    warn <- c(warn, paste0(
      "No column named 'id'. OmicsVisor is ID-driven: the modules that select, ",
      "cross-reference or intersect features need it, and will stay empty."))
  } else {
    v <- df[["id"]]
    id$n_unique    <- length(unique(v[!is.na(v)]))
    id$n_missing   <- sum(is.na(v))
    id$n_duplicate <- sum(duplicated(v))
    if (id$n_missing > 0)
      warn <- c(warn, sprintf("'id' has %d missing value(s).", id$n_missing))
    if (id$n_duplicate > 0)
      warn <- c(warn, sprintf(
        paste0("'id' has %d duplicate(s). Most views still work, but set ",
               "operations (UpSet) need unique identifiers and will refuse."),
        id$n_duplicate))
  }

  # ── comparisons ───────────────────────────────────────────────────────────
  comps <- detect_comparisons(nms)
  lone  <- setdiff(sub("^logFC_", "", grep("^logFC_", nms, value = TRUE)), comps)
  if (length(lone))
    warn <- c(warn, sprintf(
      "No adj.P.Val_ partner for: %s. These cannot be thresholded on significance.",
      paste(lone, collapse = ", ")))

  ctab <- NULL
  if (length(comps)) {
    rows <- lapply(comps, function(cmp) {
      fc <- suppressWarnings(as.numeric(df[[paste0("logFC_", cmp)]]))
      pv <- suppressWarnings(as.numeric(df[[paste0("adj.P.Val_", cmp)]]))
      bad <- sum(!is.na(pv) & (!is.finite(pv) | pv < 0 | pv > 1))
      if (bad > 0)
        fatal <<- c(fatal, sprintf(
          paste0("adj.P.Val_%s contains %d value(s) outside [0, 1]. ",
                 "These are not probabilities - check that the column has not ",
                 "been mis-mapped (a t-statistic column renamed, for example)."),
          cmp, bad))
      n_inf <- sum(is.infinite(fc))
      if (n_inf > 0)
        warn <<- c(warn, sprintf(
          "logFC_%s has %d infinite value(s); they are excluded from hit calls.",
          cmp, n_inf))
      n_zero <- sum(!is.na(pv) & pv == 0)
      if (n_zero > 0)
        warn <<- c(warn, sprintf(
          paste0("adj.P.Val_%s has %d value(s) of exactly zero, most likely ",
                 "underflow or rounding upstream. They are plotted at the ",
                 "maximum rather than dropped."), cmp, n_zero))
      data.frame(
        comparison = cmp,
        n_na_logFC = sum(is.na(fc)),
        n_na_adjP  = sum(is.na(pv)),
        padj_min   = suppressWarnings(min(pv, na.rm = TRUE)),
        padj_max   = suppressWarnings(max(pv, na.rm = TRUE)),
        n_invalid  = bad,
        stringsAsFactors = FALSE)
    })
    ctab <- do.call(rbind, rows)
    # min()/max() of an all-NA column returns +/-Inf with a warning; report NA
    for (col in c("padj_min", "padj_max"))
      ctab[[col]][!is.finite(ctab[[col]])] <- NA_real_
  }

  # ── intensity columns ─────────────────────────────────────────────────────
  int_cols <- ov_detect_columns(df, int_regex)$intensity_cols
  intensity <- list(regex = int_regex, n = length(int_cols),
                    pct_missing = NA_real_, candidates = character(0))
  if (length(int_cols) == 0) {
    intensity$candidates <- utils::head(names(ov_intensity_prefix_groups(df)), 3)
    warn <- c(warn, paste0(
      "No intensity columns matched '", int_regex,
      "'. The Heatmap, PCA, Boxplot and Correlation views will stay empty until ",
      "the pattern matches your sample columns."))
  } else {
    m <- suppressWarnings(as.matrix(df[, int_cols, drop = FALSE]))
    storage.mode(m) <- "double"
    intensity$pct_missing <- 100 * sum(is.na(m)) / max(1L, length(m))
  }

  # ── columns that are not numeric but look as though they should be ────────
  stat_like <- grep("^(logFC|t|P\\.Value|adj\\.P\\.Val)_", nms, value = TRUE)
  coerced <- stat_like[!vapply(df[stat_like], is.numeric, logical(1))]
  if (length(coerced))
    warn <- c(warn, sprintf(
      "%d statistic column(s) are not numeric and will be coerced: %s",
      length(coerced), paste(utils::head(coerced, 4), collapse = ", ")))

  list(rows = nrow(df), cols = ncol(df), id = id,
       comparisons = ctab, n_comparisons = length(comps),
       intensity = intensity, coerced = coerced,
       fatal = fatal, warnings = warn)
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

#' Column names that plausibly hold sample intensities.
#'
#' More than half of real result tables use no `Imputed`/`Intensity` prefix at
#' all (Perseus and some MaxQuant exports name columns plainly, e.g.
#' `ctr_PeC_A`), so the default preset matches nothing and the matrix-based
#' modules silently stay empty. Auto-selecting would be wrong — the same set
#' also contains `ANOVA.*` and similar derived columns — so this only supplies
#' examples for the sidebar hint, letting the user write an accurate regex.
#'
#' @return character vector of candidate column names, most-likely first
ov_intensity_candidates <- function(df) {
  nms <- names(df)
  if (is.null(nms)) return(character(0))

  numeric_cols <- nms[vapply(df, is.numeric, logical(1))]

  # Exclude identifiers and anything that is clearly a statistic.
  drop_pat <- paste0(
    "^(id|Genes?|Protein|Uniprot|Sequence)",           # identifiers
    "|^(logFC|t|P\\.Value|adj\\.P\\.Val|modF|F)[._]",  # per-comparison stats
    "|^(ANOVA|Student|Welch|q[._]?val|FDR)",           # other test output
    "|\\.(pvalue|padj|qvalue)$"
  )
  candidates <- numeric_cols[!grepl(drop_pat, numeric_cols, ignore.case = TRUE)]
  candidates
}

#' Rank candidate intensity columns by shared leading token.
#'
#' Listing arbitrary candidates is not much help on a wide table: a phospho
#' peptide-collapse export offers 49 numeric non-statistic columns, of which
#' the useful ones are two blocks (`Intensity.` x19, `Ori.` x20) buried among
#' `PTM_*`, `n_valid_*` and `sparse_*`. Grouping by leading token and ranking
#' by block size surfaces the real sample blocks first.
#'
#' @param min_size smallest block worth proposing
#' @return named list of column vectors, largest block first; names are the
#'   prefixes (including their separator), ready to use as `^<prefix>`
ov_intensity_prefix_groups <- function(df, min_size = 3L) {
  cand <- ov_intensity_candidates(df)
  if (length(cand) == 0L) return(list())

  # Leading token: up to and including the first "." if the name has one,
  # otherwise up to and including the first "_".
  stem <- ifelse(
    grepl(".", cand, fixed = TRUE),
    sub("^([^.]+\\.).*$", "\\1", cand),
    sub("^([^_]+_).*$",     "\\1", cand)
  )
  stem[stem == cand] <- ""          # no separator -> no usable prefix

  groups <- split(cand, stem)
  groups <- groups[names(groups) != "" & lengths(groups) >= min_size]
  if (length(groups) == 0L) return(list())
  groups[order(-lengths(groups), names(groups))]
}

#' Longest common leading token shared by a set of column names, if any.
#'
#' Used to propose a starting regex in the sidebar hint.
ov_common_prefix <- function(x) {
  if (length(x) < 2) return("")
  chars <- strsplit(x, "", fixed = TRUE)
  n     <- min(lengths(chars))
  if (n == 0) return("")
  first <- chars[[1]]
  k <- 0L
  for (i in seq_len(n)) {
    if (all(vapply(chars, function(cc) cc[i] == first[i], logical(1)))) k <- i else break
  }
  substr(x[1], 1, k)
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



# ─────────────────────────────────────────────────────────
# Dimension-reduction retention accounting
#
# PCA and UMAP need a complete matrix, so every feature with a missing value
# in any selected sample is dropped. With label-free proteomics this routinely
# removes most of the data, and the loss is rarely uniform: one poorly
# covered sample can be responsible for nearly all of it. A user who never
# sees that number cannot tell a PCA computed on 8,000 features from one
# computed on 400, and both look equally convincing (audit OV-NUM-09).
#
# Returns the accounting plus, for each sample, how many features would be
# recovered by excluding that sample alone - the actionable diagnostic,
# because the usual fix is to deselect one bad run rather than to impute.
# ─────────────────────────────────────────────────────────
ov_dr_retention <- function(mat) {
  mat <- as.matrix(mat)
  storage.mode(mat) <- "double"
  n_in <- nrow(mat)

  miss      <- !is.finite(mat)
  n_missing <- rowSums(miss)
  complete  <- n_missing == 0L
  n_out     <- sum(complete)

  # Features missing in exactly one sample are recoverable by dropping it.
  one_off <- which(n_missing == 1L)
  recover <- integer(ncol(mat))
  names(recover) <- colnames(mat)
  if (length(one_off)) {
    culprit <- max.col(miss[one_off, , drop = FALSE], ties.method = "first")
    tab     <- table(factor(culprit, levels = seq_len(ncol(mat))))
    recover <- as.integer(tab)
    names(recover) <- colnames(mat)
  }

  per_sample <- data.frame(
    sample     = colnames(mat),
    n_missing  = as.integer(colSums(miss)),
    pct_missing = if (n_in > 0) 100 * colSums(miss) / n_in else numeric(ncol(mat)),
    recoverable = recover,
    stringsAsFactors = FALSE, row.names = NULL
  )

  worst <- if (nrow(per_sample) && max(per_sample$recoverable) > 0)
             per_sample[which.max(per_sample$recoverable), ] else NULL

  # Whether the filter was benign. Dropout in label-free proteomics is
  # intensity-dependent, so complete-case filtering preferentially removes
  # low-abundance features and the retained set is not a random subsample.
  # Counting how many were lost says nothing about that; comparing the
  # abundance of what was kept against what was lost is the diagnostic that
  # actually answers it (audit OV-STAT-10).
  abundance <- rowMeans(mat, na.rm = TRUE)
  abundance[!is.finite(abundance)] <- NA_real_
  kept_ab <- abundance[complete]
  lost_ab <- abundance[!complete]

  q <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(c(median = NA_real_, q1 = NA_real_, q3 = NA_real_))
    stats::setNames(stats::quantile(x, c(0.5, 0.25, 0.75), names = FALSE),
                    c("median", "q1", "q3"))
  }
  kept_q <- q(kept_ab); lost_q <- q(lost_ab)

  # A shift worth mentioning, expressed on the data's own scale rather than
  # as a p-value: a rank test on thousands of features calls everything
  # significant and would tell the user nothing about magnitude.
  shift <- unname(lost_q["median"] - kept_q["median"])

  list(
    n_in        = n_in,
    n_complete  = n_out,
    n_dropped   = n_in - n_out,
    pct_retained = if (n_in > 0) 100 * n_out / n_in else NA_real_,
    complete    = complete,
    per_sample  = per_sample[order(-per_sample$n_missing), , drop = FALSE],
    worst_sample = worst,
    abundance   = list(
      values      = abundance,
      kept        = kept_q,
      lost        = lost_q,
      shift       = shift,
      n_kept      = sum(is.finite(kept_ab)),
      n_lost      = sum(is.finite(lost_ab))
    )
  )
}


# ─────────────────────────────────────────────────────────
# Export manifest
#
# Audit finding OV-REP-04: an exported figure or table carries no record of
# what produced it. This writes the provenance the app can actually vouch
# for - which file, which build, which environment - and is deliberately
# explicit about the much larger set of things it cannot know, because a
# manifest that implies more provenance than exists is worse than none.
#
# The app never sees the search engine, the normalisation, the imputation or
# the statistical test; those happened upstream, and the numbers in the
# workbook are taken entirely on trust. Saying so is the point.
# ─────────────────────────────────────────────────────────
# The commit is available when the app runs from a checkout; a Connect
# deployment has no .git, and in that case the version and release date are
# what pin the build. Better to say so than to print something misleading.
ov_git_commit <- function() {
  out <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  if (length(out) == 1 && grepl("^[0-9a-f]{7,}$", out)) out
  else "(not available; see version and release date)"
}

# Modules describe their own settings through this; the manifest renders
# whatever has been registered. A module that the user never opened
# registers nothing and is simply absent, rather than reported at defaults
# it was never actually run with.
ov_register_settings <- function(register, module, values) {
  if (is.null(register) || !is.function(register)) return(invisible(NULL))
  vals <- values[!vapply(values, is.null, logical(1))]
  register(module, vals)
  invisible(NULL)
}

ov_manifest <- function(file_name = NULL, file_path = NULL, report = NULL,
                        int_regex = NULL, modules = NULL) {

  kv <- function(k, v) sprintf("  %-22s %s", paste0(k, ":"), v)
  na <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "(not recorded)" else x

  hash <- "(not available)"
  size <- "(not available)"
  if (!is.null(file_path) && file.exists(file_path)) {
    size <- sprintf("%.2f MB", file.info(file_path)$size / 1024^2)
    hash <- tryCatch(
      digest::digest(file = file_path, algo = "sha256"),
      error = function(e) "(could not be computed)")
  }

  pkgs <- c("shiny", "openxlsx", "ggplot2", "umap", "UpSetR", "pheatmap",
            "plotly", "data.table", "dplyr")
  pkg_lines <- vapply(pkgs, function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)),
                  error = function(e) "(not installed)")
    kv(p, v)
  }, character(1), USE.NAMES = FALSE)

  out <- c(
    "OmicsVisor export manifest",
    "==========================",
    "",
    "Software",
    kv("Version",      ov_version),
    kv("Release date", ov_release_date),
    kv("Commit",       ov_git_commit()),
    kv("Generated",    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "Input file",
    kv("Name",         na(file_name)),
    kv("Size",         size),
    kv("SHA-256",      hash)
  )

  if (!is.null(report)) {
    id_state <- if (!isTRUE(report$id$present)) "no id column" else
      sprintf("%d unique, %d duplicate, %d missing",
              report$id$n_unique, report$id$n_duplicate, report$id$n_missing)
    out <- c(out,
      kv("Rows",        format(report$rows, big.mark = ",")),
      kv("Columns",     format(report$cols, big.mark = ",")),
      kv("Identifiers", id_state),
      "",
      "Detected in the file",
      kv("Comparisons", if (report$n_comparisons == 0) "none" else
                        paste(report$comparisons$comparison, collapse = ", ")),
      kv("Intensity regex",   na(int_regex %||% report$intensity$regex)),
      kv("Intensity columns", report$intensity$n)
    )
    if (length(report$warnings))
      out <- c(out, "", "Warnings raised on upload",
               paste0("  - ", report$warnings))
  }

  # Whatever the user actually configured, module by module. This is the part
  # that makes an exported figure reconstructible: the cutoffs and their
  # inclusivity, the comparison, the seed, the adjustment method.
  if (length(modules)) {
    out <- c(out, "", "Module settings")
    for (m in sort(names(modules))) {
      vals <- modules[[m]]
      if (!length(vals)) next
      out <- c(out, sprintf("  %s", m))
      for (k in names(vals)) {
        v <- vals[[k]]
        v <- if (is.null(v) || !length(v)) "(unset)"
             else if (is.logical(v)) paste(ifelse(v, "yes", "no"), collapse = ", ")
             else paste(format(v, trim = TRUE), collapse = ", ")
        out <- c(out, sprintf("    %-20s %s", paste0(k, ":"), v))
      }
    }
  }

  c(out,
    "",
    "Environment",
    kv("R", paste(R.version$major, R.version$minor, sep = ".")),
    kv("Platform", R.version$platform),
    pkg_lines,
    "",
    "What this manifest does NOT record",
    "  OmicsVisor reads a results table that was already produced elsewhere.",
    "  It cannot observe, and therefore cannot certify, any of the following:",
    "",
    "  - the search engine, database and version used for identification",
    "  - the quantification, normalisation and imputation applied upstream",
    "  - the statistical test behind the p-values, and its assumptions",
    "  - the multiple-testing correction actually used",
    "  - the base of the fold changes (log2 is assumed and never verified)",
    "  - whether the comparison directions are labelled as intended",
    "",
    "  Record those from the upstream pipeline. This manifest fixes only",
    "  which file was loaded and which build of the app read it.",
    ""
  )
}


# ─────────────────────────────────────────────────────────
# GCT reader (GenePattern 1.2 and 1.3)
#
# Audit finding OV-ENR-12: the previous reader carried the comment "should
# theoretically support #1.3 - to be tested properly". It did not.
#
# GCT 1.3 layout, which is the part that was wrong:
#
#   #1.3
#   <nrow> <ncol> <n row meta> <n col meta>
#   id  <row meta names...>  <sample names...>
#   <col meta name>  <blanks for row meta>  <values per sample>     x n col meta
#   <row id>  <row meta...>  <values...>                            x nrow
#
# Column metadata rows come *before* the data rows. The previous reader took
# the data first and the column metadata afterwards, so any 1.3 file that
# actually carried column metadata had those metadata rows parsed as data.
#
# Reads the whole file and splits it, rather than advancing a connection in
# stages: the declared dimensions can then be checked against what is
# actually present instead of being trusted.
# ─────────────────────────────────────────────────────────
ov_read_gct <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!(seq_along(lines) > 1L & !nzchar(trimws(lines)))]  # keep line 1
  if (!length(lines)) stop("Empty GCT file.", call. = FALSE)

  version <- trimws(strsplit(lines[1], "\t", fixed = TRUE)[[1]][1])
  if (!version %in% c("#1.2", "#1.3"))
    stop("Unsupported or missing GCT version header: expected #1.2 or #1.3, found '",
         substr(lines[1], 1, 20), "'.", call. = FALSE)
  if (length(lines) < 3L)
    stop("Truncated GCT file: expected a version line, a dimension line and a column header.",
         call. = FALSE)

  # strsplit() discards trailing empty fields, so a row ending in a tab - a
  # blank final sample name, or a short final data row - would lose the field
  # entirely and be reported as a dimension mismatch instead of the blank it
  # is. Appending a sentinel and dropping it preserves trailing empties.
  split_row <- function(x) {
    f <- strsplit(paste0(x, "\t."), "\t", fixed = TRUE)[[1]]
    f[-length(f)]
  }
  dims <- suppressWarnings(as.integer(split_row(trimws(lines[2]))))
  dims <- dims[!is.na(dims)]

  n_expected_dims <- if (version == "#1.2") 2L else 4L
  if (length(dims) < n_expected_dims)
    stop(sprintf("Malformed GCT %s dimension line: expected %d integers, found %d.",
                 sub("^#", "", version), n_expected_dims, length(dims)), call. = FALSE)

  n_row <- dims[1]; n_col <- dims[2]
  n_rmeta <- if (version == "#1.3") dims[3] else 0L
  n_cmeta <- if (version == "#1.3") dims[4] else 0L
  if (version == "#1.2") n_rmeta <- 1L   # 1.2's Description column

  if (is.na(n_row) || is.na(n_col) || n_row < 1L || n_col < 1L)
    stop("GCT declares a non-positive number of rows or columns.", call. = FALSE)

  hdr <- split_row(lines[3])
  expected_hdr <- 1L + n_rmeta + n_col
  if (length(hdr) != expected_hdr)
    stop(sprintf(paste("GCT column header has %d fields but the declared dimensions",
                       "require %d (1 id + %d metadata + %d samples)."),
                 length(hdr), expected_hdr, n_rmeta, n_col), call. = FALSE)

  warnings <- character(0)
  samples <- hdr[(2L + n_rmeta):expected_hdr]
  blank <- !nzchar(trimws(samples))
  if (any(blank)) {
    samples[blank] <- paste0("sample_", which(blank))
    warnings <- c(warnings, sprintf(
      "%d sample name(s) were blank and have been named %s.",
      sum(blank), paste(samples[blank], collapse = ", ")))
  }
  if (anyDuplicated(samples)) {
    warnings <- c(warnings, sprintf("Duplicate sample name(s): %s. Made unique.",
                                    paste(unique(samples[duplicated(samples)]), collapse = ", ")))
    samples <- make.unique(samples)
  }

  # Column metadata sits between the header and the data in 1.3.
  body_start <- 4L + n_cmeta
  col_meta <- NULL
  if (n_cmeta > 0L) {
    if (length(lines) < body_start - 1L)
      stop(sprintf("GCT declares %d column metadata row(s) but the file ends before them.",
                   n_cmeta), call. = FALSE)
    cm <- lapply(lines[4:(3L + n_cmeta)], split_row)
    col_meta <- as.data.frame(
      lapply(cm, function(r) r[(2L + n_rmeta):min(length(r), expected_hdr)]),
      stringsAsFactors = FALSE, col.names = vapply(cm, `[`, character(1), 1L))
    rownames(col_meta) <- samples
  }

  data_lines <- lines[body_start:length(lines)]
  if (length(data_lines) != n_row)
    stop(sprintf("GCT declares %d data row(s) but %d %s present.",
                 n_row, length(data_lines),
                 if (length(data_lines) == 1L) "is" else "are"), call. = FALSE)

  parts <- lapply(data_lines, split_row)
  short <- lengths(parts) != expected_hdr
  if (any(short))
    stop(sprintf("GCT data row %d has %d fields but %d are required.",
                 which(short)[1], lengths(parts)[which(short)[1]], expected_hdr),
         call. = FALSE)

  row_ids <- vapply(parts, `[`, character(1), 1L)
  if (anyDuplicated(row_ids))
    warnings <- c(warnings, sprintf(
      paste("%d duplicate row identifier(s), e.g. '%s'. Scores are looked up by",
            "name, so only the first occurrence of each is used."),
      sum(duplicated(row_ids)), row_ids[duplicated(row_ids)][1]))

  row_meta <- if (n_rmeta > 0L) {
    rm <- as.data.frame(do.call(rbind, lapply(parts, function(r) r[2:(1L + n_rmeta)])),
                        stringsAsFactors = FALSE)
    names(rm) <- hdr[2:(1L + n_rmeta)]
    rm
  } else NULL

  raw <- do.call(rbind, lapply(parts, function(r) r[(2L + n_rmeta):expected_hdr]))
  mat <- suppressWarnings(matrix(as.numeric(raw), nrow = n_row, ncol = n_col,
                                 dimnames = list(row_ids, samples)))
  coerced <- sum(is.na(mat) & nzchar(trimws(raw)))
  if (coerced > 0L)
    warnings <- c(warnings, sprintf(
      "%d non-numeric value(s) in the score matrix became missing.", coerced))
  if (all(is.na(mat)))
    stop("No numeric values could be read from the GCT score matrix.", call. = FALSE)

  list(data = mat, row_meta = row_meta, col_meta = col_meta,
       version = version, warnings = warnings)
}


# ─────────────────────────────────────────────────────────
# Heatmap colour semantics (audit OV-VIZ / 2.9)
#
# A diverging palette asserts that its midpoint means something. For row
# z-scores it does - zero is the row mean, and blue/red then reads as
# below/above average. For raw log intensities it does not: white lands
# wherever the data happen to be centred, so the same protein changes colour
# depending on which samples are on screen. Unscaled data therefore gets a
# sequential palette, where only order is implied.
# ─────────────────────────────────────────────────────────
ov_heatmap_palette <- function(scaled = FALSE, n = 100L) {
  if (isTRUE(scaled))
    grDevices::colorRampPalette(c("darkblue", "white", "firebrick"))(n)
  else
    grDevices::colorRampPalette(c("#F7FCF0", "#7BCCC4", "#0868AC", "#084081"))(n)
}

# Distance and linkage were fixed at Euclidean/complete. Both change the
# dendrogram, and therefore which groups a reader sees, so both are now
# chosen by the user and recorded in the manifest.
OV_HEATMAP_DISTANCES <- c("euclidean", "manhattan", "maximum", "canberra",
                          "1 - Pearson r" = "pearson",
                          "1 - Spearman r" = "spearman")
OV_HEATMAP_LINKAGES  <- c("complete", "average", "ward.D2", "single",
                          "centroid", "mcquitty", "median")

ov_cluster_dist <- function(mat, method = "euclidean") {
  if (method %in% c("pearson", "spearman")) {
    # Correlation across the other margin, turned into a distance. Features
    # with no variance have undefined correlation; treat them as maximally
    # distant rather than letting NA take down hclust().
    cm <- suppressWarnings(stats::cor(t(mat), method = method,
                                      use = "pairwise.complete.obs"))
    cm[!is.finite(cm)] <- 0
    stats::as.dist(1 - cm)
  } else {
    stats::dist(mat, method = method)
  }
}


# ─────────────────────────────────────────────────────────
# Optional sample metadata (audit OV-UX-17 / 2.7)
#
# Grouping is derived by splitting intensity column names on "_" or ".",
# which works for the standard in-house export and asks nothing of the user.
# It is fragile for anything else: a sample called "Imputed.WT_rep1.2" does
# not decompose the way the heuristic assumes.
#
# So the heuristic stays as the default, and an explicit table supersedes it
# when supplied. Matching is by sample name against the intensity column
# names, and it is deliberately strict about reporting what did not match -
# a metadata file that silently applies to half the samples would be worse
# than none, because the grouping would look deliberate.
# ─────────────────────────────────────────────────────────
ov_read_sample_metadata <- function(path, file_name = NULL, samples = NULL) {
  ext <- tolower(tools::file_ext(file_name %||% path))
  df <- if (ext %in% c("xlsx", "xls")) {
    ov_read_upload(path, file_name %||% basename(path))
  } else {
    utils::read.delim(path, sep = if (ext == "csv") "," else "\t",
                      stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!nrow(df)) stop("The sample metadata table is empty.", call. = FALSE)

  names(df) <- trimws(names(df))
  key <- grep("^sample$", names(df), ignore.case = TRUE)
  if (!length(key))
    stop("The sample metadata table needs a 'sample' column naming each intensity column.",
         call. = FALSE)
  names(df)[key[1]] <- "sample"
  df$sample <- trimws(as.character(df$sample))

  attrs <- setdiff(names(df), "sample")
  if (!length(attrs))
    stop("The sample metadata table has only a 'sample' column and no attributes.",
         call. = FALSE)

  warnings <- character(0)
  if (anyDuplicated(df$sample)) {
    dup <- unique(df$sample[duplicated(df$sample)])
    warnings <- c(warnings, sprintf(
      "Duplicate sample name(s) in the metadata: %s. The first row of each is used.",
      paste(dup, collapse = ", ")))
    df <- df[!duplicated(df$sample), , drop = FALSE]
  }

  matched <- unmatched <- extra <- character(0)
  if (!is.null(samples)) {
    matched   <- intersect(samples, df$sample)
    unmatched <- setdiff(samples, df$sample)
    extra     <- setdiff(df$sample, samples)
    if (length(unmatched))
      warnings <- c(warnings, sprintf(
        "%d intensity column(s) have no metadata row and will fall back to the column name: %s.",
        length(unmatched), paste(utils::head(unmatched, 5), collapse = ", ")))
    if (length(extra))
      warnings <- c(warnings, sprintf(
        "%d metadata row(s) match no intensity column and are ignored: %s.",
        length(extra), paste(utils::head(extra, 5), collapse = ", ")))
  }

  list(table = df, attributes = attrs, warnings = warnings,
       matched = matched, unmatched = unmatched, extra = extra)
}

# Build the group label for each sample from the chosen metadata columns,
# falling back to the sample's own name where no metadata row exists.
ov_metadata_groups <- function(meta, samples, columns) {
  columns <- intersect(columns, meta$attributes)
  if (!length(columns)) return(NULL)
  idx <- match(samples, meta$table$sample)
  lab <- vapply(seq_along(samples), function(i) {
    if (is.na(idx[i])) return(samples[i])
    paste(vapply(columns, function(cl) as.character(meta$table[[cl]][idx[i]]),
                 character(1)), collapse = "_")
  }, character(1))
  lab
}
