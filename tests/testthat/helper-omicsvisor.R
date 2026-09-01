# ─────────────────────────────────────────────────────────
# OmicsVisor - Test helpers: app loading + simulated datasets
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(dplyr)
  library(data.table)
  library(stringr)
  library(pheatmap)
  library(ggrepel)
  library(VennDiagram)
  library(UpSetR)
  library(ggbeeswarm)
  library(umap)
  library(openxlsx)
})

# VennDiagram writes a .log file per draw.*.venn() call unless futile.logger is
# muted. Keep the test output directory clean.
suppressPackageStartupMessages(
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
)

# ── Locate and source the application code ───────────────────────────────────
ov_root <- function() {
  r <- Sys.getenv("OMICSVISOR_ROOT", "")
  if (nzchar(r) && file.exists(file.path(r, "app.R"))) return(r)
  p <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:6) {
    if (file.exists(file.path(p, "app.R"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  stop("Could not locate the OmicsVisor project root.")
}

OV_ROOT <- ov_root()

# Source everything app.R sources, in the same order, but never app.R itself
# (it ends in shinyApp() which would try to start a server).
ov_source_all <- function() {
  files <- c(
    "version.R", "helper_functions.R",
    "data_overview_module.R", "volcano_plot_module.R", "heatmap_module.R",
    "venndi_module.R", "volcano_printer_module.R", "pca_module.R",
    "id_list_generator_module.R", "regex_tool_module.R",
    "documentation_module.R", "disclaimer_module.R", "donut_plot_module.R",
    "boxplot_module.R", "scatterplot_module.R", "one_d_enrichment_module.R",
    "gct_export_module.R", "upset_plot_module.R", "correlation_module.R",
    "about_module.R"
  )
  for (f in files) source(file.path(OV_ROOT, f), local = globalenv())
  invisible(files)
}

ov_source_all()

# ── Simulated data generators ────────────────────────────────────────────────
# All generators return a plain data.frame in OmicsVisor's expected layout.
# Wrap them with ov_bundle() to get the list that app.R's `data` reactive
# hands to every module.

#' Build the reactive payload that modules receive.
#'
#' @param df           data.frame in OmicsVisor layout
#' @param int_regex    intensity-column regex (as in the sidebar)
ov_bundle <- function(df, int_regex = "^Imputed") {
  list(
    data           = df,
    logFC_cols     = grep("logFC",              names(df), value = TRUE, ignore.case = TRUE),
    adjP_cols      = grep("adj\\.?p|fdr|q\\.?val", names(df), value = TRUE, ignore.case = TRUE),
    intensity_cols = grep(int_regex,            names(df), value = TRUE, ignore.case = TRUE)
  )
}

#' A well-formed, fully populated dataset — the happy path.
#'
#' @param n_features  number of rows
#' @param comparisons character vector of "x.over.y" comparison names
#' @param groups      named list: group label -> number of replicates
#' @param na_frac     fraction of logFC/adj.P cells set to NA
#' @param int_na_frac fraction of intensity cells set to NA
#' @param int_prefix  prefix for intensity columns
#' @param seed        RNG seed
sim_omics <- function(n_features  = 200,
                      comparisons = c("KO.over.WT", "TRT.over.WT", "KO.over.TRT"),
                      groups      = list(WT = 3, KO = 3, TRT = 3),
                      na_frac     = 0,
                      int_na_frac = 0,
                      int_prefix  = "Imputed",
                      seed        = 42) {
  set.seed(seed)

  genes <- sprintf("GENE%04d", seq_len(n_features))
  acc   <- sprintf("P%05d", seq_len(n_features))

  df <- data.frame(
    id            = paste0(genes, "_", acc),
    Genes         = genes,
    Protein.Group = acc,
    stringsAsFactors = FALSE
  )

  for (cmp in comparisons) {
    lfc <- rnorm(n_features, mean = 0, sd = 0.4)
    # Plant a clearly separated set of true hits so that BH adjustment still
    # leaves plenty of features past the default cutoffs. Without this the
    # fixtures produce near-empty hit sets and the cutoff tests are vacuous.
    hit <- sample(n_features, min(n_features, max(2L, floor(n_features * 0.12))))
    lfc[hit] <- sample(c(-1, 1), length(hit), replace = TRUE) *
      runif(length(hit), 2.5, 6)

    pv       <- runif(n_features, 0.05, 1)
    pv[hit]  <- runif(length(hit), 0, 1e-8)
    adjp     <- p.adjust(pv, method = "BH")

    if (na_frac > 0) {
      k <- floor(n_features * na_frac)
      if (k > 0) {
        lfc[sample(n_features, k)]  <- NA_real_
        adjp[sample(n_features, k)] <- NA_real_
      }
    }

    df[[paste0("logFC_",      cmp)]] <- lfc
    df[[paste0("t_",          cmp)]] <- lfc / 0.4
    df[[paste0("P.Value_",    cmp)]] <- pv
    df[[paste0("adj.P.Val_",  cmp)]] <- adjp
  }

  # Intensity columns: <prefix>.<group>_<replicate>
  int_names <- unlist(lapply(names(groups), function(g)
    sprintf("%s.%s_%02d", int_prefix, g, seq_len(groups[[g]]))))
  for (cn in int_names) {
    v <- rnorm(n_features, mean = 20, sd = 2)
    if (int_na_frac > 0) {
      k <- floor(n_features * int_na_frac)
      if (k > 0) v[sample(n_features, k)] <- NA_real_
    }
    df[[cn]] <- v
  }

  df
}

# ── Edge-case fixtures ───────────────────────────────────────────────────────

#' Dataset with no logFC/adj.P columns at all (intensity-only matrix).
sim_no_comparisons <- function(n_features = 50) {
  df <- sim_omics(n_features = n_features, comparisons = character(0))
  df
}

#' Dataset whose intensity regex matches nothing (like a phospho QQ export
#' with raw group_replicate column names and no Imputed./Intensity prefix).
sim_no_intensity <- function(n_features = 50) {
  df <- sim_omics(n_features = n_features, int_prefix = "Raw")
  df
}

#' Dataset with duplicated ids — IDs are assumed unique across the app.
sim_duplicate_ids <- function(n_features = 20) {
  df <- sim_omics(n_features = n_features)
  df$id[2] <- df$id[1]
  df
}

#' Single-row dataset.
sim_single_row <- function() sim_omics(n_features = 1)

#' Dataset where the intensity columns are character, not numeric — happens
#' when an upstream export writes "NaN"/"Filtered" into the matrix.
sim_character_intensities <- function(n_features = 30) {
  df <- sim_omics(n_features = n_features)
  int <- grep("^Imputed", names(df), value = TRUE)
  for (cn in int) {
    v <- as.character(round(df[[cn]], 3))
    v[sample(n_features, 3)] <- "NaN"
    df[[cn]] <- v
  }
  df
}

#' Dataset with non-syntactic intensity column names (spaces, leading digits).
sim_awkward_colnames <- function(n_features = 30) {
  df <- sim_omics(n_features = n_features)
  int <- grep("^Imputed", names(df), value = TRUE)
  names(df)[match(int, names(df))] <-
    sub("^Imputed\\.", "Imputed ", int)   # "Imputed.WT_01" -> "Imputed WT_01"
  df
}

#' Dataset where an entire logFC column is NA.
sim_all_na_column <- function(n_features = 40) {
  df <- sim_omics(n_features = n_features)
  df$logFC_KO.over.WT     <- NA_real_
  df$adj.P.Val_KO.over.WT <- NA_real_
  df
}

#' Dataset with Inf / -Inf logFC values (division-by-zero upstream).
sim_infinite_values <- function(n_features = 40) {
  df <- sim_omics(n_features = n_features)
  df$logFC_KO.over.WT[1:3] <- c(Inf, -Inf, NaN)
  df
}

#' Comparison names that do not follow the x.over.y convention.
sim_non_over_comparisons <- function(n_features = 30) {
  sim_omics(n_features = n_features, comparisons = c("ContrastA", "ContrastB"))
}

#' Write a data.frame out as .xlsx and return the path (for upload round-trips).
sim_write_xlsx <- function(df, path = tempfile(fileext = ".xlsx")) {
  openxlsx::write.xlsx(df, path, overwrite = TRUE)
  path
}

#' Minimal GMT file for the ID List Generator / 1D Enrichment modules.
sim_write_gmt <- function(sets = NULL, path = tempfile(fileext = ".gmt")) {
  if (is.null(sets)) {
    sets <- list(
      SET_ALPHA = sprintf("GENE%04d", 1:30),
      SET_BETA  = sprintf("GENE%04d", 20:60),
      SET_TINY  = sprintf("GENE%04d", 1:2)
    )
  }
  lines <- vapply(names(sets), function(nm)
    paste(c(nm, paste0("desc of ", nm), sets[[nm]]), collapse = "\t"),
    character(1))
  writeLines(lines, path)
  path
}

# ── Small assertions used across module tests ────────────────────────────────

#' Assert that evaluating `expr` neither errors nor emits a warning.
expect_clean <- function(expr, info = NULL) {
  res <- withCallingHandlers(
    tryCatch(list(ok = TRUE, value = force(expr), err = NULL),
             error = function(e) list(ok = FALSE, value = NULL,
                                      err = conditionMessage(e))),
    warning = function(w) {
      # shiny's validate()/req() short-circuits are not warnings, so any warning
      # here is a genuine signal worth failing on.
      testthat::fail(paste0("unexpected warning: ", conditionMessage(w),
                            if (!is.null(info)) paste0(" [", info, "]") else ""))
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_true(
    res$ok,
    info = paste0(info, if (!res$ok) paste0(" — error: ", res$err))
  )
  invisible(res$value)
}

#' Force a ggplot to actually build (catches errors deferred until render).
build_gg <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  invisible(ggplot2::ggplot_build(p))
}

#' Force a plotly object to build.
build_plotly <- function(p) {
  invisible(plotly::plotly_build(p))
}

# Inside shiny::testServer() reading `output$foo` for a downloadHandler already
# runs the content function and returns the path of the written file, so tests
# assert on the path directly rather than invoking the handler themselves.
