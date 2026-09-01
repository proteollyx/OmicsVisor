# ─────────────────────────────────────────────────────────
# OmicsVisor - Real-data smoke harness
# Author: Oliver Popp
#
# Drives every module against real QQ_Results-style result tables and reports
# any error or warning. The data itself is never part of the repository, so
# this is deliberately kept out of tests/testthat/ and out of CI.
#
# Usage:
#   Rscript tests/manual/real_data_smoke.R <dir-or-file> [<dir-or-file> ...]
#   Rscript tests/manual/real_data_smoke.R            # uses OV_TEST_DATA
#
# Every path given is scanned recursively for *QQ_Results*.xlsx.
# ─────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny); library(ggplot2); library(plotly); library(DT)
  library(dplyr); library(data.table); library(stringr); library(pheatmap)
  library(ggrepel); library(VennDiagram); library(UpSetR); library(ggbeeswarm)
  library(umap); library(openxlsx)
})
suppressPackageStartupMessages(
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
)

OV_ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=",
  commandArgs(trailingOnly = FALSE), value = TRUE)[1])), "..", ".."),
  mustWork = FALSE)
if (!file.exists(file.path(OV_ROOT, "app.R"))) OV_ROOT <- normalizePath(getwd())

for (f in c("version.R", "helper_functions.R",
            "data_overview_module.R", "volcano_plot_module.R", "heatmap_module.R",
            "venndi_module.R", "volcano_printer_module.R", "pca_module.R",
            "id_list_generator_module.R", "regex_tool_module.R",
            "documentation_module.R", "disclaimer_module.R", "donut_plot_module.R",
            "boxplot_module.R", "scatterplot_module.R", "one_d_enrichment_module.R",
            "gct_export_module.R", "upset_plot_module.R", "correlation_module.R",
            "about_module.R"))
  source(file.path(OV_ROOT, f))

# ── Result collection ────────────────────────────────────────────────────────
RESULTS <- new.env(parent = emptyenv())
RESULTS$rows <- list()
RESULTS$out  <- Sys.getenv("OV_SMOKE_OUT",
                           file.path(tempdir(), "ov_smoke_results.tsv"))

# Append every row as it is produced. A long sweep that is interrupted (or
# killed) then still leaves usable results on disk instead of nothing.
record <- function(file, module, check, status, detail = "") {
  row <- data.frame(
    file = file, module = module, check = check,
    status = status, detail = gsub("[\r\n\t]+", " ", substr(detail, 1, 300)),
    stringsAsFactors = FALSE
  )
  RESULTS$rows[[length(RESULTS$rows) + 1L]] <- row
  utils::write.table(row, RESULTS$out, sep = "\t", row.names = FALSE,
                     col.names = !file.exists(RESULTS$out), append = TRUE,
                     quote = FALSE)
  if (status == "ERROR")
    message("  ERROR  ", module, " / ", check, " :: ", substr(detail, 1, 160))
  invisible(NULL)
}

# Run `expr`; classify as OK / VALIDATE (a deliberate validate()/req() stop) /
# WARN / ERROR. validate() messages are the app telling the user something —
# they are not defects.
probe <- function(file, module, check, expr) {
  warns <- character(0)
  res <- withCallingHandlers(
    tryCatch({ force(expr); list(ok = TRUE, msg = "") },
             shiny.silent.error = function(e)
               list(ok = NA, msg = conditionMessage(e)),
             error = function(e) list(ok = FALSE, msg = conditionMessage(e))),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")
    }
  )

  # ggplot/plotly routinely report dropped NA rows; that is expected on real data.
  benign <- "Removed [0-9]+ row|Ignoring [0-9]+ observation|non-finite|deprecated|UpSetR|linewidth|aes_string|plotly_click|Returning the palette|rows containing missing"
  warns <- warns[!grepl(benign, warns)]

  status <- if (isFALSE(res$ok)) "ERROR"
            else if (length(warns) > 0) "WARN"
            else if (is.na(res$ok)) "VALIDATE"
            else "OK"
  detail <- if (isFALSE(res$ok)) res$msg
            else if (length(warns) > 0) paste(unique(warns), collapse = " | ")
            else if (is.na(res$ok)) res$msg else ""
  record(file, module, check, status, detail)
  invisible(res$ok)
}

# ── Per-file exercise of every module ────────────────────────────────────────
exercise_file <- function(path) {
  tag <- basename(path)
  message("\n=== ", tag, " (", dirname(path), ")")

  df <- tryCatch(ov_read_upload(path, path), error = function(e) e)
  if (inherits(df, "error")) {
    record(tag, "upload", "read file", "ERROR", conditionMessage(df)); return(invisible())
  }
  record(tag, "upload", "read file", "OK",
         sprintf("%d rows x %d cols", nrow(df), ncol(df)))

  # Try the two shipped intensity presets, keep the one that matches something.
  bundles <- list()
  for (rx in c("^Imputed", "^Intensity", "")) {
    b <- c(list(data = df), ov_detect_columns(df, rx))
    if (length(b$intensity_cols) > 0) { bundles[[rx]] <- b; break }
  }
  if (length(bundles) == 0) bundles[["^Imputed"]] <-
    c(list(data = df), ov_detect_columns(df, "^Imputed"))

  bundle   <- bundles[[1]]
  rx_used  <- names(bundles)[1]
  int_cols <- bundle$intensity_cols
  comps    <- detect_comparisons(names(df))
  record(tag, "detect", "columns", if (length(comps) > 0) "OK" else "WARN",
         sprintf("regex='%s' | %d comparisons | %d intensity cols | id=%s",
                 rx_used, length(comps), length(int_cols),
                 "id" %in% names(df)))

  data_r <- function() bundle
  R <- function(x) shiny::reactive(x)

  # ── Data Overview ──────────────────────────────────────────────────────────
  testServer(data_overview_server, args = list(data = R(bundle)), {
    session$setInputs(data_preview_rows_selected = seq_len(min(5, nrow(df))))
    probe(tag, "data_overview", "selected ids", selected_ids())
    probe(tag, "data_overview", "table",        output$data_preview)
  })

  if (length(comps) > 0) {
    cmp <- comps[1]

    # ── Volcano ──────────────────────────────────────────────────────────────
    testServer(volcano_plot_server, args = list(data = R(bundle)), {
      session$setInputs(comparison_name = cmp, pval_cutoff = 0.05,
                        logfc_cutoff = 1, label_columns = character(0),
                        generate_ids = 1)
      probe(tag, "volcano", "plot", output$volcano_plot)
      probe(tag, "volcano", "id lists", {
        stopifnot(!any(is.na(id_lists$all)))
        length(id_lists$all)
      })
    })

    # ── Volcano Printer ──────────────────────────────────────────────────────
    testServer(volcano_printer_server, args = list(data = R(bundle)), {
      session$setInputs(comparison_name = cmp, pval_cutoff = 0.05,
                        logfc_cutoff = 1, label_columns = character(0),
                        label_only_sig = TRUE,
                        id_selection = paste(utils::head(df$id, 5), collapse = ", "),
                        manual_axes = FALSE, plot_width = 8, plot_height = 6)
      probe(tag, "volcano_printer", "plot", output$volcano_ggplot)
      probe(tag, "volcano_printer", "pdf",  file.size(output$download_plot) > 1000)
      probe(tag, "volcano_printer", "no NA labels",
            stopifnot(!any(is.na(plot_data()$label_display))))
    })

    # ── Donut ────────────────────────────────────────────────────────────────
    testServer(donut_plot_server, args = list(data = R(bundle)), {
      session$setInputs(logfc_cutoff = 1, pval_cutoff = 0.05, apply_cutoff = 1,
                        select_up_1 = TRUE)
      probe(tag, "donut", "selected ids", {
        s <- selected_ids(); stopifnot(!any(is.na(s))); length(s)
      })
      probe(tag, "donut", "ui", output$donut_plots_ui)
    })

    # ── UpSet ────────────────────────────────────────────────────────────────
    testServer(upset_plot_server, args = list(data = R(bundle)), {
      session$setInputs(direction = "both", logfc_cutoff = 1, adjp_cutoff = 0.05,
                        n_intersects = 40, min_set_size = 1)
      probe(tag, "upset", "membership", membership_data())
      probe(tag, "upset", "plot",       output$upset_plot)
      probe(tag, "upset", "csv",        output$download_csv)
    })

    # ── logFC Scatter ────────────────────────────────────────────────────────
    lf <- grep("^logFC_", names(df), value = TRUE)
    if (length(lf) >= 2) {
      testServer(scatterplot_server, args = list(data = R(bundle)), {
        session$setInputs(x_logfc = lf[1], y_logfc = lf[2], highlight_ids = "",
                          label_all_ids = FALSE, pval_cutoff = 0.05,
                          logfc_cutoff = 1, point_size = 2, label_size = 3,
                          plot_width = 8, plot_height = 6, lock_aspect = FALSE)
        probe(tag, "scatterplot", "data", {
          s <- scatter_data(); stopifnot(!any(is.na(s$Significance))); nrow(s)
        })
        probe(tag, "scatterplot", "plot", output$scatter_plot)
        probe(tag, "scatterplot", "pdf",  file.size(output$download_plot) > 1000)
      })
    }

    # ── GCT Export ───────────────────────────────────────────────────────────
    gene_like <- grep("(?i)gene", names(df), value = TRUE, perl = TRUE)
    if (length(gene_like) > 0 && length(lf) > 0) {
      testServer(gct_export_server, args = list(data = R(bundle)), {
        session$setInputs(gene_col = gene_like[1], metric_cols = lf,
                          desc_col = "<none>", gene_clean_regex = ";.*$",
                          method = "absmax", collapse_mode = "per_column",
                          require_gene_nonempty = TRUE,
                          outfile = "export.gct")
        probe(tag, "gct_export", "gene dropdown", {
          txt <- paste(as.character(output$gene_col_ui), collapse = " ")
          # The dropdown must not degrade into "every column in the table".
          stopifnot(!grepl("adj.P.Val", txt, fixed = TRUE))
        })
        probe(tag, "gct_export", "collapse", nrow(collapse_df()))
        probe(tag, "gct_export", "write gct", {
          p <- output$dl_gct; stopifnot(readLines(p, n = 1) == "#1.2"); p
        })
      })
    }
  } else {
    record(tag, "comparisons", "detect", "WARN", "no logFC_/adj.P.Val_ pairs")
  }

  # ── Modules that need intensity columns ────────────────────────────────────
  if (length(int_cols) >= 3) {
    ic <- utils::head(int_cols, 24)   # keep runtime sane on wide matrices

    testServer(heatmap_server, args = list(data = R(bundle)), {
      session$setInputs(id_selection = paste(utils::head(df$id, 40), collapse = ", "),
                        intensity_columns = ic, row_label_columns = character(0),
                        cluster_columns = TRUE, cluster_rows = TRUE,
                        scale_rows = TRUE, use_custom_limits = FALSE,
                        color_min = -1, color_max = 1, pdf_width = 8,
                        pdf_height = 6, fontsize_row = 8, fontsize_col = 8)
      probe(tag, "heatmap", "matrix", {
        m <- final_heatmap_data()$matrix; stopifnot(is.numeric(m)); dim(m)
      })
      probe(tag, "heatmap", "plot", output$heatmap_plot)
      probe(tag, "heatmap", "pdf",  file.size(output$download_pdf) > 1000)
      probe(tag, "heatmap", "data", output$download_data)
    })

    testServer(pca_server, args = list(data = R(bundle)), {
      session$setInputs(dr_method = "PCA", row_selection = "all",
                        id_selection = "", intensity_columns = ic,
                        pca_center = TRUE, pca_scale = TRUE,
                        color_scheme = "combined", pdf_width = 8, pdf_height = 6,
                        point_size = 3, label_size = 3, loadings_top_n = 20,
                        group_component_2 = TRUE)
      probe(tag, "pca", "results", {
        r <- pca_results()
        stopifnot(setequal(r$df$Sample, ic))   # names must survive untouched
        nrow(r$df)
      })
      session$setInputs(pca_x_pc = "PC1", pca_y_pc = "PC2")
      probe(tag, "pca", "plot",     output$pca_plot)
      probe(tag, "pca", "scree",    output$scree_plot)
      probe(tag, "pca", "loadings", output$loadings_table)
      probe(tag, "pca", "pdf",      file.size(output$download_pdf) > 1000)

      session$setInputs(dr_method = "UMAP", umap_n_neighbors = min(15, length(ic) - 1),
                        umap_min_dist = 0.1, umap_n_components = 2)
      probe(tag, "pca", "umap", output$pca_plot)
    })

    testServer(plot_server, args = list(data = R(bundle)), {
      session$setInputs(id_selection = df$id[1], intensity_columns = ic,
                        plot_type = "box", error_type = "sd",
                        show_jitter = TRUE, show_beeswarm = FALSE,
                        manual_y = FALSE, y_min = 0, y_max = 30,
                        pdf_width = 8, pdf_height = 6,
                        group_component_1 = TRUE, group_component_2 = TRUE,
                        group_component_3 = TRUE, group_component_4 = TRUE,
                        group_component_5 = TRUE)
      probe(tag, "boxplot", "grouping (all 5 components)", {
        a <- group_annotations(); stopifnot(length(a) == length(ic)); length(a)
      })
      probe(tag, "boxplot", "plot", output$protein_plot)
      probe(tag, "boxplot", "pdf",  file.size(output$download_plot) > 1000)
    })

    # Correlation is O(n_features) cor.test calls; sample the table to keep the
    # smoke run quick while still exercising the real column layout.
    small <- bundle
    small$data <- df[seq_len(min(nrow(df), 1500)), , drop = FALSE]
    testServer(correlation_server, args = list(data = R(small)), {
      session$setInputs(ref_feature = small$data$id[1], intensity_columns = ic,
                        corr_method = "spearman",
                        label_col = if ("Genes" %in% names(df)) "Genes" else "id",
                        r_threshold = 0.7, adjp_threshold = 0.1,
                        point_size = 1.5, label_size = 2.5,
                        pdf_width = 10, pdf_height = 6, run_corr = 1)
      probe(tag, "correlation", "results", nrow(corr_results()$results))
      probe(tag, "correlation", "plot",    output$corr_plot)
      probe(tag, "correlation", "csv",     output$download_csv)
    })
  } else {
    record(tag, "intensity", "detect", "WARN",
           sprintf("only %d intensity columns matched '%s'", length(int_cols), rx_used))
  }

  # ── ID List Generator ──────────────────────────────────────────────────────
  gcol <- grep("^genes$", names(df), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(gcol)) {
    probe_genes <- utils::head(stats::na.omit(unique(df[[gcol]])), 5)
    testServer(id_list_generator_server, args = list(data = R(bundle)), {
      session$setInputs(gene_list = paste(c(probe_genes, "ZZZ_NOT_A_GENE"),
                                          collapse = ", "),
                        search_column = gcol, remove_na = FALSE,
                        ignore_case = FALSE)
      probe(tag, "id_list", "no NA in output", {
        m <- matched_ids(); stopifnot(!any(is.na(m))); length(m)
      })
    })
  }

  invisible()
}

# ── Entry point ──────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) args <- strsplit(Sys.getenv("OV_TEST_DATA", ""), ":")[[1]]
args <- args[nzchar(args)]
if (length(args) == 0)
  stop("Give one or more directories/files, or set OV_TEST_DATA.", call. = FALSE)

files <- unlist(lapply(args, function(a) {
  if (dir.exists(a))
    list.files(a, pattern = "QQ_Results.*\\.xlsx$", recursive = TRUE,
               full.names = TRUE)
  else a
}))
files <- files[file.exists(files) & !grepl("^~\\$", basename(files))]
if (length(files) == 0) stop("No QQ_Results*.xlsx files found.", call. = FALSE)

if (file.exists(RESULTS$out)) unlink(RESULTS$out)

# Optional: cap the run, e.g. OV_SMOKE_LIMIT=25
lim <- suppressWarnings(as.integer(Sys.getenv("OV_SMOKE_LIMIT", "")))
if (!is.na(lim) && lim > 0 && lim < length(files)) {
  set.seed(1)
  files <- files[sort(sample(length(files), lim))]
}

message(sprintf("Exercising %d file(s)\n", length(files)))
for (f in files) {
  tryCatch(exercise_file(f),
           error = function(e)
             record(basename(f), "HARNESS", "run", "ERROR", conditionMessage(e)))
}

out     <- do.call(rbind, RESULTS$rows)
outfile <- RESULTS$out

cat("\n\n================ SUMMARY ================\n")
print(table(out$status))
bad <- out[out$status %in% c("ERROR", "WARN"), ]
if (nrow(bad) > 0) {
  cat("\n---- ERRORs and WARNs ----\n")
  print(bad[order(bad$status, bad$module), ], row.names = FALSE)
} else {
  cat("\nNo errors or unexpected warnings.\n")
}
cat("\nFull results: ", outfile, "\n", sep = "")
