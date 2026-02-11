# gct_export_module.R
# GCT v1.2 export (collapse to unique genes from multiple logFC_ or t_ columns)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || !isTRUE(nzchar(a))) b else a

gct_export_ui <- function(id, title = "GCT Export (v1.2)") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        h3(title),
        helpText("Export a GCT v1.2 matrix for 1D Enrichment\n(rows = unique genes, columns = logFC_ or t_ columns)."),
        
        
        div(tags$b("ID column:"), " id"),  # fixed
        
        uiOutput(ns("gene_col_ui")),    # only columns containing 'gene' (case-insensitive)
        uiOutput(ns("metric_col_ui")),  # columns starting with 'logFC_' or 't_'
        uiOutput(ns("desc_col_ui")),    # <none> by default
        
        # Gene cleanup regex
        textInput(
          ns("gene_clean_regex"),
          "Gene cleanup regex (applied with sub, removed from the end)",
          value = ";.*$"
        ),
        
        hr(),
        # Aggregation of numeric values
        selectInput(
          ns("method"), "Aggregation method (per gene, per column)",
          choices = c(
            "Highest |value| (default)" = "absmax",
            "Median"                   = "median",
            "Mean"                     = "mean"
          ),
          selected = "absmax"
        ),
        
        # NEW: strategy for mapping proteins → gene row
        selectInput(
          ns("collapse_mode"), "Gene-level strategy",
          choices = c(
            "Per column: strongest per column (chimeric row)" = "per_column",
            "Single representative: one protein for all columns" = "single_row"
          ),
          selected = "per_column"
        ),
        
        # NEW: short description of the collapsing strategies
        uiOutput(ns("collapse_help_ui")),
        
        
        checkboxInput(ns("require_gene_nonempty"), "Require non-empty gene names", TRUE),
        
        textInput(ns("outfile"), "Output file name", value = "export_logFC_matrix.gct"),
        
        hr(),
        downloadButton(ns("dl_gct"), "Download GCT v1.2"),
        br(), br(),
        verbatimTextOutput(ns("summary_txt"))
      ),
      column(
        width = 9,
        h4("Preview"),
        DT::dataTableOutput(ns("preview"))  # full table (paginated/searchable)
      )
    )
  )
}

gct_export_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    req_data <- reactive({
      d <- data()
      validate(need(!is.null(d) && !is.null(d$data), "No data available"))
      as.data.frame(d$data, stringsAsFactors = FALSE)
    })
    
    
    # --- Short explanatory text for collapsing strategies -------------------
    output$collapse_help_ui <- renderUI({
      mode <- input$collapse_mode %||% "per_column"
      
      txt <- if (mode == "single_row") {
        HTML(
          "<small><b>Gene-level strategy:</b> Single representative protein per gene.<br/>
       One protein is selected (highest mean |value| across all selected columns).<br/>
       All columns for that gene use the raw values from this same protein.<br/>
       <b>Important:</b> In this mode the aggregation method (median/mean/absmax) is <u>not applied</u>.</small>"
        )
      } else {
        HTML(
          "<small><b>Gene-level strategy:</b> Per-column aggregation (chimeric rows).<br/>
       For each gene and each column, values are aggregated independently<br/>
       using the selected method (highest |value|, median, or mean).<br/>
       Different columns of the same gene may come from different proteins/IDs.<br/>
       <b>Aggregation method is applied separately to each column.</b></small>"
        )
      }
      
      div(style = "margin-bottom: 10px; color: #555;", txt)
    })
    
    # --- Column selectors ----------------------------------------------------
    output$gene_col_ui <- renderUI({
      nms <- names(req_data())
      gene_like <- nms[grepl("(?i)gene", nms)]
      selectInput(ns("gene_col"), "Gene symbol column", choices = gene_like %||% nms)
    })
    
    # Value columns: prefer all ^logFC_ columns, else ^t_ columns
    output$metric_col_ui <- renderUI({
      nms <- names(req_data())
      logfc_only <- nms[grepl("^logFC_", nms)]
      t_only     <- nms[grepl("^t_",     nms)]
      
      choices <- c(logfc_only, t_only)
      default <- if (length(logfc_only) > 0) logfc_only else t_only
      
      validate(
        need(length(default) > 0,
             "No columns starting with 'logFC_' or 't_' found in the data.")
      )
      
      selectInput(
        ns("metric_cols"),
        "Value columns (default: all logFC_ or, if absent, all t_)",
        choices = choices,
        selected = default,
        multiple = TRUE
      )
    })
    
    output$desc_col_ui <- renderUI({
      nms <- names(req_data())
      selectInput(ns("desc_col"), "Row description column (optional)",
                  choices = c("<none>", nms), selected = "<none>")
    })
    
    # Keep outfile synced with selected metric columns (use first one as label)
    observeEvent(input$metric_cols, ignoreInit = FALSE, {
      cols <- input$metric_cols
      if (!is.null(cols) && length(cols) > 0) {
        prefix <- if (any(grepl("^logFC_", cols))) "logFC" else if (any(grepl("^t_", cols))) "t" else "matrix"
        updateTextInput(session, "outfile",
                        value = paste0("export_", prefix, "_matrix.gct"))
      }
    })
    
    # --- GCT writer ----------------------------------------------------------
    write_gct_12 <- function(path, mat, row_ids, row_desc = NULL, col_names = NULL) {
      stopifnot(is.matrix(mat))
      if (nrow(mat) == 0) stop("No rows to write to GCT.")
      if (is.null(col_names)) col_names <- colnames(mat)
      if (is.null(col_names)) col_names <- paste0("C", seq_len(ncol(mat)))
      if (is.null(row_desc)) row_desc <- rep("", nrow(mat))
      if (length(row_desc) != nrow(mat)) row_desc <- rep_len(row_desc, nrow(mat))
      
      con <- file(path, open = "wt"); on.exit(close(con), add = TRUE)
      writeLines("#1.2", con)
      writeLines(sprintf("%d\t%d", nrow(mat), ncol(mat)), con)
      hdr <- c("Name", "Description", col_names)
      writeLines(paste(hdr, collapse = "\t"), con)
      for (i in seq_len(nrow(mat))) {
        writeLines(
          paste(c(row_ids[i], row_desc[i], as.character(mat[i, ])), collapse = "\t"),
          con
        )
      }
      invisible(path)
    }
    
    # --- Collapse to gene level ---------------------------------------------
    collapse_df <- reactive({
      d <- req_data()
      validate(need(nrow(d) > 0, "Input table is empty."))
      
      id_col      <- "id"
      gene_col    <- input$gene_col
      metric_cols <- input$metric_cols
      desc_col    <- if (identical(input$desc_col, "<none>")) NULL else input$desc_col
      
      validate(need(id_col      %in% names(d), "Missing column: id"))
      validate(need(gene_col    %in% names(d), sprintf("Missing gene column: %s", gene_col)))
      validate(need(!is.null(metric_cols) && length(metric_cols) > 0,
                    "No value columns selected (logFC_ or t_)."))
      validate(need(all(metric_cols %in% names(d)),
                    "One or more selected value columns are not present in the data."))
      
      keep <- unique(c(id_col, gene_col, metric_cols, if (!is.null(desc_col)) desc_col))
      df <- d[, keep, drop = FALSE]
      
      # Clean gene names using user-provided regex (default ";.*$")
      clean_pat <- input$gene_clean_regex %||% ";.*$"
      if (nzchar(clean_pat)) {
        token <- sub(clean_pat, "", trimws(as.character(df[[gene_col]])))
      } else {
        token <- trimws(as.character(df[[gene_col]]))
      }
      
      empty_mask <- is.na(token) | !nzchar(token)
      valid_gene_like <- grepl("^[A-Za-z0-9_.-]+$", token)
      non_gene_like_mask <- !empty_mask & !valid_gene_like
      
      if (isTRUE(input$require_gene_nonempty)) {
        df <- df[!empty_mask, , drop = FALSE]
        token <- token[!empty_mask]
        non_gene_like_mask <- non_gene_like_mask[!empty_mask]
      }
      validate(need(nrow(df) > 0, "No rows remain after filtering for non-empty gene names."))
      
      # Convert metric columns to numeric
      for (mc in metric_cols) {
        df[[mc]] <- suppressWarnings(as.numeric(df[[mc]]))
      }
      
      mat_vals <- as.matrix(df[, metric_cols, drop = FALSE])
      # Keep rows where at least one metric column is finite
      row_ok <- apply(mat_vals, 1, function(x) any(is.finite(x)))
      df <- df[row_ok, , drop = FALSE]
      token <- token[row_ok]
      non_gene_like_mask <- non_gene_like_mask[row_ok]
      validate(need(nrow(df) > 0, "No finite values found in the selected value columns."))
      
      df$.__gene__ <- token
      
      method        <- input$method
      collapse_mode <- input$collapse_mode
      groups        <- split(seq_len(nrow(df)), df$.__gene__)
      
      collapsed_list <- lapply(groups, function(idx) {
        sub  <- df[idx, , drop = FALSE]
        vals <- as.matrix(sub[, metric_cols, drop = FALSE])
        
        ## --- choose representative row if needed (single_row mode) ----
        # representative row = protein with highest mean |value| across all selected columns
        rep_idx_local <- which.max(
          apply(vals, 1, function(v) mean(abs(v), na.rm = TRUE))
        )
        
        if (collapse_mode == "single_row") {
          # all columns from the same protein/ID
          agg_vals <- vals[rep_idx_local, ]
          names(agg_vals) <- metric_cols
          
          desc <- ""
          if (!is.null(desc_col)) {
            desc <- sub[[desc_col]][rep_idx_local]
          }
        } else {
          # collapse_mode == "per_column"
          # Aggregate per gene, per column (chimeric row possible)
          agg_vals <- switch(
            method,
            "median" = apply(vals, 2, stats::median, na.rm = TRUE),
            "mean"   = apply(vals, 2, mean, na.rm = TRUE),
            "absmax" = apply(vals, 2, function(v) {
              if (all(is.na(v))) NA_real_ else v[which.max(abs(v))]
            }),
            # fallback
            apply(vals, 2, function(v) {
              if (all(is.na(v))) NA_real_ else v[which.max(abs(v))]
            })
          )
          
          # Description: use row with strongest signal in the first metric column (by |value|)
          desc <- ""
          if (!is.null(desc_col)) {
            v1 <- sub[[metric_cols[1]]]
            v1[is.na(v1)] <- 0
            desc <- sub[[desc_col]][which.max(abs(v1))]
          }
        }
        
        data.frame(
          gene = sub$.__gene__[1],
          desc = desc,
          t(agg_vals),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })
      
      collapsed <- do.call(rbind, collapsed_list)
      rownames(collapsed) <- NULL
      
      attr(collapsed, "diag") <- list(
        rows_in          = nrow(d),
        empty_gene_count = sum(empty_mask, na.rm = TRUE),
        non_gene_like    = sum(non_gene_like_mask, na.rm = TRUE),
        metric_cols      = metric_cols,
        collapse_mode    = collapse_mode
      )
      
      collapsed
    })
    
    # --- Preview table -------------------------------------------------------
    output$preview <- DT::renderDataTable({
      if (!requireNamespace("DT", quietly = TRUE)) stop("Please install.packages('DT')")
      DT::datatable(
        collapse_df(),
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, 200),
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # --- Summary information -------------------------------------------------
    output$summary_txt <- renderPrint({
      d   <- req_data()
      cdf <- collapse_df()
      dg  <- attr(cdf, "diag")
      metric_cols   <- dg$metric_cols   %||% input$metric_cols
      collapse_mode <- dg$collapse_mode %||% input$collapse_mode
      
      cat("Rows in input:", if (!is.null(dg$rows_in)) dg$rows_in else nrow(d), "\n")
      cat("Unique genes exported:", nrow(cdf), "\n")
      cat("Empty gene names:", dg$empty_gene_count %||% 0, "\n")
      cat("Non-gene-like symbols:", dg$non_gene_like %||% 0, "\n")
      cat("Aggregation method (per gene, per column):",
          switch(input$method,
                 absmax = "Highest |value|",
                 median = "Median",
                 mean   = "Mean"),
          "\n")
      cat("Gene-level strategy:",
          if (collapse_mode == "single_row") {
            "Single representative protein per gene (same row used for all columns)"
          } else {
            "Per column strongest/aggregated values (different proteins can contribute per column)"
          },
          "\n")
      cat("Value columns used (logFC_ / t_):",
          if (!is.null(metric_cols)) paste(metric_cols, collapse = ", ") else "none",
          "\n")
      cat("Gene cleanup regex:", input$gene_clean_regex %||% "", "\n")
    })
    
    # --- Download GCT --------------------------------------------------------
    output$dl_gct <- downloadHandler(
      filename = function() input$outfile %||% "export_logFC_matrix.gct",
      content = function(file) {
        cdf <- collapse_df()
        validate(need(nrow(cdf) > 0, "No rows to export."))
        
        # Build matrix from all metric columns
        metric_cols <- attr(cdf, "diag")$metric_cols %||% input$metric_cols
        validate(need(!is.null(metric_cols) && length(metric_cols) > 0,
                      "No metric columns available for export."))
        
        mat <- as.matrix(cdf[, metric_cols, drop = FALSE])
        rownames(mat) <- cdf$gene
        
        write_gct_12(
          path      = file,
          mat       = mat,
          row_ids   = cdf$gene,
          row_desc  = cdf$desc,
          col_names = colnames(mat)
        )
      }
    )
  })
}