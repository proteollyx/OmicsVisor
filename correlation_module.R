# ─────────────────────────────────────────────────────────
# OmicsVisor - Feature Correlation Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

correlation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Feature Correlation Module"),
    p(
      "Correlates every feature against one selected reference feature across",
      "the chosen intensity columns. Results are ranked by |r| and displayed",
      "as a ranked correlation plot. P-values are BH-adjusted."
    ),

    fluidRow(

      # ── Controls ────────────────────────────────────────────────────────────
      column(
        width = 5,
        wellPanel(

          h4("Reference Feature"),
          textInput(ns("ref_feature"), "Feature ID:",
                    placeholder = "Exact ID from the id column"),
          helpText("Enter a unique feature ID exactly as it appears in the id column (e.g. from the ID List Generator)."),
          actionButton(ns("run_corr"), "Run Correlation",
                       class = "btn-primary btn-block"),

          hr(),
          h4("Intensity Columns"),
          selectInput(ns("intensity_columns"), "Select columns:",
                      choices = NULL, multiple = TRUE),
          fluidRow(
            column(6, actionButton(ns("select_all"),   "Select All",   class = "btn-sm")),
            column(6, actionButton(ns("deselect_all"), "Deselect All", class = "btn-sm"))
          ),

          hr(),
          h4("Settings"),
          radioButtons(ns("corr_method"), "Correlation method:",
                       choices  = c("Spearman" = "spearman",
                                    "Pearson"  = "pearson"),
                       selected = "spearman",
                       inline   = TRUE),
          selectInput(ns("label_col"), "Label column:", choices = NULL),
          numericInput(ns("r_threshold"),
                       "|r| threshold (labelling & highlight):",
                       value = 0.7, min = 0, max = 1, step = 0.05),
          numericInput(ns("adjp_threshold"),
                       "adj.p threshold (labelling & highlight):",
                       value = 0.1, min = 0, max = 1, step = 0.01),
          numericInput(ns("point_size"), "Point size:", value = 1.5,
                       min = 0.5, step = 0.5),
          numericInput(ns("label_size"), "Label size:", value = 2.5,
                       min = 1, step = 0.5),
          numericInput(ns("pdf_width"),  "PDF width (in):",  value = 10, min = 4),
          numericInput(ns("pdf_height"), "PDF height (in):", value = 6,  min = 3),

          hr(),
          downloadButton(ns("download_pdf"), "Download Plot (PDF)"),
          br(), br(),
          downloadButton(ns("download_csv"), "Download Results (CSV)")
        )
      ),

      # ── Output ───────────────────────────────────────────────────────────────
      column(
        width = 9,
        h4("Ranked Correlation Plot"),
        plotOutput(ns("corr_plot"), height = "500px"),
        br(),
        h4("Correlation Results"),
        DT::dataTableOutput(ns("results_table"))
      )
    )
  )
}


correlation_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # ── Populate inputs ────────────────────────────────────────────────────────
    observe({
      req(data())
      col_names <- names(data()$data)

      updateSelectInput(session, "intensity_columns",
                        choices = data()$intensity_cols)

      default_label <- if ("id" %in% col_names) "id" else col_names[1]
      updateSelectInput(session, "label_col",
                        choices  = col_names,
                        selected = default_label)
    })

    observeEvent(input$select_all, {
      updateSelectInput(session, "intensity_columns",
                        selected = data()$intensity_cols)
    })
    observeEvent(input$deselect_all, {
      updateSelectInput(session, "intensity_columns",
                        selected = character(0))
    })

    # ── Correlation computation (button-triggered) ─────────────────────────────
    corr_results <- eventReactive(input$run_corr, {
      req(input$intensity_columns, input$ref_feature)

      df       <- data()$data
      int_cols <- input$intensity_columns

      validate(
        need("id" %in% names(df),
             "No 'id' column found in the dataset."),
        need(length(int_cols) >= 3,
             "Select at least 3 intensity columns to compute meaningful correlations.")
      )

      # Build numeric matrix: rows = features, columns = samples
      mat <- apply(as.matrix(df[, int_cols, drop = FALSE]), 2, as.numeric)
      rownames(mat) <- df$id

      # Locate the reference row — exact match against the id column only --------
      feature_name <- trimws(input$ref_feature)
      ref_idx      <- which(df$id == feature_name)

      validate(
        need(length(ref_idx) > 0,
             paste0("'", feature_name, "' not found in the id column. ",
                    "Use an exact ID as shown in the ID List Generator.")),
        need(length(ref_idx) == 1,
             paste0("'", feature_name, "' matches ", length(ref_idx),
                    " rows — IDs must be unique."))
      )

      ref_vec <- as.numeric(mat[ref_idx, ])

      validate(
        need(sd(ref_vec, na.rm = TRUE) > 0,
             "The reference feature has zero variance — cannot compute correlations.")
      )

      # Per-row correlation -------------------------------------------------------
      n_rows <- nrow(mat)
      cor_r  <- numeric(n_rows)
      cor_p  <- numeric(n_rows)

      for (i in seq_len(n_rows)) {
        row_vec <- as.numeric(mat[i, ])
        if (is.na(sd(row_vec, na.rm = TRUE)) || sd(row_vec, na.rm = TRUE) == 0) {
          cor_r[i] <- NA; cor_p[i] <- NA; next
        }
        ct <- tryCatch(
          cor.test(row_vec, ref_vec,
                   method = input$corr_method,
                   use    = "pairwise.complete.obs"),
          error = function(e) NULL
        )
        if (is.null(ct)) {
          cor_r[i] <- NA; cor_p[i] <- NA
        } else {
          cor_r[i] <- ct$estimate
          cor_p[i] <- ct$p.value
        }
      }

      adj_p <- p.adjust(cor_p, method = "BH")

      # Assemble results table ---------------------------------------------------
      results <- data.frame(
        id           = df$id,
        r            = round(cor_r, 4),
        p.value      = round(cor_p,  6),
        adj.p.value  = round(adj_p,  6),
        is_reference = seq_len(n_rows) == ref_idx,
        stringsAsFactors = FALSE
      )

      # Insert label column (with its real name) if different from id
      label_col_name <- input$label_col
      if (label_col_name %in% names(df)) {
        results[[label_col_name]] <- as.character(df[[label_col_name]])
      }

      # Sort by |r| descending; reference row is kept but flagged
      results <- results[order(abs(results$r), decreasing = TRUE, na.last = TRUE), ]
      results$rank <- seq_len(nrow(results))

      list(
        results        = results,
        ref_id         = df$id[ref_idx],
        ref_label      = as.character(df[[input$label_col]][ref_idx]),
        label_col_name = label_col_name,
        method         = input$corr_method,
        n_samples      = length(int_cols)
      )
    })

    # ── Plot ─────────────────────────────────────────────────────────────────
    make_plot <- reactive({
      res       <- corr_results()
      results   <- res$results
      r_thr     <- input$r_threshold
      adjp_thr  <- input$adjp_threshold

      # Exclude the reference row itself from the plot
      plot_df <- results[!results$is_reference & !is.na(results$r), ]
      plot_df$neg_log10_adjp <- -log10(pmax(plot_df$adj.p.value, 1e-300))
      plot_df$significant    <- !is.na(plot_df$adj.p.value) &
        plot_df$adj.p.value < adjp_thr &
        abs(plot_df$r) >= r_thr

      label_col <- res$label_col_name
      plot_df$plot_label <- ifelse(
        plot_df$significant,
        if (label_col %in% names(plot_df)) plot_df[[label_col]] else plot_df$id,
        NA_character_
      )

      ggplot(plot_df, aes(x = rank, y = r, colour = neg_log10_adjp)) +
        geom_point(size = input$point_size, alpha = 0.65) +
        geom_hline(yintercept = c(-r_thr, r_thr),
                   linetype = "dashed", colour = "grey40", linewidth = 0.45) +
        geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
        scale_colour_gradient(
          low  = "grey75",
          high = "firebrick",
          name = expression(-log[10](adj.p))
        ) +
        ggrepel::geom_text_repel(
          aes(label = plot_label),
          size            = input$label_size,
          max.overlaps    = 30,
          na.rm           = TRUE,
          box.padding     = 0.3,
          segment.colour  = "grey50"
        ) +
        labs(
          title    = paste0(
            "Proteome correlation with ", res$ref_label,
            "  (", res$method, ")"
          ),
          subtitle = sprintf(
            "n = %d features | %d samples | dashed: |r| = %.2f | labels: adj.p < %.2f & |r| ≥ %.2f",
            nrow(plot_df), res$n_samples, r_thr, adjp_thr, r_thr
          ),
          x = "Rank (by |r|)",
          y = paste0(tools::toTitleCase(res$method), " r")
        ) +
        theme_bw(base_size = 12) +
        theme(legend.position = "right")
    })

    output$corr_plot <- renderPlot({
      make_plot()
    }, width = 800, height = 600)

    # ── Results table ──────────────────────────────────────────────────────────
    output$results_table <- DT::renderDataTable({
      res <- corr_results()
      tbl <- res$results
      tbl$is_reference <- NULL

      DT::datatable(
        tbl,
        rownames = FALSE,
        options  = list(pageLength = 25, scrollX = TRUE,
                        lengthMenu = c(10, 25, 50, 100))
      ) |>
        DT::formatRound(columns = c("r", "p.value", "adj.p.value"), digits = 4) |>
        DT::formatStyle(
          "rank",
          target          = "row",
          backgroundColor = DT::styleEqual(1, "#fff3cd")
        )
    })

    # ── Downloads ──────────────────────────────────────────────────────────────
    safe_name <- function(label) gsub("[^A-Za-z0-9_]", "_", label)

    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("correlation_", safe_name(corr_results()$ref_label),
               "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        ggsave(file, plot = make_plot(), device = "pdf",
               width = input$pdf_width, height = input$pdf_height)
      }
    )

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("correlation_", safe_name(corr_results()$ref_label),
               "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        tbl <- corr_results()$results
        tbl$is_reference <- NULL
        utils::write.csv(tbl, file, row.names = FALSE)
      }
    )
  })
}
