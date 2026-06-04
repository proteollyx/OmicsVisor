# ─────────────────────────────────────────────────────────
# OmicsVisor - Donut Plot Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
donut_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Donut Plot Module"),
    p("Generates a donut plot for each two-group comparison using user-defined `logFC` and `Adj.P-value` cutoffs. Checkboxes allow you to select up- or downregulated candidates for each comparison and copy them to your clipboard. Within each comparison, the logic is OR (includes all selected IDs); across comparisons, the logic is AND (IDs must meet criteria in all selected comparisons), allowing you to identify overlapping candidates."),
    p("Select upregulated and/or downregulated IDs using checkboxes for each comparison. Selecting one checkbox within a comparison will include all IDs meeting that criterion, while selecting both will include all IDs meeting either criterion. Across comparisons, only IDs meeting all selected criteria (AND logic) will be included."),

    fluidRow(
      column(4, numericInput(ns("logfc_cutoff"), "Global logFC Cutoff", value = 1, min = 0)),
      column(4, numericInput(ns("pval_cutoff"), "Global Adj.P-value Cutoff", value = 0.05, min = 0, max = 1, step = 0.01)),
      column(4, actionButton(ns("apply_cutoff"), "Apply Cutoff", class = "btn-sm"))
    ),
    uiOutput(ns("donut_plots_ui")),
    fluidRow(
      column(8, verbatimTextOutput(ns("selected_ids_output"))),
      column(4, verbatimTextOutput(ns("selected_ids_count")))
    ),
    fluidRow(
      column(12, actionButton(ns("copy_selected_ids"), "Copy IDs", icon = icon("copy"), class = "btn-sm"))
    )
  )
}

donut_plot_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$apply_cutoff, {
      output$donut_plots_ui <- renderUI({
        tagList(
          lapply(seq_along(data()$logFC_cols), function(i) {
            logfc_col  <- data()$logFC_cols[i]
            adjp_col   <- gsub("logFC", "adj.P.Val", logfc_col)
            all_ids    <- data()$data$id
            down_ids   <- all_ids[data()$data[[logfc_col]] < -input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
            up_ids     <- all_ids[data()$data[[logfc_col]] >  input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
            plot_title <- gsub("\\.", " ", sub("logFC_", "", logfc_col))
            tagList(
              plotOutput(ns(paste0("donut_plot_", i)), height = "300px"),
              checkboxInput(ns(paste0("select_up_",   i)), label = paste("Select Upregulated for",   plot_title)),
              checkboxInput(ns(paste0("select_down_", i)), label = paste("Select Downregulated for", plot_title)),
              hr()
            )
          })
        )
      })

      lapply(seq_along(data()$logFC_cols), function(i) {
        logfc_col  <- data()$logFC_cols[i]
        adjp_col   <- gsub("logFC", "adj.P.Val", logfc_col)
        all_ids    <- data()$data$id
        down_ids   <- all_ids[data()$data[[logfc_col]] < -input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
        up_ids     <- all_ids[data()$data[[logfc_col]] >  input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
        plot_title <- gsub("\\.", " ", sub("logFC_", "", logfc_col))
        output[[paste0("donut_plot_", i)]] <- renderPlot({
          donut_plot(all_ids, down_ids, up_ids, plot_title)
        }, width = 600, height = 300)
      })
    })

    selected_ids <- reactive({
      any_selected <- any(sapply(seq_along(data()$logFC_cols), function(i) {
        isTRUE(input[[paste0("select_up_", i)]]) || isTRUE(input[[paste0("select_down_", i)]])
      }))
      if (!any_selected) return(character(0))

      result <- NULL
      for (i in seq_along(data()$logFC_cols)) {
        if (isTRUE(input[[paste0("select_up_", i)]]) || isTRUE(input[[paste0("select_down_", i)]])) {
          logfc_col <- data()$logFC_cols[i]
          adjp_col  <- gsub("logFC", "adj.P.Val", logfc_col)
          up_ids    <- if (isTRUE(input[[paste0("select_up_", i)]])) {
            data()$data$id[data()$data[[logfc_col]] >  input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
          } else character(0)
          down_ids  <- if (isTRUE(input[[paste0("select_down_", i)]])) {
            data()$data$id[data()$data[[logfc_col]] < -input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
          } else character(0)
          pair_ids  <- unique(c(up_ids, down_ids))
          result    <- if (is.null(result)) pair_ids else intersect(result, pair_ids)
        }
      }
      if (is.null(result)) character(0) else result
    })

    output$selected_ids_output <- renderText({
      sel <- selected_ids()
      if (length(sel) > 0) paste("Selected IDs:", paste(sel, collapse = ", "))
      else                  "No IDs meet the selected criteria."
    })

    output$selected_ids_count <- renderText({
      paste("Total Count:", length(selected_ids()))
    })

    observeEvent(input$copy_selected_ids, {
      session$sendCustomMessage("copyToClipboard", paste(selected_ids(), collapse = ", "))
    })
  })
}

# Donut Plot Helper
donut_plot <- function(all_ids, down_ids, up_ids, plot_title) {
  total_proteins <- length(all_ids)
  upregulated    <- length(up_ids)
  downregulated  <- length(down_ids)
  other          <- total_proteins - upregulated - downregulated

  plot_data <- data.frame(
    category = c("Upregulated", "Downregulated", "Other"),
    count    = c(upregulated, downregulated, other)
  )
  plot_data$total      <- total_proteins
  plot_data$percentage <- plot_data$count / plot_data$total
  plot_data$cumulative <- cumsum(plot_data$percentage) - plot_data$percentage / 2

  ggplot(plot_data, aes(x = 2, y = percentage, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = pi / 360) +
    xlim(0.5, 2.5) +
    geom_text(aes(label = total_proteins), x = 0.5, y = 0, size = 6) +
    geom_label_repel(aes(label = count, fill = category),
                     position = position_stack(vjust = 0.5),
                     size = 5, alpha = 0.5, color = "white") +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("Upregulated" = "#DC143C",
                                 "Downregulated" = "#4169E1",
                                 "Other" = "#6C7B8B")) +
    labs(title = plot_title)
}
