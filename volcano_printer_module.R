# volcano_printer_module.R

# UI function for Volcano Printer
volcano_printer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("Volcano Printer Module"),
      p("The Volcano Printer module extends the functionality of the Volcano Plot module, enabling PDF export of publication-quality volcano plots."),
      p("Like the Volcano Plot module, the Volcano Printer automatically pairs selected `logFC` columns with their `Adj.P-value` columns. Set `Adj.P-value cutoff` and `logFC cutoff` values as desired. Data is in log2 space, so an absolute log2 fold change of 1 represents a two-fold linear change."),
      p("Choose one or more columns for labeling points in the volcano plot. Select ‘Label Only Significant Selected IDs’ to label only those IDs meeting the cutoff criteria."),
      p("Click ‘Download’ to save the plot as a vector graphic PDF."),
      p("Please note that selected IDs might not be displayed in the interactive user interface but will appear in the downloaded PDF."),
      
      # selectInput(ns("logFC_column"), "Select logFC Column:", choices = NULL),
      # uiOutput(ns("adjP_column_ui")),  # UI for automatically selected adj.P column
      
      # Single drop-down for comparison
      selectInput(ns("comparison_name"), "Select Comparison:", choices = NULL),
      
      numericInput(ns("pval_cutoff"), "P-value cutoff:", 0.05, min = 0, max = 1),
      numericInput(ns("logfc_cutoff"), "logFC cutoff:", 1, min = 0),
      
      # Label columns + optional labeling of significant
      selectInput(ns("label_columns"), "Select Label Columns:", choices = NULL, multiple = TRUE),
      checkboxInput(ns("label_only_sig"), "Label Only Significant Selected IDs", value = TRUE),
      
      textInput(ns("id_selection"), "Manually select IDs (comma-separated):", ""),
      
      numericInput(ns("plot_width"), "Plot Width (inches)", value = 8, min = 4),
      numericInput(ns("plot_height"), "Plot Height (inches)", value = 6, min = 4),
      downloadButton(ns("download_plot"), "Download Volcano Plot"),
      plotOutput(ns("volcano_ggplot"))
    )
  )
}

# Server function for Volcano Printer
volcano_printer_server <- function(input, output, session, data) {
  ns <- session$ns
  
  # 1) Observe the data to detect all possible comparisons
  observe({
    req(data()$data)
    all_cols <- names(data()$data)
    
    # detect_comparisons() is from your helper file
    comp_choices <- detect_comparisons(all_cols)
    
    if (length(comp_choices) == 0) {
      showNotification("No valid comparisons found (no matching logFC_ / adj.P.Val_ pairs).", type = "warning")
    }
    
    # Update the single selectInput with discovered comparisons
    updateSelectInput(session, "comparison_name", choices = comp_choices)
  })
  
  # 2) Also update the label columns with all columns from data
  observe({
    req(data()$data)
    updateSelectInput(session, "label_columns", choices = names(data()$data))
  })
  
  # 3) Reactive that returns the actual logFC and adj.P.Val columns for the chosen comparison
  chosen_cols <- reactive({
    req(input$comparison_name)
    logFC_col <- paste0("logFC_", input$comparison_name)
    adjP_col  <- paste0("adj.P.Val_", input$comparison_name)
    
    # optional check to ensure they exist
    missing_cols <- setdiff(c(logFC_col, adjP_col), names(data()$data))
    if (length(missing_cols) > 0) {
      showNotification(
        paste("Missing columns for comparison:", paste(missing_cols, collapse = ", ")),
        type = "error"
      )
    }
    
    list(
      logFC = logFC_col,
      adjP  = adjP_col
    )
  })
  
  # 4) Prepare data for ggplot
  plot_data <- reactive({
    # build the derived columns
    cols <- chosen_cols()
    req(cols$logFC, cols$adjP)  # ensure they're valid
    
    df <- data()$data
    
    # Significance cutoff
    df$significant <- abs(df[[cols$logFC]]) > input$logfc_cutoff & 
      df[[cols$adjP]] < input$pval_cutoff
    
    # If user typed IDs
    selected_ids <- strsplit(input$id_selection, ",")[[1]]
    selected_ids <- trimws(selected_ids)
    
    # Label columns
    if (!is.null(input$label_columns) && length(input$label_columns) > 0) {
      df$labels <- apply(df[, input$label_columns, drop = FALSE], 1, paste, collapse = "_")
    } else {
      df$labels <- ""
    }
    
    # If "label_only_sig" is TRUE, we label only those that are both selected and significant
    df$label_display <- ifelse(
      df$id %in% selected_ids & (!input$label_only_sig | df$significant),
      df$labels,
      ""
    )
    
    df
  })
  
  # 5) Render ggplot for the UI
  output$volcano_ggplot <- renderPlot({
    df <- plot_data()
    cols <- chosen_cols()
    req(cols$logFC, cols$adjP)
    
    ggplot(df, aes(x = .data[[cols$logFC]], y = -log10(.data[[cols$adjP]]))) +
      geom_point(aes(color = significant), size = 2) +
      scale_color_manual(values = c("grey", "#F85414"), labels = c("Non-Significant", "Significant")) +
      labs(
        x = paste0(cols$logFC, " (log2 fold change)"),
        y = "-log10(adj.P-value)"
      ) +
      theme_minimal() +
      ggrepel::geom_text_repel(
        aes(label = label_display),
        color = "black",
        max.overlaps = Inf,
        size = 2.3
      ) +
      # Add cutoffs
      geom_hline(yintercept = -log10(input$pval_cutoff), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = c(-input$logfc_cutoff, input$logfc_cutoff), linetype = "dashed", color = "blue")
  })
  
  # 6) Download Handler for PDF
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("volcano_plot_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      df <- plot_data()
      cols <- chosen_cols()
      req(cols$logFC, cols$adjP)
      
      plot_to_save <- ggplot(df, aes(x = .data[[cols$logFC]], y = -log10(.data[[cols$adjP]]))) +
        geom_point(aes(color = significant), size = 2) +
        scale_color_manual(values = c("grey", "#F85414"), labels = c("Non-Significant", "Significant")) +
        labs(
          x = paste0(cols$logFC, " (log2 fold change)"),
          y = "-log10(adj.P-value)"
        ) +
        theme_minimal() +
        ggrepel::geom_text_repel(
          aes(label = label_display),
          color = "black",
          max.overlaps = Inf,
          size = 2.3
        ) +
        geom_hline(yintercept = -log10(input$pval_cutoff), linetype = "dashed", color = "blue") +
        geom_vline(xintercept = c(-input$logfc_cutoff, input$logfc_cutoff), linetype = "dashed", color = "blue")
      
      # Finally, save
      ggsave(
        filename = file, plot = plot_to_save, device = "pdf",
        width = input$plot_width, height = input$plot_height
      )
    }
  )
}