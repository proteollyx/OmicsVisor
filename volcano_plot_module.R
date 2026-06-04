# ─────────────────────────────────────────────────────────
# OmicsVisor - Volcano Plot Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

volcano_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
      # h3("Volcano Plot Module"),
      # p("The Volcano Plot module visualises two-sample comparisons in the data. When you select a `logFC` column, the corresponding `Adj.P-value` column is selected automatically. Always verify that the selected `logFC` and `Adj.P-value` columns are correctly paired."),
      # p("Use the numeric input fields for `Adj. P-value cutoff` and `logFC cutoff` to set thresholds. Data is in log2 scale, so an absolute log2 fold change of 1 equals a linear fold change of 2."),
      # p("The ‘Select Label Columns’ field allows you to define labels in the volcano plot. Multiple fields can be combined."),
      # p("Click 'Generate IDs for Heatmap Selection' to create lists of significant IDs on either side of the volcano plot based on your cutoffs. These lists can be used as comma-separated inputs for the Heatmap, Volcano Printer, and PCA modules. The copy button lets you easily copy them to your clipboard."),
      # p("For the best viewing experience, adjust your browser window to ensure the plots are displayed at optimal proportions."),
      
      h3("Volcano Plot Module"),
      p("The Volcano Plot module visualises pairwise comparisons in the data. Rather than selecting ‘logFC’ and ‘Adj.P-value’ columns separately, 
   you can choose from a list of available comparisons. The tool will then automatically detect the associated logFC and adjusted p-value columns, 
   ensuring they match correctly. Make sure your data has unique column names for each comparison to avoid mismatches or duplicates."),
      p("Use the ‘Adj. P-value cutoff’ and ‘logFC cutoff’ fields to define significance thresholds. 
   Data is typically in log2 scale, meaning an absolute log2 fold change of 1 is equivalent to a 2-fold change. 
   You can combine multiple fields under ‘Select Label Columns’ to generate informative point labels in the volcano plot."),
      p("After setting up these parameters, click ‘Generate IDs for Heatmap Selection’ to create lists of significant IDs on both sides of the volcano. 
   These lists can be easily copied for use in the Heatmap, Volcano Printer, or PCA modules. 
   Adjust your browser window size or the numeric inputs for optimal display."),
      p("Note: If your dataset includes multiple or duplicate column sets for the same comparison, 
   please rename or remove redundant columns so each comparison is unique. This helps ensure the automatic column selection is correct."),
      
      
      # Single dropdown for the comparison
      selectInput(ns("comparison_name"), "Select Comparison:", choices = NULL),
      
      numericInput(ns("pval_cutoff"), "Adj. P-value cutoff:", 0.05, min = 0, max = 1, step = 0.01),
      numericInput(ns("logfc_cutoff"), "logFC cutoff:", 1, min = 0),
      selectInput(ns("label_columns"), "Select Label Columns:", choices = NULL, multiple = TRUE),
      actionButton(ns("generate_ids"), "Generate IDs for Heatmap Selection", class = "btn-sm"),

      h4("Left Side (logFC < 0)"),
      actionButton(ns("copy_neg_btn"), "Copy", icon = icon("copy"), class = "btn-sm"),
      verbatimTextOutput(ns("negative_ids")),

      h4("Right Side (logFC > 0)"),
      actionButton(ns("copy_pos_btn"), "Copy", icon = icon("copy"), class = "btn-sm"),
      verbatimTextOutput(ns("positive_ids")),

      h4("Both Sides (All Significant IDs)"),
      actionButton(ns("copy_both_btn"), "Copy", icon = icon("copy"), class = "btn-sm"),
      verbatimTextOutput(ns("both_ids")),
      
      # plotlyOutput(ns("volcano_plot")),
      plotlyOutput(ns("volcano_plot"), height = "600px", width = "100%")
      # fluidRow(
      #   column(12, plotlyOutput(ns("volcano_plot"), height = "600px", width = "100%"))
      # )
  )
}

volcano_plot_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  ns <- session$ns

  # store IDs of clicked points
  clicked_ids <- reactiveVal(character())

  # store generated ID lists (safe clipboard access via sendCustomMessage)
  id_lists <- reactiveValues(pos = character(0), neg = character(0), all = character(0))
  
  # 1) Observe the data to figure out which comparisons are available
  observe({
    req(data()$data)
    
    all_cols <- names(data()$data)
    comp_choices <- detect_comparisons(all_cols)
    
    if (length(comp_choices) == 0) {
      showNotification("No valid comparisons found (no matching logFC_ / adj.P.Val_ pairs).", type = "warning")
    }
    
    # Check for duplicates
    dups <- comp_choices[duplicated(comp_choices)]
    if (length(dups) > 0) {
      showNotification(
        paste("Warning: Duplicate comparisons detected:", paste(unique(dups), collapse = ", ")),
        type = "warning"
      )
    }
    
    updateSelectInput(session, "comparison_name", choices = comp_choices)
  })
  
  # 2) Populate the label columns with all columns from the data
  observe({
    req(data()$data)
    updateSelectInput(session, "label_columns", choices = names(data()$data))
  })
  
  # 3) Reactive that returns the actual logFC and adj.P.Val column names
  chosen_cols <- reactive({
    req(input$comparison_name)
    logFC_col <- paste0("logFC_", input$comparison_name)
    adjP_col  <- paste0("adj.P.Val_", input$comparison_name)
    
    missing_cols <- setdiff(c(logFC_col, adjP_col), names(data()$data))
    if (length(missing_cols) > 0) {
      showNotification(
        paste("Missing expected columns:", paste(missing_cols, collapse = ", ")),
        type = "error"
      )
    }
    
    list(logFC = logFC_col, adjP = adjP_col)
  })
  
  observeEvent(event_data("plotly_click", source = "volcano_src"), {
    click <- event_data("plotly_click", source = "volcano_src")
    if (is.null(click)) return(NULL)
    
    this_id <- click$key
    if (is.null(this_id) || is.na(this_id)) return(NULL)
    
    current <- clicked_ids()
    
    # toggle behaviour: click to add/remove
    if (this_id %in% current) {
      clicked_ids(setdiff(current, this_id))
    } else {
      clicked_ids(c(current, this_id))
    }
  })
  
  # 4) Build the volcano plot
  output$volcano_plot <- renderPlotly({
    # Retrieve the columns from chosen_cols
    cols <- chosen_cols()
    req(cols$logFC, cols$adjP)  # ensure they exist
    
    df <- data()$data
    
    # Mark significance
    df$significant <- abs(df[[cols$logFC]]) > input$logfc_cutoff & 
      df[[cols$adjP]] < input$pval_cutoff
    
    # Generate labels if label_columns are selected
    if (!is.null(input$label_columns) && length(input$label_columns) > 0) {
      df$labels <- apply(df[, input$label_columns, drop = FALSE], 1, paste, collapse = "_")
    } else {
      # NEW: fall back to id so clicked labels are visible
      df$labels <- df$id
    }
    
    # base scatter
    p <- plot_ly(
      df,
      x = ~df[[cols$logFC]],
      y = ~-log10(df[[cols$adjP]]),
      text = ~labels,
      key  = ~id,                     # NEW: carry the ID, needed in plotly_click
      source = "volcano_src",         # NEW: match event_data() source
      color = ~significant, 
      colors = c("grey", "red"),
      type = "scatter", 
      mode = "markers",
      hoverinfo = "text"
    )
    
    # NEW: add permanent text labels for clicked points
    lab_ids <- clicked_ids()
    if (length(lab_ids) > 0) {
      df_labs <- df[df$id %in% lab_ids, , drop = FALSE]
      
      p <- add_text(
        p,
        data = df_labs,
        x = ~df_labs[[cols$logFC]],
        y = ~-log10(df_labs[[cols$adjP]]),
        text = ~labels,
        textposition = "top center",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    
    p <- layout(
      p,
      title = paste("Volcano Plot:", input$comparison_name),
      xaxis = list(title = "logFC"),
      yaxis = list(title = "-log10(adj.P-value)")
    )
    
    # NEW: explicitly register click events
    p <- event_register(p, 'plotly_click')
    
    p
  })

  # 5) Generate ID lists when 'Generate IDs' is clicked
  observeEvent(input$generate_ids, {
    cols <- chosen_cols()
    req(cols$logFC, cols$adjP)
    
    df <- data()$data
    
    # Positive side
    # pos_ids <- df[
    #   df[[cols$logFC]] > 0 & 
    #     df[[cols$adjP]] < input$pval_cutoff & 
    #     abs(df[[cols$logFC]]) > input$logfc_cutoff, 
    #   "id"
    # ]
    
    # Negative side
    # neg_ids <- df[
    #   df[[cols$logFC]] < 0 & 
    #     df[[cols$adjP]] < input$pval_cutoff & 
    #     abs(df[[cols$logFC]]) > input$logfc_cutoff, 
    #   "id"
    # ]
    
    valid_rows <- 
      !is.na(df[[cols$logFC]]) &
      !is.na(df[[cols$adjP]]) &
      df[[cols$adjP]] < input$pval_cutoff &
      abs(df[[cols$logFC]]) > input$logfc_cutoff
    
    id_lists$pos <- df[valid_rows & df[[cols$logFC]] > 0, "id"]
    id_lists$neg <- df[valid_rows & df[[cols$logFC]] < 0, "id"]
    id_lists$all <- c(id_lists$neg, id_lists$pos)

    output$positive_ids <- renderText({ paste(id_lists$pos, collapse = ", ") })
    output$negative_ids <- renderText({ paste(id_lists$neg, collapse = ", ") })
    output$both_ids     <- renderText({ paste(id_lists$all, collapse = ", ") })
  })

  observeEvent(input$copy_neg_btn,  {
    session$sendCustomMessage("copyToClipboard", paste(id_lists$neg, collapse = ", "))
  })
  observeEvent(input$copy_pos_btn,  {
    session$sendCustomMessage("copyToClipboard", paste(id_lists$pos, collapse = ", "))
  })
  observeEvent(input$copy_both_btn, {
    session$sendCustomMessage("copyToClipboard", paste(id_lists$all, collapse = ", "))
  })
  })
}