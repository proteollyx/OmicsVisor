# donut_plot_module.R

# UI function for Donut Plot Module
donut_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("Donut Plot Module"),
      p("Generates a donut plot for each two-group comparison using user-defined `logFC` and `Adj.P-value` cutoffs. Checkboxes allow you to select up- or downregulated candidates for each comparison and copy them to your clipboard. Within each comparison, the logic is OR (includes all selected IDs); across comparisons, the logic is AND (IDs must meet criteria in all selected comparisons), allowing you to identify overlapping candidates."),
      # Description of checkbox selection logic
      p("Select upregulated and/or downregulated IDs using checkboxes for each comparison. Selecting one checkbox within a comparison will include all IDs meeting that criterion, while selecting both will include all IDs meeting either criterion. Across comparisons, only IDs meeting all selected criteria (AND logic) will be included."),
      
      fluidRow(
        column(4, numericInput(ns("logfc_cutoff"), "Global logFC Cutoff", value = 1, min = 0)),
        column(4, numericInput(ns("pval_cutoff"), "Global Adj.P-value Cutoff", value = 0.05, min = 0, max = 1)),
        column(4, actionButton(ns("apply_cutoff"), "Apply Cutoff"))
      ),
      uiOutput(ns("donut_plots_ui")),
      # fluidRow(
      #   column(10, verbatimTextOutput(ns("selected_ids_output"))),
      #   column(2, actionButton(ns("copy_selected_ids"), "Copy IDs", icon = icon("copy")))
      # )
      fluidRow(
        column(8, verbatimTextOutput(ns("selected_ids_output"))),
        column(4, verbatimTextOutput(ns("selected_ids_count")))
      ),
      fluidRow(
        column(12, actionButton(ns("copy_selected_ids"), "Copy IDs", icon = icon("copy")))
      )
    )
  )
}

# Server function for Donut Plot Module
donut_plot_server <- function(input, output, session, data) {
  ns <- session$ns
  
  # Observe cutoff settings and re-generate donut plots
  observeEvent(input$apply_cutoff, {
    output$donut_plots_ui <- renderUI({
      # Generate donut plots for each available logFC column
      tagList(
        lapply(seq_along(data()$logFC_cols), function(i) {
          logfc_col <- data()$logFC_cols[i]
          adjp_col <- gsub("logFC", "adj.P.Val", logfc_col)  # Infer the corresponding adj.P-value column
          
          all_ids <- data()$data$id
          down_ids <- all_ids[data()$data[[logfc_col]] < -input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
          up_ids <- all_ids[data()$data[[logfc_col]] > input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
          
          plot_title <- gsub("\\.", " ", sub("logFC_", "", logfc_col))
          
          # Generate a donut plot with checkboxes for up/downregulated selection
          tagList(
            plotOutput(ns(paste0("donut_plot_", i)), height = "300px"),
            checkboxInput(ns(paste0("select_up_", i)), label = paste("Select Upregulated for", plot_title)),
            checkboxInput(ns(paste0("select_down_", i)), label = paste("Select Downregulated for", plot_title)),
            hr()
          )
        })
      )
    })
    
    # Render each donut plot based on the computed down/up IDs
    lapply(seq_along(data()$logFC_cols), function(i) {
      logfc_col <- data()$logFC_cols[i]
      adjp_col <- gsub("logFC", "adj.P.Val", logfc_col)
      
      all_ids <- data()$data$id
      down_ids <- all_ids[data()$data[[logfc_col]] < -input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
      up_ids <- all_ids[data()$data[[logfc_col]] > input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
      
      plot_title <- gsub("\\.", " ", sub("logFC_", "", logfc_col))
      
      output[[paste0("donut_plot_", i)]] <- renderPlot({
        donut_plot(all_ids, down_ids, up_ids, plot_title)
      })
    })
  })
  
  # Reactive expression to generate selected IDs based on checkbox selections
  # selected_ids <- reactive({
  #   selected_ids <- data()$data$id  # Start with all IDs
  #   
  #   # Apply OR condition within each selected logFC-adj.P-value pair, and AND condition across selected pairs
  #   for (i in seq_along(data()$logFC_cols)) {
  #     logfc_col <- data()$logFC_cols[i]
  #     adjp_col <- gsub("logFC", "adj.P.Val", logfc_col)
  #     
  #     # Initialize up and down IDs to empty vectors
  #     up_ids <- down_ids <- character(0)
  #     
  #     # Collect IDs based on up and/or down selection within the current pair
  #     if (!is.null(input[[paste0("select_up_", i)]]) && input[[paste0("select_up_", i)]]) {
  #       up_ids <- data()$data$id[data()$data[[logfc_col]] > input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
  #     }
  #     if (!is.null(input[[paste0("select_down_", i)]]) && input[[paste0("select_down_", i)]]) {
  #       down_ids <- data()$data$id[data()$data[[logfc_col]] < -input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
  #     }
  #     
  #     # If either up or down IDs are selected, combine them and apply AND logic across pairs
  #     if (length(up_ids) > 0 || length(down_ids) > 0) {
  #       pair_selected_ids <- unique(c(up_ids, down_ids))  # OR logic within the current pair
  #       selected_ids <- intersect(selected_ids, pair_selected_ids)  # AND logic across pairs
  #     }
  #   }
  #   selected_ids
  # })
  
  selected_ids <- reactive({
    # Check if any checkbox is selected across comparisons
    any_selected <- any(sapply(seq_along(data()$logFC_cols), function(i) {
      isTRUE(input[[paste0("select_up_", i)]]) || isTRUE(input[[paste0("select_down_", i)]])
    }))
    
    # If no checkboxes are selected, return an empty vector
    if (!any_selected) {
      return(character(0))
    }
    
    # Initialize result as NULL
    result <- NULL
    
    # Loop through each comparison where a checkbox is selected
    for (i in seq_along(data()$logFC_cols)) {
      if (isTRUE(input[[paste0("select_up_", i)]]) || isTRUE(input[[paste0("select_down_", i)]]) ) {
        logfc_col <- data()$logFC_cols[i]
        adjp_col <- gsub("logFC", "adj.P.Val", logfc_col)
        
        # Determine selected IDs for this pair
        up_ids <- if (isTRUE(input[[paste0("select_up_", i)]])) {
          data()$data$id[data()$data[[logfc_col]] > input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
        } else character(0)
        
        down_ids <- if (isTRUE(input[[paste0("select_down_", i)]])) {
          data()$data$id[data()$data[[logfc_col]] < -input$logfc_cutoff & data()$data[[adjp_col]] < input$pval_cutoff]
        } else character(0)
        
        pair_selected_ids <- unique(c(up_ids, down_ids))
        
        # For the first valid comparison, initialize result
        if (is.null(result)) {
          result <- pair_selected_ids
        } else {
          result <- intersect(result, pair_selected_ids)
        }
      }
    }
    
    if (is.null(result)) character(0) else result
  })
  
  # Display selected IDs based on AND conditions across donut plots
  # output$selected_ids_output <- renderText({
  #   selected_ids <- selected_ids()
  #   if (length(selected_ids) > 0) {
  #     paste("Selected IDs:", paste(selected_ids, collapse = ", "))
  #   } else {
  #     "No IDs meet the selected criteria."
  #   }
  # })
  
  # Display selected IDs without the count
  output$selected_ids_output <- renderText({
    sel_ids <- selected_ids()
    if (length(sel_ids) > 0) {
      paste("Selected IDs:", paste(sel_ids, collapse = ", "))
    } else {
      "No IDs meet the selected criteria."
    }
  })
  
  # Display the count of selected IDs in the UI
  output$selected_ids_count <- renderText({
    paste("Total Count:", length(selected_ids()))
  })
  
  # Copy selected IDs (only the list is copied)
  observeEvent(input$copy_selected_ids, {
    selected_ids_text <- paste(selected_ids(), collapse = ", ")
    session$sendCustomMessage("copyToClipboard", selected_ids_text)
  })
  
  # Copy selected IDs to clipboard when button is clicked
  observeEvent(input$copy_selected_ids, {
    selected_ids_text <- paste(selected_ids(), collapse = ", ")
    session$sendCustomMessage("copyToClipboard", selected_ids_text)
  })
}

# Donut Plot Function (unchanged)
donut_plot <- function(all_ids, down_ids, up_ids, plot_title) {
  total_proteins <- length(all_ids)
  upregulated <- length(up_ids)
  downregulated <- length(down_ids)
  other <- total_proteins - upregulated - downregulated
  
  plot_data <- data.frame(
    category = c("Upregulated", "Downregulated", "Other"),
    count = c(upregulated, downregulated, other)
  )
  plot_data$total <- total_proteins
  plot_data$percentage <- plot_data$count / plot_data$total
  plot_data$cumulative <- cumsum(plot_data$percentage) - plot_data$percentage / 2
  
  ggplot(plot_data, aes(x = 2, y = percentage, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = pi/360) +  # Start from the top (12 o'clock)
    xlim(0.5, 2.5) +  # Create donut shape
    geom_text(aes(label = total_proteins), x = 0.5, y = 0, size = 6) +  # Total in center
    geom_label_repel(aes(label = count, fill = category), position = position_stack(vjust = 0.5), size = 5, alpha = 0.5, color = "white") +
    theme_void() + 
    theme(legend.position = "none") + 
    scale_fill_manual(values = c("Upregulated" = "#DC143C", "Downregulated" = "#4169E1", "Other" = "#6C7B8B")) +
    labs(title = plot_title)
}
