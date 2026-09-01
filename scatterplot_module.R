# ─────────────────────────────────────────────────────────
# OmicsVisor - logFC Scatter Plot Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

scatterplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
      h3("Scatterplot Module"),
      p("This module creates a scatterplot comparing two selected logFC columns. Points can be highlighted based on significance in either or both experiments, and user-specified IDs can be labeled."),
      
      # Select X and Y logFC columns
      fluidRow(
        column(6, selectInput(ns("x_logfc"), "X-axis logFC column:", choices = NULL)),
        column(6, selectInput(ns("y_logfc"), "Y-axis logFC column:", choices = NULL))
      ),
      
      # Highlighted ID input
      textInput(ns("highlight_ids"), "Highlight IDs (comma-separated):", value = ""),
      
      checkboxInput(ns("label_all_ids"), "Label highlighted IDs even if not significant", value = FALSE),
      
      # Cutoffs
      fluidRow(
        column(6, numericInput(ns("pval_cutoff"), "Adj. P-value cutoff:", value = 0.05, min = 0, max = 1, step = 0.01)),
        column(6, numericInput(ns("logfc_cutoff"), "logFC cutoff:", value = 1, min = 0))
      ),
      
      # Plot customization
      fluidRow(
        column(6, numericInput(ns("point_size"), "Point Size:", value = 2, min = 0.5)),
        column(6, numericInput(ns("label_size"), "Label Size:", value = 3, min = 1))
      ),
      
      fluidRow(
        column(6, numericInput(ns("plot_width"), "Plot Width (inches):", value = 8)),
        column(6, numericInput(ns("plot_height"), "Plot Height (inches):", value = 6))
      ),
      
      downloadButton(ns("download_plot"), "Download Scatterplot as PDF", class = "btn-sm"),
      
      fluidRow(
        column(12, plotOutput(ns("scatter_plot"), height = "600px", width = "100%"))
      ),
      
      checkboxInput(ns("lock_aspect"), "Fix aspect ratio (1:1)", value = FALSE)
      
      # plotOutput(ns("scatter_plot"), height = "600px", width = "100%")
  )
}

scatterplot_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  # Populate logFC selection inputs
  observe({
    updateSelectInput(session, "x_logfc", choices = data()$logFC_cols)
    updateSelectInput(session, "y_logfc", choices = data()$logFC_cols)
  })
  
  # Reactive for processed data
  scatter_data <- reactive({
    req(input$x_logfc, input$y_logfc)
    
    df <- data()$data
    adj_x <- gsub("logFC", "adj.P.Val", input$x_logfc)
    adj_y <- gsub("logFC", "adj.P.Val", input$y_logfc)
    
    df$Significance <- "None"
    df$Significance[df[[adj_x]] < input$pval_cutoff & abs(df[[input$x_logfc]]) > input$logfc_cutoff] <- "Exp1"
    df$Significance[df[[adj_y]] < input$pval_cutoff & abs(df[[input$y_logfc]]) > input$logfc_cutoff] <- "Exp2"
    df$Significance[df[[adj_x]] < input$pval_cutoff & abs(df[[input$x_logfc]]) > input$logfc_cutoff &
                      df[[adj_y]] < input$pval_cutoff & abs(df[[input$y_logfc]]) > input$logfc_cutoff] <- "Both"
    
    highlight_ids <- trimws(unlist(strsplit(input$highlight_ids, ",")))
    df$Label <- ifelse(df$id %in% highlight_ids, df$id, "")
    
    df
  })
  
  
  plot_scatter <- function(df, input) {
    x <- input$x_logfc
    y <- input$y_logfc
    
    df_bg <- df[df$Significance == "None", ]
    df_fg <- df[df$Significance != "None", ]
    
    label_df <- if (input$label_all_ids) {
      df[df$Label != "", ]
    } else {
      df[df$Label != "" & df$Significance != "None", ]
    }
    
    # Correlation coefficients. cor(use = "complete.obs") errors outright when
    # no complete pair exists (an all-NA comparison column), which would take
    # the whole plot down rather than just leaving the subtitle blank.
    safe_cor <- function(method) {
      tryCatch(
        cor(df[[input$x_logfc]], df[[input$y_logfc]],
            method = method, use = "complete.obs"),
        error = function(e) NA_real_, warning = function(w) NA_real_
      )
    }
    pearson  <- safe_cor("pearson")
    spearman <- safe_cor("spearman")
    
    subtitle_text <- sprintf("Pearson: %.2f  |  Spearman: %.2f  |  adj.P ≤ %.2g  |  |logFC| ≥ %.2g",
                             pearson, spearman, input$pval_cutoff, input$logfc_cutoff)
    
    
    p <- ggplot() +
      # Background: nonsignificant points
      geom_point(data = df_bg, aes(x = .data[[x]], y = .data[[y]]),
                 color = "darkgrey", size = input$point_size, alpha = 0.5) +

      # Foreground: significant points with colour mapping
      geom_point(data = df_fg,
                 aes(x = .data[[x]], y = .data[[y]], color = .data$Significance),
                 size = input$point_size) +

      # Labels for selected points using same colours
      ggrepel::geom_text_repel(
        data = label_df,
        aes(x = .data[[x]], y = .data[[y]],
            label = .data$Label, color = .data$Significance),
        size = input$label_size,
        segment.color = "black",
        max.overlaps = Inf,
        # fontface = "bold",
        show.legend = FALSE  # Hides legend for text labels
      ) +
      
      geom_hline(yintercept = 0, linetype = "dashed", color = "steelblue", alpha = 0.3) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "steelblue", alpha = 0.3) +
      
      scale_color_manual(values = c(
        "Exp1" = "#0072B2",  # a deeper, cooler blue
        "Exp2" = "#E69F00",  # a softer golden-orange
        "Both" = "#D65DB1"   # a vibrant medium pink
      )) +
      
      labs(
        title = "logFC Scatterplot",
        subtitle = subtitle_text,
        x = input$x_logfc,
        y = input$y_logfc
      ) +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "right")
    
    if (input$lock_aspect) {
      lims <- range(c(df[[input$x_logfc]], df[[input$y_logfc]]), na.rm = TRUE)
      lims <- c(-max(abs(lims)), max(abs(lims)))  # symmetric limits
      
      p <- p + 
        coord_fixed() +
        xlim(lims) +
        ylim(lims)
    }
    
    p
  }
  
  
  output$scatter_plot <- renderPlot({
    df <- scatter_data()
    plot_scatter(df, input)
  }, width = 800, height = 600)
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("scatterplot_",
             gsub("logFC_", "", input$x_logfc), "_vs_",
             gsub("logFC_", "", input$y_logfc),
             "_adjP", input$pval_cutoff,
             "_logFC", input$logfc_cutoff,
             "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      ggsave(file, plot = plot_scatter(scatter_data(), input),
             device = ov_pdf_device(),
             width = input$plot_width, height = input$plot_height)
    }
  )
  })
}
