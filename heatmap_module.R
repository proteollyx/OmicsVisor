# heatmap_module.R


heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("Heatmap Module"),
      p("The Heatmap module allows users to visualise a selected list of IDs and plot intensity values across chosen columns. 
         You can paste a comma-separated list of IDs into ‘Manually select IDs,’ then choose intensity columns from the 
         'Select Intensity Columns' tool. If your 'Intensity Column Regex' was set properly, relevant columns will appear; 
         leaving it blank shows all columns. 
         You can also quickly select or deselect all columns using the new buttons below the dropdown menu.
         Use the 'Select Row Name Column' field to choose which column from the dataset should become the row labels in the heatmap. 
         
         For grouping, you can select one or more components from each column's split name—by checking the boxes in the 
         'Select Components for Grouping Annotation' section. The order in which you click these boxes matters for how the 
         combined group label is formed. If clustering is turned off, the columns will be sorted alphabetically by these group labels, 
         effectively grouping columns that share the same annotated label. 
         
         Clustering options allow you to cluster rows and/or columns using hierarchical clustering. Row-wise z-score scaling 
         can be applied to normalize intensities. The 'Download Data' button exports the data matrix as a tab-delimited `.txt` file, 
         and you can also download the resulting heatmap as a PDF for further analysis."),
      
      textInput(ns("id_selection"), "Manually select IDs (comma-separated):", ""),
      
      tags$style(HTML(".selectize-input { width: 400px !important; }")),
      selectInput(ns("intensity_columns"), "Select Intensity Columns:", choices = NULL, multiple = TRUE),
      fluidRow(
        column(2, actionButton(ns("select_all_intensity"), "Select All")),
        column(2, actionButton(ns("deselect_all_intensity"), "Deselect All"))
      ),
      
      selectInput(ns("rowname_column"), "Select Row Name Column:", choices = NULL),
      
      # NEW: multi-select for combining multiple columns as row labels
      selectInput(ns("row_label_columns"), 
                  "Select Label Columns for Row Names:", 
                  choices = NULL, 
                  multiple = TRUE),
      
      h4("Select Components for Grouping Annotation (order of clicking matters)"),
      uiOutput(ns("grouping_checkboxes_ui")),
      verbatimTextOutput(ns("group_annotation_preview")),
      
      checkboxInput(ns("cluster_columns"), "Cluster Columns", value = TRUE),
      checkboxInput(ns("cluster_rows"), "Cluster Rows", value = TRUE),
      checkboxInput(ns("scale_rows"), "Scale by Row (Z-score)", value = FALSE),
      
      downloadButton(ns("download_data"), "Download Data"),
      
      numericInput(ns("pdf_width"), "PDF Width", value = 8, min = 4),
      numericInput(ns("pdf_height"), "PDF Height", value = 6, min = 4),
      numericInput(ns("fontsize_row"), "Font Size Row", value = 8, min = 6),
      numericInput(ns("fontsize_col"), "Font Size Column", value = 8, min = 6),
      downloadButton(ns("download_pdf"), "Download Heatmap as PDF"),
      
      plotOutput(ns("heatmap_plot"), height = "700px", width = "100%")
    )
  )
}

heatmap_server <- function(input, output, session, data) {
  ns <- session$ns
  
  # Track order in which grouping components are selected (for columns)
  component_order <- reactiveVal(character(0))
  
  # Populate intensity and rowname selectInputs
  observe({
    updateSelectInput(session, "intensity_columns",
                      choices = data()$intensity_cols)
    updateSelectInput(session, "rowname_column",
                      choices = names(data()$data))
    # NEW:
    all_cols <- names(data()$data)  # or a subset
    updateSelectInput(session, "row_label_columns", choices = all_cols)
  })
  
  # Select All / Deselect All
  observeEvent(input$select_all_intensity, {
    updateSelectInput(session, "intensity_columns",
                      selected = data()$intensity_cols)
  })
  observeEvent(input$deselect_all_intensity, {
    updateSelectInput(session, "intensity_columns", selected = character(0))
  })
  
  # Split column names
  split_components <- reactive({
    req(input$intensity_columns)
    col_names <- input$intensity_columns
    str_split_fixed(col_names, "_|\\.", n = 5)
  })
  
  # Generate checkboxes for grouping annotation
  output$grouping_checkboxes_ui <- renderUI({
    comps <- split_components()
    n_comp <- ncol(comps)
    tagList(
      lapply(seq_len(n_comp), function(i) {
        checkboxInput(ns(paste0("group_component_", i)),
                      label = paste("Use Component", i),
                      value = FALSE)
      })
    )
  })
  
  # Observe toggles to track component order
  observe({
    comps <- split_components()
    n_comp <- ncol(comps)
    
    lapply(seq_len(n_comp), function(i) {
      observeEvent(input[[paste0("group_component_", i)]], {
        clicked <- input[[paste0("group_component_", i)]]
        
        current <- component_order()
        i_char <- as.character(i)
        if (clicked) {
          # If just checked, move i_char to end
          current <- setdiff(current, i_char)
          current <- c(current, i_char)
        } else {
          # If unchecked, remove it
          current <- setdiff(current, i_char)
        }
        component_order(current)
      })
    })
  })
  
  # Build group annotations for columns based on selection order
  group_annotations <- reactive({
    co <- component_order()
    if (length(co) == 0) return(NULL)
    
    comps <- split_components()
    co_num <- as.numeric(co)
    selected_mat <- comps[, co_num, drop = FALSE]
    apply(selected_mat, 1, function(row) paste(row, collapse = "_"))
  })
  
  output$group_annotation_preview <- renderText({
    annot <- group_annotations()
    if (!is.null(annot)) {
      paste("Group Names:", paste(annot, collapse = ", "))
    } else {
      "No group names selected."
    }
  })
  
  # Reactive that returns: the final matrix, plus row/col dendrograms if used
  final_heatmap_data <- reactive({
    req(input$intensity_columns, input$rowname_column)
    df <- data()$data
    
    # Filter rows by ID
    if (nzchar(input$id_selection)) {
      sel_ids <- strsplit(input$id_selection, ",")[[1]]
      sel_ids <- trimws(sel_ids)
      df <- df[df$id %in% sel_ids, , drop = FALSE]
    }
    
    # NEW: Set row names
    # Create a new combined label by pasting across the selected columns
    # NEW: Set row names
    if (length(input$row_label_columns) > 0) {
      # Multi-column approach: combine the selected columns
      df$.combinedLabel <- apply(df[, input$row_label_columns, drop = FALSE], 1, paste, collapse = "_")
      
      # Check for duplicates
      if (anyDuplicated(df$.combinedLabel) > 0) {
        showNotification("Some row labels are duplicated!", type = "error")
      }
      rownames(df) <- df$.combinedLabel
      
    } else {
      # Fallback: use the selected rowname column
      rownames(df) <- df[[input$rowname_column]]
    }
    ## end NEW
    
    
    # Subset columns
    mat <- as.matrix(df[, input$intensity_columns, drop = FALSE])
    
    # Scale rows if needed
    if (input$scale_rows) {
      mat <- t(scale(t(mat), center = TRUE, scale = TRUE))
    }
    
    # We'll store row_dend & col_dend
    col_dend <- NULL
    row_dend <- NULL
    
    # Column clustering or grouping
    if (input$cluster_columns) {
      # Create column dendrogram but don't reorder now
      col_dend <- hclust(dist(t(mat)))
    } else {
      # No clustering => alphabetical grouping by group_annotations
      col_groups <- group_annotations()
      if (!is.null(col_groups)) {
        new_col_order <- order(col_groups)
        mat <- mat[, new_col_order, drop = FALSE]
      }
    }
    
    # Row clustering or no row clustering
    if (input$cluster_rows) {
      # Create row dendrogram
      row_dend <- hclust(dist(mat))
      # (Do not reorder mat here; let pheatmap do it if we pass row_dend.)
    } else {
      # If you do NOT want row grouping logic, just do nothing
      # mat remains as is
      #
      # Alternatively, if you want row grouping, you'd define a separate function
      # that maps rownames(mat) -> group labels, then reorder. But presumably your
      # group_annotations() are for columns, so let's skip that.
    }
    
    list(
      matrix = mat,
      col_dend = col_dend,
      row_dend = row_dend
    )
  })
  
  # Render the heatmap
  output$heatmap_plot <- renderPlot({
    hm_data <- final_heatmap_data()
    data_matrix <- hm_data$matrix
    col_dend <- hm_data$col_dend
    row_dend <- hm_data$row_dend
    
    # Build a column annotation data.frame if group_annotations exist
    # We'll match them to the current colnames
    col_annot <- NULL
    ga <- group_annotations()  # for columns
    if (!is.null(ga)) {
      # ga is the group label for each column *in the original order*
      original_cols <- input$intensity_columns
      names(ga) <- original_cols
      
      # Now match to the current colnames of data_matrix
      # (If cluster_columns=TRUE, data_matrix is still in the original col order,
      #  pheatmap will reorder it. If cluster_columns=FALSE, we've already changed data_matrix,
      #  so colnames differ from the original. This matching step ensures alignment.)
      matched_groups <- ga[colnames(data_matrix)]
      col_annot <- data.frame(Group = matched_groups, check.names = FALSE)
      rownames(col_annot) <- colnames(data_matrix)
    }
    
    # Prepare color scale
    colour_palette <- colorRampPalette(c("darkblue", "white", "firebrick"))(100)
    if (input$scale_rows) {
      max_abs_value <- max(abs(data_matrix), na.rm = TRUE)
      breaks <- seq(-max_abs_value, max_abs_value, length.out = 101)
    } else {
      breaks <- NULL
    }
    
    # pass row_dend and col_dend to pheatmap
    # pheatmap reorders rows/cols + draws dendrograms
    
    pheatmap::pheatmap(
      data_matrix,
      cluster_rows = if (!is.null(row_dend)) row_dend else FALSE,
      cluster_cols = if (!is.null(col_dend)) col_dend else FALSE,
      scale = "none",
      color = colour_palette,
      breaks = breaks,
      fontsize_row = input$fontsize_row,
      fontsize_col = input$fontsize_col,
      annotation_col = col_annot
    )
  })
  
  # Download data => reorder row/col by dendrogram if used
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("heatmap_data", if (input$scale_rows) "_scaled" else "", ".txt")
    },
    content = function(file) {
      hm_data <- final_heatmap_data()
      mat <- hm_data$matrix
      col_dend <- hm_data$col_dend
      row_dend <- hm_data$row_dend
      
      # If col_dend is not NULL, reorder columns by col_dend$order
      if (!is.null(col_dend)) {
        mat <- mat[, col_dend$order, drop = FALSE]
      }
      # If row_dend is not NULL, reorder rows by row_dend$order
      if (!is.null(row_dend)) {
        mat <- mat[row_dend$order, , drop = FALSE]
      }
      
      write.table(as.data.frame(mat), file, sep = "\t", quote = FALSE,
                  row.names = TRUE, col.names = NA)
    }
  )
  
  # Download PDF => same logic as output$heatmap_plot
  # but we reorder the data ourselves or let pheatmap do it
  output$download_pdf <- downloadHandler(
    filename = function() {
      "heatmap_plot.pdf"
    },
    content = function(file) {
      pdf(file, width = input$pdf_width, height = input$pdf_height)
      
      hm_data <- final_heatmap_data()
      mat <- hm_data$matrix
      col_dend <- hm_data$col_dend
      row_dend <- hm_data$row_dend
      
      # Build annotation in the same way
      col_annot <- NULL
      ga <- group_annotations()
      if (!is.null(ga)) {
        names(ga) <- input$intensity_columns
        matched_groups <- ga[colnames(mat)]
        col_annot <- data.frame(Group = matched_groups, check.names = FALSE)
        rownames(col_annot) <- colnames(mat)
      }
      
      # color palette
      colour_palette <- colorRampPalette(c("darkblue", "white", "firebrick"))(100)
      if (input$scale_rows) {
        max_abs_value <- max(abs(mat), na.rm = TRUE)
        breaks <- seq(-max_abs_value, max_abs_value, length.out = 101)
      } else {
        breaks <- NULL
      }
      
      pheatmap::pheatmap(
        mat,
        cluster_rows = if (!is.null(row_dend)) row_dend else FALSE,
        cluster_cols = if (!is.null(col_dend)) col_dend else FALSE,
        color = colour_palette,
        scale = "none",
        breaks = breaks,
        fontsize_row = input$fontsize_row,
        fontsize_col = input$fontsize_col,
        annotation_col = col_annot
      )
      
      dev.off()
    }
  )
}