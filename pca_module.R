# ─────────────────────────────────────────────────────────
# OmicsVisor - PCA / UMAP Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
      h3("Dimension Reduction (PCA / UMAP)"),
      p("This module performs dimension reduction (PCA or UMAP) on selected intensity columns, similar to the Heatmap module."),
      p("Use ‘Intensity Column Regex’ in the sidebar to filter which columns appear under ‘Select Intensity Columns.’ 
         You can then quickly select or deselect all listed columns using the buttons below that dropdown. 
         Leaving the Regex field blank displays all columns."),
      p("Choose whether to use ‘All Rows’ or only ‘Selected Rows by ID.’ If selecting by ID, 
         enter a comma-separated list of IDs in the provided field to subset the data."),
      p("You can define grouping for colour annotation using the grouping checkboxes, which split the selected intensity 
         column names by underscores or dots and allow combining components to form group labels."),
      
      # ---- Method choice: PCA vs UMAP ----
      selectInput(
        ns("dr_method"),
        "Dimension Reduction Method:",
        choices = c("PCA", "UMAP"),
        selected = "PCA"
      ),
      
      # ---- Row selection ----
      radioButtons(
        ns("row_selection"), "Row Selection:",
        choices = c("All Rows" = "all", "Selected Rows by ID" = "selected")
      ),
      
      # Input field for manually selecting IDs (visible only if "Selected Rows by ID" is chosen)
      conditionalPanel(
        condition = sprintf("input['%s'] == 'selected'", ns("row_selection")),
        textInput(ns("id_selection"), "Manually select IDs (comma-separated):", "")
      ),
      
      # ---- Intensity columns ----
      selectInput(
        ns("intensity_columns"),
        "Select Intensity Columns:",
        choices = NULL,
        multiple = TRUE
      ),
      fluidRow(
        column(2, actionButton(ns("select_all_intensity"),   "Select All",   class = "btn-sm")),
        column(2, actionButton(ns("deselect_all_intensity"), "Deselect All", class = "btn-sm"))
      ),
      
      # ---- Grouping annotation controls ----
      h4("Select Components for Grouping Annotation"),
      uiOutput(ns("grouping_checkboxes_ui")),  # Dynamically generated checkboxes for grouping selection
      verbatimTextOutput(ns("group_annotation_preview")),  # Display resulting group names
      
      # ---- PCA-specific options ----
      h4("PCA Options"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'PCA'", ns("dr_method")),
        fluidRow(
          column(
            4,
            selectInput(ns("pca_x_pc"), "X-axis PC:", choices = NULL)
          ),
          column(
            4,
            selectInput(ns("pca_y_pc"), "Y-axis PC:", choices = NULL)
          )
        ),
        checkboxInput(ns("pca_center"), "Center data", value = TRUE),
        checkboxInput(ns("pca_scale"),  "Scale data",  value = TRUE)
      ),
      
      # ---- UMAP-specific options ----
      h4("UMAP Options"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'UMAP'", ns("dr_method")),
        fluidRow(
          column(
            4,
            numericInput(ns("umap_n_neighbors"), "n_neighbors", value = 15, min = 2, step = 1)
          ),
          column(
            4,
            numericInput(ns("umap_min_dist"), "min_dist", value = 0.1, min = 0, max = 1, step = 0.01)
          ),
          column(
            4,
            numericInput(ns("umap_n_components"), "n_components", value = 2, min = 2, max = 3, step = 1)
          )
        ),
        helpText("For plotting, only the first two UMAP components are used (UMAP1 vs UMAP2). 
                  Additional components can be used for downstream export or custom analysis.")
      ),
      
      # ---- Colour palette selection ----
      h4("Colour Settings"),
      selectInput(
        ns("color_scheme"),
        "Colour Palette:",
        choices = c(
          "Combined (global)" = "combined",
          "ggplot2 default"   = "ggplot",
          "Brewer Set1"       = "set1",
          "Brewer Set2"       = "set2",
          "Okabe-Ito (CB-friendly)" = "okabe"
        ),
        selected = "combined"
      ),
      
      # ---- Download & plot options ----
      numericInput(ns("pdf_width"),  "Plot Width",  value = 8, min = 4),
      numericInput(ns("pdf_height"), "Plot Height", value = 6, min = 4),
      numericInput(ns("point_size"), "Point Size",  value = 3, min = 1),
      numericInput(ns("label_size"), "Label Size",  value = 3, min = 1),
      
      fluidRow(
        column(4, downloadButton(ns("download_pdf"),    "Download Plot as PDF")),
        column(4, downloadButton(ns("download_coords"), "Download Coordinates (CSV)"))
      ),
      
      fluidRow(
        column(12, plotOutput(ns("pca_plot"), height = "600px"))
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'PCA'", ns("dr_method")),
        hr(),
        h4("Scree Plot"),
        fluidRow(
          column(12, plotOutput(ns("scree_plot"), height = "300px"))
        ),
        hr(),
        h4("PC Loadings"),
        p("Features ranked by absolute loading for the selected principal components.
           The table shows the union of the top N features from each selected PC.
           Click a column header to re-sort."),
        fluidRow(
          column(4, numericInput(ns("loadings_top_n"), "Top N features per PC:", value = 20, min = 1, step = 1)),
          column(4, tags$br(), downloadButton(ns("download_loadings"), "Download Full Loadings (CSV)", class = "btn-sm"))
        ),
        DT::dataTableOutput(ns("loadings_table"))
      )
  )
}

pca_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  # ---- Update intensity columns from main data reactive ----
  observe({
    req(data())
    updateSelectInput(session, "intensity_columns", choices = data()$intensity_cols)
  })
  
  # "Select All" button for intensity columns
  observeEvent(input$select_all_intensity, {
    req(data())
    updateSelectInput(session, "intensity_columns",
                      selected = data()$intensity_cols)
  })
  
  # "Deselect All" button for intensity columns
  observeEvent(input$deselect_all_intensity, {
    updateSelectInput(session, "intensity_columns",
                      selected = character(0))
  })
  
  # ---- Split column names into components (for grouping) ----
  split_components <- reactive({
    req(input$intensity_columns)
    col_names <- input$intensity_columns
    stringr::str_split_fixed(col_names, "_|\\.", n = 5)  # Limit to 5 components
  })
  
  # Dynamically generate checkboxes for grouping component selection
  output$grouping_checkboxes_ui <- renderUI({
    components <- split_components()
    num_components <- ncol(components)
    
    tagList(
      lapply(seq_len(num_components), function(i) {
        checkboxInput(
          ns(paste0("group_component_", i)),
          label = paste("Use Component", i),
          value = FALSE
        )
      })
    )
  })
  
  # Generate group annotations based on selected components
  group_annotations <- reactive({
    components <- split_components()
    selected_groups <- lapply(seq_len(ncol(components)), function(i) {
      if (isTRUE(input[[paste0("group_component_", i)]])) components[, i] else NULL
    })
    
    selected_groups <- selected_groups[!vapply(selected_groups, is.null, logical(1))]
    
    if (length(selected_groups) > 0) {
      apply(do.call(cbind, selected_groups), 1, paste, collapse = "_")
    } else {
      NULL
    }
  })
  
  # Preview group names
  output$group_annotation_preview <- renderText({
    annotations <- group_annotations()
    if (!is.null(annotations)) {
      paste("Group Names:", paste(annotations, collapse = ", "))
    } else {
      "No group names selected."
    }
  })
  
  # ---- Base data for DR: filter rows and select intensities ----
  dr_data <- reactive({
    req(input$row_selection, input$intensity_columns)
    df <- data()$data

    # Filter rows by selected IDs (if requested)
    if (input$row_selection == "selected" && nzchar(input$id_selection)) {
      selected_ids <- trimws(strsplit(input$id_selection, ",")[[1]])
      validate(need("id" %in% colnames(df),
                    "No 'id' column found; cannot filter by selected IDs."))
      df <- df[df$id %in% selected_ids, , drop = FALSE]
    }

    # Select only chosen intensity columns and coerce to numeric
    df <- df[, input$intensity_columns, drop = FALSE]
    df <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
    rownames(df) <- seq_len(nrow(df))

    # Drop features (rows) with any missing value; PCA/UMAP require a complete matrix
    n_before <- nrow(df)
    df <- df[complete.cases(df), , drop = FALSE]
    n_dropped <- n_before - nrow(df)
    if (n_dropped > 0) {
      showNotification(
        paste0(n_dropped, " feature(s) with missing values excluded from ",
               "dimension reduction (", nrow(df), " of ", n_before, " retained). ",
               "Consider using imputed intensities for a complete matrix."),
        type = "message", duration = 8
      )
    }

    validate(
      need(nrow(df) >= 3,
           paste0("Too few complete features for dimension reduction (",
                  nrow(df), " remain after removing features with missing values). ",
                  "Try selecting more samples or switch to imputed intensities."))
    )

    df
  })
  
  # ---- PCA results ----
  pca_results <- reactive({
    req(input$dr_method == "PCA")
    df <- dr_data()

    # Drop samples (columns) that are entirely non-finite (belt-and-suspenders)
    keep_cols <- vapply(df, function(x) any(is.finite(x)), logical(1))
    df <- df[, keep_cols, drop = FALSE]
    validate(
      need(ncol(df) > 1, "Need at least two samples with finite values for PCA.")
    )

    # prcomp expects variables in columns, samples in rows → transpose
    pca <- tryCatch(
      prcomp(t(df), scale. = input$pca_scale, center = input$pca_center),
      error = function(e) {
        validate(need(FALSE, paste0("PCA failed: ", conditionMessage(e))))
      }
    )
    
    pca_df <- as.data.frame(pca$x)
    pca_df$Sample <- rownames(pca_df)
    
    # Apply grouping annotations
    annotations <- group_annotations()
    if (!is.null(annotations)) {
      # annotations are per column (i.e., per sample), so we map by column order
      pca_df$Group <- annotations
    } else {
      pca_df$Group <- pca_df$Sample
    }
    
    var_explained <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    names(var_explained) <- colnames(pca$x)

    list(pca = pca, df = pca_df, var_explained = var_explained)
  })
  
  # Update PCA axis choices when PCA has been (re)computed
  observeEvent(pca_results(), {
    res <- pca_results()
    pc_names <- grep("^PC", names(res$df), value = TRUE)
    if (length(pc_names) == 0) return(NULL)
    
    x_default <- pc_names[1]
    y_default <- if (length(pc_names) >= 2) pc_names[2] else pc_names[1]
    
    updateSelectInput(session, "pca_x_pc", choices = pc_names, selected = x_default)
    updateSelectInput(session, "pca_y_pc", choices = pc_names, selected = y_default)
  })
  
  # ---- UMAP results (using umap::umap) ----
  umap_results <- reactive({
    req(input$dr_method == "UMAP")
    validate(
      need(requireNamespace("umap", quietly = TRUE),
           "The 'umap' package is not installed. Please install.packages('umap').")
    )
    
    df <- dr_data()
    keep_cols <- vapply(df, function(x) any(is.finite(x)), logical(1))
    df <- df[, keep_cols, drop = FALSE]
    
    validate(
      need(ncol(df) > 1, "Need at least two samples with finite values for UMAP.")
    )

    mat <- t(as.matrix(df))  # samples in rows

    n_neighbors  <- input$umap_n_neighbors
    min_dist     <- input$umap_min_dist
    n_components <- input$umap_n_components

    validate(
      need(n_neighbors < nrow(mat),
           "n_neighbors must be smaller than the number of samples.")
    )

    # Configure UMAP via umap.defaults
    config <- umap::umap.defaults
    config$n_neighbors  <- n_neighbors
    config$min_dist     <- min_dist
    config$n_components <- n_components

    umap_res <- tryCatch(
      umap::umap(mat, config = config),
      error = function(e) {
        validate(need(FALSE, paste0("UMAP failed: ", conditionMessage(e))))
      }
    )
    
    layout <- umap_res$layout
    umap_df <- as.data.frame(layout)
    colnames(umap_df) <- paste0("UMAP", seq_len(ncol(umap_df)))
    umap_df$Sample <- rownames(mat)
    
    annotations <- group_annotations()
    if (!is.null(annotations)) {
      umap_df$Group <- annotations
    } else {
      umap_df$Group <- umap_df$Sample
    }
    
    umap_df
  })
  
  # ---- Colour scale reactive ----
  color_scale <- reactive({
    scheme <- input$color_scheme
    
    if (scheme == "combined") {
      if (exists("combined_colors", inherits = TRUE)) {
        cols <- get("combined_colors", inherits = TRUE)
        if (is.null(cols) || length(cols) == 0) {
          return(scale_color_discrete())
        } else {
          return(scale_color_manual(values = cols))
        }
      } else {
        return(scale_color_discrete())
      }
    }
    
    if (scheme == "ggplot") {
      return(scale_color_discrete())
    }
    
    if (scheme == "set1") {
      return(scale_color_brewer(palette = "Set1"))
    }
    
    if (scheme == "set2") {
      return(scale_color_brewer(palette = "Set2"))
    }
    
    if (scheme == "okabe") {
      okabe_ito <- c(
        "#E69F00", "#56B4E9", "#009E73", "#F0E442",
        "#0072B2", "#D55E00", "#CC79A7", "#000000"
      )
      return(scale_color_manual(values = okabe_ito))
    }
    
    scale_color_discrete()
  })
  
  # ---- Unified plotting function ----
  create_dr_plot <- reactive({
    method <- input$dr_method
    col_scale <- color_scale()
    
    if (method == "PCA") {
      res    <- pca_results()
      pca_df <- res$df
      
      x_pc <- input$pca_x_pc
      y_pc <- input$pca_y_pc
      
      validate(
        need(x_pc %in% names(pca_df), "Selected X-axis PC not available."),
        need(y_pc %in% names(pca_df), "Selected Y-axis PC not available.")
      )
      
      var <- res$var_explained
      x_label <- sprintf("%s (%.1f%%)", x_pc, var[x_pc])
      y_label <- sprintf("%s (%.1f%%)", y_pc, var[y_pc])

      ggplot(pca_df, aes_string(x = x_pc, y = y_pc, color = "Group", label = "Sample")) +
        geom_point(size = input$point_size) +
        ggrepel::geom_text_repel(size = input$label_size) +
        labs(
          x = x_label,
          y = y_label,
          title = sprintf("PCA Plot (%s vs %s)", x_pc, y_pc)
        ) +
        theme_minimal() +
        col_scale
      
    } else {  # UMAP
      umap_df <- umap_results()
      
      validate(
        need("UMAP1" %in% names(umap_df) && "UMAP2" %in% names(umap_df),
             "UMAP did not produce at least two components.")
      )
      
      ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Group, label = Sample)) +
        geom_point(size = input$point_size) +
        ggrepel::geom_text_repel(size = input$label_size) +
        labs(
          x = "UMAP1",
          y = "UMAP2",
          title = "UMAP Plot (UMAP1 vs UMAP2)"
        ) +
        theme_minimal() +
        col_scale
    }
  })
  
  # ---- Coordinates table for export ----
  coords_table <- reactive({
    method <- input$dr_method
    
    if (method == "PCA") {
      res <- pca_results()
      df  <- res$df
      df$Method <- "PCA"
      df
    } else {
      df <- umap_results()
      df$Method <- "UMAP"
      df
    }
  })
  
  # ---- Render plot ----
  output$pca_plot <- renderPlot({
    p <- create_dr_plot()
    print(p)
  }, width = 800, height = 600)
  
  # ---- Download handler: plot as PDF ----
  output$download_pdf <- downloadHandler(
    filename = function() {
      method <- input$dr_method
      paste0(tolower(method), "_plot.pdf")
    },
    content = function(file) {
      pdf(file, width = input$pdf_width, height = input$pdf_height)
      p <- create_dr_plot()
      print(p)
      dev.off()
    }
  )
  
  # ---- Download handler: coordinates as CSV ----
  output$download_coords <- downloadHandler(
    filename = function() {
      method <- input$dr_method
      paste0(tolower(method), "_coordinates.csv")
    },
    content = function(file) {
      df <- coords_table()
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  # ---- Scree plot ----
  output$scree_plot <- renderPlot({
    req(input$dr_method == "PCA")
    res <- pca_results()
    var <- res$var_explained
    df  <- data.frame(
      PC      = factor(names(var), levels = names(var)),
      Variance = var
    )
    ggplot(df, aes(x = PC, y = Variance)) +
      geom_col(fill = "steelblue", width = 0.7) +
      geom_line(aes(group = 1), color = "firebrick", linewidth = 0.8) +
      geom_point(color = "firebrick", size = 2) +
      labs(
        title = "Scree Plot",
        x     = "Principal Component",
        y     = "Variance Explained (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, width = 800, height = 300)

  # ---- Feature IDs for PCA loadings ----
  # Mirrors the row-filtering and complete.cases logic in dr_data() so that
  # the i-th element maps to the i-th row of pca$rotation.
  feature_ids <- reactive({
    req(input$row_selection, input$intensity_columns)
    df_full <- data()$data

    if (input$row_selection == "selected" && nzchar(input$id_selection)) {
      selected_ids <- trimws(strsplit(input$id_selection, ",")[[1]])
      if ("id" %in% colnames(df_full))
        df_full <- df_full[df_full$id %in% selected_ids, , drop = FALSE]
    }

    df_int <- df_full[, input$intensity_columns, drop = FALSE]
    df_int <- as.data.frame(lapply(df_int, function(x) as.numeric(as.character(x))))
    complete_idx <- complete.cases(df_int)

    if ("id" %in% colnames(df_full)) df_full$id[complete_idx]
    else as.character(seq_len(sum(complete_idx)))
  })

  # ---- Full loadings matrix (features × all PCs) ----
  loadings_data <- reactive({
    req(input$dr_method == "PCA")
    res <- pca_results()
    ids <- feature_ids()
    rot <- as.data.frame(res$pca$rotation)
    rot <- cbind(Feature = ids, rot)
    rot
  })

  # ---- Loadings table: top N from each selected PC ----
  output$loadings_table <- DT::renderDataTable({
    req(input$pca_x_pc, input$pca_y_pc)
    ldf  <- loadings_data()
    x_pc <- input$pca_x_pc
    y_pc <- input$pca_y_pc
    n    <- max(1L, as.integer(input$loadings_top_n %||% 20L))

    top_x <- order(-abs(ldf[[x_pc]]))[seq_len(min(n, nrow(ldf)))]
    top_y <- order(-abs(ldf[[y_pc]]))[seq_len(min(n, nrow(ldf)))]
    keep  <- unique(c(top_x, top_y))

    display_df <- ldf[keep, c("Feature", x_pc, y_pc), drop = FALSE]
    display_df <- display_df[order(-abs(display_df[[x_pc]])), ]

    DT::datatable(
      display_df,
      options  = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    ) |>
      DT::formatRound(columns = c(x_pc, y_pc), digits = 4)
  })

  # ---- Download full loadings ----
  output$download_loadings <- downloadHandler(
    filename = function() paste0("pca_loadings_", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(loadings_data(), file, row.names = FALSE)
  )
  })
}