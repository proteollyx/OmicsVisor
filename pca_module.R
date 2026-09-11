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
      # An explicit table supersedes filename parsing when supplied; the
      # heuristic stays the default because it asks nothing of the user and
      # works for the standard export (audit OV-UX-17).
      fileInput(ns("sample_metadata"),
                "Sample metadata (optional: CSV, TSV or XLSX)",
                accept = c(".csv", ".tsv", ".txt", ".xlsx")),
      helpText(paste("Needs a 'sample' column matching the intensity column names,",
                     "plus any attributes you want to group by (condition, batch,",
                     "replicate). When supplied it replaces the component checkboxes",
                     "below.")),
      div(style = "margin: 4px 0 10px 0;",
          downloadButton(ns("download_metadata_template"),
                         "Download metadata template (CSV)", class = "btn-sm"),
          tags$span(style = "margin-left:8px; color:#555; font-size:0.85em;",
                    "Pre-filled with your sample names and current grouping.")),
      uiOutput(ns("metadata_status")),
      uiOutput(ns("metadata_group_ui")),

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
        # Default changed to FALSE in v1.4.0 (audit 2.8). Autoscaling gives a
        # protein with negligible dynamic range the same weight as a highly
        # variable one; for already-normalised log intensities, centring alone
        # is the better default, and it is what prcomp() itself defaults to.
        # The setting is printed on the plot and recorded in the manifest, so
        # a figure can always be traced to the choice that produced it.
        checkboxInput(ns("pca_scale"),  "Scale data (unit variance per feature)",
                      value = FALSE),
        helpText(paste("Scaling weights every feature equally regardless of its",
                       "dynamic range. For normalised log intensities, leaving it",
                       "off is usually the better choice. Changed from on to off",
                       "in v1.4.0; the active setting is shown on the plot."))
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
        fluidRow(
          column(
            4,
            numericInput(ns("umap_seed"), "Random seed", value = 1, min = 0, step = 1)
          )
        ),
        helpText("UMAP is stochastic. The seed fixes the embedding so the same
                  data and settings reproduce the same coordinates; it is
                  recorded in the exported coordinate file."),
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
          "Okabe-Ito (colourblind-safe up to 8 groups)" = "okabe"
        ),
        selected = "combined"
      ),
      uiOutput(ns("palette_note")),
      
      # ---- Download & plot options ----
      numericInput(ns("pdf_width"),  "Plot Width",  value = 8, min = 4),
      numericInput(ns("pdf_height"), "Plot Height", value = 6, min = 4),
      numericInput(ns("point_size"), "Point Size",  value = 3, min = 1),
      numericInput(ns("label_size"), "Label Size",  value = 3, min = 1),
      
      fluidRow(
        column(4, downloadButton(ns("download_pdf"),    "Download Plot as PDF")),
        column(4, downloadButton(ns("download_coords"), "Download Coordinates (CSV)"))
      ),
      
      uiOutput(ns("dr_retention")),

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

pca_server <- function(id, data, register = NULL) {
  moduleServer(id, function(input, output, session) {

    observe({
      # Retention is part of the record: a PCA of 400 features and one of
      # 8,000 are different analyses even with identical settings.
      dd  <- try(dr_data(), silent = TRUE)
      ret <- if (inherits(dd, "try-error")) NULL else dd$retention
      ov_register_settings(register, "PCA / UMAP", list(
        method             = input$dr_method,
        intensity_columns  = length(input$intensity_columns %||% character(0)),
        pca_centred        = input$pca_center,
        pca_scaled         = input$pca_scale,
        umap_seed          = if (identical(input$dr_method, "UMAP")) input$umap_seed else NULL,
        umap_n_neighbors   = if (identical(input$dr_method, "UMAP")) input$umap_n_neighbors else NULL,
        umap_min_dist      = if (identical(input$dr_method, "UMAP")) input$umap_min_dist else NULL,
        features_retained  = if (is.null(ret)) NULL else ret$n_complete,
        features_excluded  = if (is.null(ret)) NULL else ret$n_dropped,
        colour_palette     = input$color_scheme,
        grouping           = if (is.null(input$sample_metadata))
                               "parsed from intensity column names"
                             else paste0("sample metadata file: ",
                                         input$sample_metadata$name),
        grouped_by         = if (is.null(input$sample_metadata)) NULL
                             else input$metadata_group_cols
      ))
    })

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
  
  # ---- Optional explicit sample metadata ----
  sample_metadata <- reactive({
    req(input$sample_metadata)
    tryCatch(
      ov_read_sample_metadata(input$sample_metadata$datapath,
                              input$sample_metadata$name,
                              samples = input$intensity_columns),
      error = function(e) {
        showNotification(paste("Sample metadata:", conditionMessage(e)),
                         type = "error", duration = 14)
        NULL
      }
    )
  })

  # What matched and what did not. A metadata file that silently applies to
  # half the samples would be worse than none, because the grouping would
  # still look deliberate.
  # Seeded from the component selection, so the explicit table starts from the
  # heuristic's best guess rather than a blank sheet.
  output$download_metadata_template <- downloadHandler(
    filename = function()
      paste0("OmicsVisor_sample_metadata_template_", Sys.Date(), ".csv"),
    content = function(file) {
      samples <- input$intensity_columns %||% character(0)
      cond <- tryCatch(group_annotations(), error = function(e) NULL)
      if (!is.null(cond) && length(cond) != length(samples)) cond <- NULL
      utils::write.csv(ov_metadata_template(samples, condition = cond),
                       file, row.names = FALSE)
    }
  )

  output$metadata_status <- renderUI({
    if (is.null(input$sample_metadata)) return(NULL)
    m <- sample_metadata()
    if (is.null(m)) return(NULL)
    n_ok <- length(m$matched)
    n_all <- length(input$intensity_columns %||% character(0))
    tone <- if (n_ok == n_all) "#18682a" else "#8a4b00"
    tagList(div(
      style = sprintf(paste("border-left:3px solid %s; background:#F7F9FB;",
                            "padding:6px 10px; margin:4px 0 8px 0; font-size:0.88em;"), tone),
      tags$div(style = sprintf("color:%s; font-weight:600;", tone),
               sprintf("%d of %d samples matched", n_ok, n_all)),
      if (length(m$warnings))
        tags$ul(style = "margin:4px 0 0 0; padding-left:16px; color:#8a4b00;",
                lapply(m$warnings, tags$li)) else NULL
    ))
  })

  output$metadata_group_ui <- renderUI({
    if (is.null(input$sample_metadata)) return(NULL)
    m <- sample_metadata()
    if (is.null(m)) return(NULL)
    selectInput(ns("metadata_group_cols"), "Group samples by:",
                choices = m$attributes,
                selected = m$attributes[1], multiple = TRUE)
  })

  # Generate group annotations: explicit metadata wins, filename parsing is
  # the fallback.
  group_annotations <- reactive({
    m <- if (is.null(input$sample_metadata)) NULL else sample_metadata()
    if (!is.null(m)) {
      cols <- input$metadata_group_cols %||% m$attributes[1]
      g <- ov_metadata_groups(m, input$intensity_columns, cols)
      if (!is.null(g)) return(g)
    }

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

    # Select only chosen intensity columns and coerce to numeric.
    # check.names = FALSE: without it make.names() silently rewrites any
    # non-syntactic sample name ("Imputed 1" -> "Imputed.1"), so the PCA
    # points end up labelled with something the user never chose.
    # Capture the identifiers before subsetting to intensity columns, so that
    # every downstream filter can carry them along instead of reconstructing
    # them. A separate reactive that re-derived the ids used to drift out of
    # step with the zero-variance filter below, mislabelling the loadings.
    ids <- if ("id" %in% colnames(df)) as.character(df$id)
           else as.character(seq_len(nrow(df)))

    df <- df[, input$intensity_columns, drop = FALSE]
    df <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))),
                        check.names = FALSE, stringsAsFactors = FALSE)
    names(df)    <- input$intensity_columns
    rownames(df) <- seq_len(nrow(df))

    # PCA and UMAP require a complete matrix, so any feature with a missing
    # value in any selected sample is dropped. How many that is, and which
    # sample is responsible, is reported persistently beside the plot rather
    # than in a notification that disappears (audit OV-NUM-09).
    ret <- ov_dr_retention(df)
    df  <- df[ret$complete, , drop = FALSE]
    ids <- ids[ret$complete]

    validate(
      need(nrow(df) >= 3,
           paste0("Too few complete features for dimension reduction (",
                  nrow(df), " of ", ret$n_in,
                  " remain after removing features with missing values). ",
                  "Try selecting fewer samples, or switch to imputed intensities."))
    )

    list(mat = df, ids = ids, retention = ret)
  })
  
  # ---- PCA results ----
  pca_results <- reactive({
    req(input$dr_method == "PCA")
    dd  <- dr_data()
    df  <- dd$mat
    ids <- dd$ids

    # Drop samples (columns) that are entirely non-finite (belt-and-suspenders)
    keep_cols <- vapply(df, function(x) any(is.finite(x)), logical(1))
    df <- df[, keep_cols, drop = FALSE]
    validate(
      need(ncol(df) > 1, "Need at least two samples with finite values for PCA.")
    )
    if (any(!keep_cols))
      showNotification(
        paste0(sum(!keep_cols), " sample(s) dropped from PCA: no finite values (",
               paste(names(keep_cols)[!keep_cols], collapse = ", "), ")."),
        type = "warning", duration = 8
      )

    # prcomp(scale. = TRUE) cannot rescale a constant column, and a feature that
    # is constant across the selected samples is routine - single-value
    # imputation upstream produces them readily (audit OV-NUM-08). Drop them
    # rather than letting one such feature take down the whole PCA.
    if (isTRUE(input$pca_scale)) {
      feature_sd <- apply(df, 1, stats::sd, na.rm = TRUE)
      keep_rows  <- is.finite(feature_sd) & feature_sd > sqrt(.Machine$double.eps)
      if (any(!keep_rows)) {
        showNotification(
          sprintf("%d zero-variance feature(s) excluded from the scaled PCA.",
                  sum(!keep_rows)),
          type = "warning", duration = 8)
        df  <- df[keep_rows, , drop = FALSE]
        ids <- ids[keep_rows]          # keep labels in lockstep with rotation
      }
      validate(need(nrow(df) >= 3,
                    "Too few varying features remain for a scaled PCA. Uncheck
                     'Scale data' or select more samples."))
    }

    # prcomp expects variables in columns, samples in rows → transpose
    pca <- tryCatch(
      prcomp(t(df), scale. = input$pca_scale, center = input$pca_center),
      error = function(e) {
        validate(need(FALSE, paste0("PCA failed: ", conditionMessage(e))))
      }
    )
    
    pca_df <- as.data.frame(pca$x)
    pca_df$Sample <- rownames(pca_df)

    # Apply grouping annotations. They are computed per selected intensity
    # column, so they must be subset by keep_cols before being attached —
    # otherwise dropping a sample makes the lengths disagree.
    annotations <- group_annotations()
    if (!is.null(annotations) && length(annotations) == length(keep_cols)) {
      pca_df$Group <- annotations[keep_cols]
    } else {
      pca_df$Group <- pca_df$Sample
    }
    
    var_explained <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    names(var_explained) <- colnames(pca$x)

    list(pca = pca, df = pca_df, var_explained = var_explained, ids = ids)
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
    
    df <- dr_data()$mat
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
    # UMAP is stochastic; without a fixed random_state the same data and
    # settings give a different embedding on every run, and the exported
    # coordinates carry no record of which one was produced (audit OV-REP-04).
    config$random_state <- as.integer(input$umap_seed %||% 1L)

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
    if (!is.null(annotations) && length(annotations) == length(keep_cols)) {
      umap_df$Group <- annotations[keep_cols]
    } else {
      umap_df$Group <- umap_df$Sample
    }

    umap_df$umap_seed <- config$random_state
    umap_df
  })
  
  # ---- Retention panel ----
  # How much of the data the ordination is actually based on. Complete-case
  # filtering can remove most features without the user noticing, and a PCA of
  # 400 features looks exactly as convincing as one of 8,000 (audit OV-NUM-09).
  output$dr_retention <- renderUI({
    dd <- try(dr_data(), silent = TRUE)
    if (inherits(dd, "try-error")) return(NULL)
    r <- dd$retention

    severe <- is.finite(r$pct_retained) && r$pct_retained < 50
    accent <- if (severe) "#8a4b00" else "#1E3791"
    bg     <- if (severe) "#FDF6EC" else "#F3F7FB"

    ab_shift <- if (is.null(r$abundance)) NA_real_ else r$abundance$shift
    worst <- r$worst_sample
    advice <- if (!is.null(worst) && worst$recoverable > 0)
      tags$div(
        style = "margin-top:6px;",
        sprintf("Excluding %s alone would recover %s feature%s (%.0f%% \u2192 %.0f%%).",
                worst$sample, format(worst$recoverable, big.mark = ","),
                if (worst$recoverable == 1) "" else "s",
                r$pct_retained,
                100 * (r$n_complete + worst$recoverable) / r$n_in))
    else NULL

    tagList(div(
      style = sprintf(paste("border:1px solid #dbe4ee; border-left:4px solid %s;",
                            "background:%s; border-radius:4px;",
                            "padding:10px 14px; margin:4px 0 12px 0; font-size:0.92em;"),
                      accent, bg),
      tags$div(style = sprintf("font-weight:700; color:%s;", accent),
               "Features used for dimension reduction"),
      tags$div(
        style = "margin-top:4px;",
        sprintf("%s of %s features complete across the %d selected sample%s (%.1f%%).",
                format(r$n_complete, big.mark = ","),
                format(r$n_in,       big.mark = ","),
                nrow(r$per_sample),
                if (nrow(r$per_sample) == 1) "" else "s",
                r$pct_retained),
        if (r$n_dropped > 0)
          sprintf(" %s dropped for missing values.", format(r$n_dropped, big.mark = ","))
        else NULL
      ),
      advice,

      # Counting the loss is not the same as knowing whether it mattered.
      if (!is.na(ab_shift)) tags$div(
        style = "margin-top:6px;",
        sprintf(
          "Discarded features are %s in abundance than retained ones (median %.2f vs %.2f). %s",
          if (ab_shift < 0) "lower" else "higher",
          r$abundance$lost[["median"]], r$abundance$kept[["median"]],
          if (abs(ab_shift) >= 0.5)
            "Complete-case filtering has not removed a random subsample of the proteome."
          else
            "The two distributions are close, so the filter looks broadly unbiased.")) else NULL,

      if (r$n_dropped > 0 && !is.na(ab_shift)) tags$details(
        style = "margin-top:6px;",
        tags$summary(style = "cursor:pointer; color:#1E3791;",
                     "Abundance of retained vs discarded features"),
        plotOutput(ns("retention_abundance"), height = "220px")
      ) else NULL,

      if (r$n_dropped > 0) tags$details(
        style = "margin-top:6px;",
        tags$summary(style = "cursor:pointer; color:#1E3791;", "Missing values per sample"),
        tags$table(
          style = "margin-top:4px; border-collapse:collapse;",
          tags$tr(lapply(c("Sample", "Missing", "%", "Recoverable"), function(h)
            tags$th(style = "text-align:left; padding:1px 12px 1px 0; color:#555; font-weight:600;", h))),
          apply(r$per_sample, 1, function(row) tags$tr(
            tags$td(style = "padding:1px 12px 1px 0;", row[["sample"]]),
            tags$td(style = "padding:1px 12px 1px 0;", format(as.integer(row[["n_missing"]]), big.mark = ",")),
            tags$td(style = "padding:1px 12px 1px 0;", sprintf("%.1f", as.numeric(row[["pct_missing"]]))),
            tags$td(style = "padding:1px 12px 1px 0;", format(as.integer(row[["recoverable"]]), big.mark = ","))
          ))
        )
      ) else NULL
    ))
  })

  # The distribution behind the one-line summary above. Intensity-dependent
  # dropout shows up here as a discarded population sitting to the left of the
  # retained one; a benign filter overlays the two.
  output$retention_abundance <- renderPlot({
    dd <- try(dr_data(), silent = TRUE)
    if (inherits(dd, "try-error")) return(NULL)
    r <- dd$retention
    req(r$n_dropped > 0, any(is.finite(r$abundance$values)))

    pdf_df <- data.frame(
      abundance = r$abundance$values,
      status    = ifelse(r$complete, "Retained", "Discarded"),
      stringsAsFactors = FALSE
    )
    pdf_df <- pdf_df[is.finite(pdf_df$abundance), , drop = FALSE]
    pdf_df$status <- factor(pdf_df$status, levels = c("Retained", "Discarded"))

    ggplot(pdf_df, aes(x = abundance, fill = status, colour = status)) +
      geom_density(alpha = 0.35, linewidth = 0.5, na.rm = TRUE) +
      scale_fill_manual(values   = c(Retained = "#1E3791", Discarded = "#C2570A")) +
      scale_colour_manual(values = c(Retained = "#1E3791", Discarded = "#C2570A")) +
      labs(
        x = "Mean intensity across selected samples (feature)",
        y = "Density", fill = NULL, colour = NULL,
        title = "What complete-case filtering removed",
        subtitle = sprintf(
          "%s retained, %s discarded \u2014 median shift %+.2f",
          format(r$abundance$n_kept, big.mark = ","),
          format(r$abundance$n_lost, big.mark = ","),
          r$abundance$shift)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  # ---- Colour scale ----
  # Every fixed palette is finite (Okabe-Ito has 8 colours, Brewer Set2 has 8,
  # Set1 has 9). With one group per sample a routine experiment exceeds that
  # and scale_*_manual()/scale_*_brewer() abort with "Insufficient values in
  # manual scale". Interpolating to n_groups keeps the intended look and never
  # takes the plot down.
  color_scale <- function(n_groups) {
    scheme <- input$color_scheme %||% "combined"
    n      <- max(1L, as.integer(n_groups))

    base_cols <- switch(
      scheme,
      combined = if (exists("combined_colors", inherits = TRUE))
                   get("combined_colors", inherits = TRUE) else NULL,
      set1     = RColorBrewer::brewer.pal(9, "Set1"),
      set2     = RColorBrewer::brewer.pal(8, "Set2"),
      okabe    = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                   "#0072B2", "#D55E00", "#CC79A7", "#000000"),
      NULL
    )

    if (is.null(base_cols) || length(base_cols) == 0)
      return(scale_color_discrete())

    scale_color_manual(values = ov_expand_palette(base_cols, n))
  }

  # Okabe-Ito is colourblind-safe because of the specific eight colours it
  # contains. Interpolating past eight keeps the plot from failing but
  # produces intermediate colours the palette never claimed to distinguish,
  # so the accessibility guarantee no longer holds. The audit was right that
  # this needs saying rather than being silently inherited.
  output$palette_note <- renderUI({
    n <- tryCatch({
      d <- if (identical(input$dr_method, "UMAP")) umap_results() else pca_results()$df
      length(unique(d$Group))
    }, error = function(e) NA_integer_)

    if (is.na(n)) return(NULL)
    cap <- switch(input$color_scheme %||% "combined",
                  okabe = 8L, set2 = 8L, set1 = 9L, NA_integer_)
    if (is.na(cap) || n <= cap) return(NULL)

    div(style = paste("border-left:3px solid #8a4b00; background:#FDF6EC;",
                      "padding:6px 10px; margin:4px 0 8px 0; font-size:0.88em;",
                      "color:#8a4b00;"),
        sprintf(paste("%d groups exceed this palette's %d distinct colours.",
                      "The extra colours are interpolated, so the palette's",
                      "colourblind-safe property no longer holds. Consider",
                      "fewer groups for a figure that must be accessible."),
                n, cap))
  })
  
  # ---- Unified plotting function ----
  create_dr_plot <- reactive({
    method <- input$dr_method

    if (method == "PCA") {
      res    <- pca_results()
      pca_df <- res$df
      col_scale <- color_scale(length(unique(pca_df$Group)))

      x_pc <- input$pca_x_pc
      y_pc <- input$pca_y_pc
      
      validate(
        need(x_pc %in% names(pca_df), "Selected X-axis PC not available."),
        need(y_pc %in% names(pca_df), "Selected Y-axis PC not available.")
      )
      
      var <- res$var_explained
      x_label <- sprintf("%s (%.1f%%)", x_pc, var[x_pc])
      y_label <- sprintf("%s (%.1f%%)", y_pc, var[y_pc])

      ggplot(pca_df, aes(x = .data[[x_pc]], y = .data[[y_pc]],
                         color = .data$Group, label = .data$Sample)) +
        geom_point(size = input$point_size) +
        ggrepel::geom_text_repel(size = input$label_size) +
        labs(
          x = x_label,
          y = y_label,
          title = sprintf("PCA Plot (%s vs %s)", x_pc, y_pc),
          # Which preprocessing produced this plot, stated on the plot. Two
          # PCAs of the same data with different scaling can look entirely
          # different, and the figure is what outlives the session.
          subtitle = sprintf("%s \u00b7 %s \u00b7 %s features",
                             if (isTRUE(input$pca_center)) "centred" else "not centred",
                             if (isTRUE(input$pca_scale)) "scaled to unit variance"
                             else "not scaled",
                             format(nrow(res$pca$rotation), big.mark = ","))
        ) +
        theme_minimal() +
        col_scale
      
    } else {  # UMAP
      umap_df   <- umap_results()
      col_scale <- color_scale(length(unique(umap_df$Group)))

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
      ggsave(file, plot = create_dr_plot(), device = ov_pdf_device(),
             width = input$pdf_width, height = input$pdf_height)
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

  # ---- Full loadings matrix (features × all PCs) ----
  loadings_data <- reactive({
    req(input$dr_method == "PCA")
    res <- pca_results()
    rot <- as.data.frame(res$pca$rotation)
    # res$ids is produced by the same filtering that produced the rotation,
    # so this can no longer silently recycle a mismatched label vector.
    stopifnot(length(res$ids) == nrow(rot))
    cbind(Feature = res$ids, rot)
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