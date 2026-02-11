# upset_plot_module.R
# UpSet plots with global cutoffs for all logFC_ (and matching adj.P.Val_) columns
# ID column is assumed to be the first column in the data frame and unique.

upset_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("UpSet Plot"),
      p("Create UpSet plots of overlapping 'hit' sets defined by global cutoffs on all logFC_ (and matching adj.P.Val_) columns."),
      p("Each logFC_ column defines one set; hits are called using the global cutoffs and direction."),
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Global Cutoffs"),
            
            div(
              style = "margin-bottom:6px; color:#555;",
              tags$b("ID column:"),
              span(" first column of the data (assumed unique)")
            ),
            
            radioButtons(
              ns("direction"), "Direction:",
              choices = c(
                "Both (|logFC| ≥ cutoff)" = "both",
                "Up (logFC ≥ cutoff)"     = "up",
                "Down (logFC ≤ -cutoff)"  = "down"
              ),
              selected = "both"
            ),
            
            numericInput(
              ns("logfc_cutoff"),
              label = "Global logFC cutoff:",
              value = 1,
              step = 0.1
            ),
            
            numericInput(
              ns("adjp_cutoff"),
              label = "Global adj.P.Val cutoff (≤):",
              value = 0.05,
              step = 0.01
            ),
            helpText("For each logFC_ column, OmicsVisor will try to use a matching adj.P.Val_ column with the same suffix."),
            
            hr(),
            h4("Plot options"),
            numericInput(
              ns("n_intersects"),
              "Max number of intersections to display:",
              value = 40, min = 1, step = 1
            ),
            numericInput(
              ns("min_set_size"),
              "Minimum set size (IDs per set):",
              value = 1, min = 1, step = 1
            ),
            
            hr(),
            downloadButton(ns("download_pdf"),  "Download UpSet Plot (PDF)"),
            br(), br(),
            downloadButton(ns("download_csv"),  "Download Membership (CSV)")
          )
        ),
        
        column(
          width = 9,
          h4("UpSet Plot"),
          plotOutput(ns("upset_plot"), height = "600px"),
          br(),
          h4("Set Membership Preview"),
          DT::dataTableOutput(ns("membership_table"))
        )
      )
    )
  )
}

upset_plot_server <- function(input, output, session, data) {
  ns <- session$ns
  
  # --- Data access ----------------------------------------------------------
  req_data <- reactive({
    d <- data()
    validate(need(!is.null(d) && !is.null(d$data), "No data available"))
    as.data.frame(d$data, stringsAsFactors = FALSE)
  })
  
  # --- Pure helper: given a plain data.frame, build membership & upset input ----
  build_upset_from_df <- function(df,
                                  direction,
                                  logfc_cut,
                                  adjp_cut,
                                  min_set_size,
                                  n_intersects) {
    stopifnot(is.data.frame(df))
    nms <- names(df)
    
    # ID = first column
    id_col <- nms[1]
    
    logfc_cols <- nms[grepl("^logFC_", nms)]
    if (length(logfc_cols) == 0L) {
      return(list(
        ok          = FALSE,
        reason      = "No logFC_ columns found. UpSet requires logFC_* columns.",
        membership  = NULL,
        upset_input = NULL,
        id_col      = id_col
      ))
    }
    
    # find matching adj.P.Val_ columns
    adj_cols <- rep(NA_character_, length(logfc_cols))
    for (i in seq_along(logfc_cols)) {
      lf  <- logfc_cols[i]
      suf <- sub("^logFC[_\\.]*", "", lf)
      pat1 <- paste0("^adj\\.?P\\.?Val[_\\.]*", suf, "$")
      hit <- nms[grepl(pat1, nms, ignore.case = TRUE)]
      if (length(hit) > 0) adj_cols[i] <- hit[1]
    }
    
    ids <- df[[id_col]]
    
    # numeric coercion
    for (lf in logfc_cols) {
      df[[lf]] <- suppressWarnings(as.numeric(df[[lf]]))
    }
    for (ac in adj_cols[!is.na(adj_cols)]) {
      df[[ac]] <- suppressWarnings(as.numeric(df[[ac]]))
    }
    
    # build membership: one logical column per logFC_ column
    mat_list <- vector("list", length(logfc_cols))
    names(mat_list) <- logfc_cols
    
    for (i in seq_along(logfc_cols)) {
      lf <- logfc_cols[i]
      ac <- adj_cols[i]
      
      v <- df[[lf]]
      if (direction == "both") {
        hits <- abs(v) >= logfc_cut
      } else if (direction == "up") {
        hits <- v >= logfc_cut
      } else {
        hits <- v <= -logfc_cut
      }
      
      if (!is.na(ac)) {
        pv <- df[[ac]]
        hits <- hits & is.finite(pv) & pv <= adjp_cut
      }
      
      hits[is.na(hits)] <- FALSE
      mat_list[[lf]] <- hits
    }
    
    membership <- as.data.frame(mat_list, stringsAsFactors = FALSE)
    membership[[id_col]] <- ids
    
    if (nrow(membership) == 0L) {
      return(list(
        ok          = FALSE,
        reason      = "No data available for UpSet (no rows in membership).",
        membership  = NULL,
        upset_input = NULL,
        id_col      = id_col
      ))
    }
    
    # keep only IDs that are in at least one set
    in_any <- apply(membership[, logfc_cols, drop = FALSE], 1, any)
    membership <- membership[in_any, , drop = FALSE]
    
    if (nrow(membership) == 0L) {
      return(list(
        ok          = FALSE,
        reason      = "No IDs passed the current global cutoffs.",
        membership  = NULL,
        upset_input = NULL,
        id_col      = id_col
      ))
    }
    
    upset_input <- membership[, logfc_cols, drop = FALSE]
    upset_input[] <- lapply(upset_input, function(x) as.integer(x))
    
    # minimum set size
    if (is.null(min_set_size) || is.na(min_set_size) || min_set_size < 1) {
      min_set_size <- 1L
    }
    set_sizes <- colSums(upset_input)
    keep_sets <- names(set_sizes)[set_sizes >= min_set_size]
    
    if (length(keep_sets) == 0L) {
      return(list(
        ok          = FALSE,
        reason      = "No logFC_ sets meet the minimum set size criterion.",
        membership  = membership,
        upset_input = NULL,
        id_col      = id_col
      ))
    }
    
    if (length(keep_sets) < 2L) {
      return(list(
        ok          = FALSE,
        reason      = "UpSet requires ≥ 2 sets. Adjust cutoffs or min. set size.",
        membership  = membership,
        upset_input = NULL,
        id_col      = id_col
      ))
    }
    
    upset_input <- upset_input[, keep_sets, drop = FALSE]
    
    if (is.null(n_intersects) || is.na(n_intersects) || n_intersects < 1) {
      n_intersects <- 40L
    }
    
    list(
      ok          = TRUE,
      reason      = NULL,
      membership  = membership,
      upset_input = upset_input,
      id_col      = id_col,
      n_inter     = as.integer(n_intersects)
    )
  }
  
  # --- Reactive: membership for table/CSV -----------------------------------
  membership_data <- reactive({
    df <- req_data()
    res <- build_upset_from_df(
      df           = df,
      direction    = input$direction,
      logfc_cut    = input$logfc_cutoff,
      adjp_cut     = input$adjp_cutoff,
      min_set_size = 1,  # membership table doesn't filter by set size
      n_intersects = input$n_intersects
    )
    
    validate(need(res$ok, res$reason %||% "Unable to build membership matrix."))
    res$membership
  })
  
  # --- Plot: UpSet ----------------------------------------------------------
  output$upset_plot <- renderPlot({
    validate(need(requireNamespace("UpSetR", quietly = TRUE),
                  "Package 'UpSetR' is required. Please install.packages('UpSetR')."))
    
    df <- req_data()
    res <- build_upset_from_df(
      df           = df,
      direction    = input$direction,
      logfc_cut    = input$logfc_cutoff,
      adjp_cut     = input$adjp_cutoff,
      min_set_size = input$min_set_size,
      n_intersects = input$n_intersects
    )
    
    validate(need(res$ok, res$reason %||% "Unable to build UpSet input."))
    
    # UpSetR uses grid; wrap in print() for safety
    print(
      UpSetR::upset(
        res$upset_input,
        nsets       = ncol(res$upset_input),
        nintersects = res$n_inter,
        order.by    = "freq"
      )
    )
  })
  
  # --- Membership table -----------------------------------------------------
  output$membership_table <- DT::renderDataTable({
    mem <- membership_data()
    DT::datatable(
      mem,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100, 200),
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # --- Download PDF ---------------------------------------------------------
  output$download_pdf <- downloadHandler(
    filename = function() "upset_plot_global_cutoffs.pdf",
    content = function(file) {
      if (!requireNamespace("UpSetR", quietly = TRUE)) {
        stop("Package 'UpSetR' is required. Please install.packages('UpSetR').")
      }
      
      df <- req_data()
      res <- build_upset_from_df(
        df           = df,
        direction    = input$direction,
        logfc_cut    = input$logfc_cutoff,
        adjp_cut     = input$adjp_cutoff,
        min_set_size = input$min_set_size,
        n_intersects = input$n_intersects
      )
      
      pdf(file, width = 8, height = 6)
      on.exit(dev.off(), add = TRUE)
      
      if (!isTRUE(res$ok) || is.null(res$upset_input)) {
        plot.new()
        text(
          0.5, 0.5,
          res$reason %||% "Unable to build UpSet input for export.",
          cex = 1.1
        )
      } else {
        print(
          UpSetR::upset(
            res$upset_input,
            nsets       = ncol(res$upset_input),
            nintersects = res$n_inter,
            order.by    = "freq"
          )
        )
      }
    }
  )
  
  # --- Download CSV for membership matrix -----------------------------------
  output$download_csv <- downloadHandler(
    filename = function() "upset_membership_matrix.csv",
    content = function(file) {
      mem <- membership_data()
      utils::write.csv(mem, file, row.names = FALSE)
    }
  )
}