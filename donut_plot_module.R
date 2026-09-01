# ─────────────────────────────────────────────────────────
# OmicsVisor - Donut Plot Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
donut_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Donut Plot Module"),
    p(em("Finding commonly up/down-regulated candidates across comparisons."),
      style = "color: #666; margin-top: -6px; font-size: 1.05em;"),
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

    # The donuts are drawn from a snapshot taken when "Apply Cutoff" is
    # clicked. The ID selection below must read the same snapshot, or editing
    # a cutoff without re-applying it silently returns IDs that disagree with
    # the plotted counts.
    applied <- reactiveValues(logfc = 1, pval = 0.05)
    observeEvent(input$apply_cutoff, {
      applied$logfc <- input$logfc_cutoff
      applied$pval  <- input$pval_cutoff
    }, ignoreInit = FALSE)

    # Single source of truth for "which IDs are hits in comparison i".
    # This used to be spelled out at three separate call sites, which is how
    # the v1.0.4 NA-inflation bug came to be fixed in one of them but not the
    # others. Everything below now goes through here.
    hits_for <- function(i, logfc_cut, pval_cut) {
      empty     <- list(up = character(0), down = character(0),
                        all_ids = character(0), title = "")
      logfc_col <- data()$logFC_cols[i]
      if (is.na(logfc_col)) return(empty)

      adjp_col <- gsub("logFC", "adj.P.Val", logfc_col)
      df       <- data()$data
      lfc      <- df[[logfc_col]]
      adjp     <- df[[adjp_col]]
      title    <- gsub("\\.", " ", sub("logFC_", "", logfc_col))

      # A logFC column without a matching adj.P.Val column cannot be
      # thresholded; show it as all-"Other" rather than as zero features.
      if (is.null(lfc) || is.null(adjp))
        return(list(up = character(0), down = character(0),
                    all_ids = df$id, title = title))

      # !is.na() guards are essential: NA < 0.05 is NA, and subsetting with NA
      # inserts NA elements that length() then counts as hits.
      valid <- !is.na(lfc) & !is.na(adjp) & adjp < pval_cut
      list(
        up      = df$id[valid & lfc >  logfc_cut],
        down    = df$id[valid & lfc < -logfc_cut],
        all_ids = df$id,
        title   = title
      )
    }

    observeEvent(input$apply_cutoff, {
      n_comp <- length(data()$logFC_cols)

      output$donut_plots_ui <- renderUI({
        tagList(
          lapply(seq_len(n_comp), function(i) {
            title <- hits_for(i, applied$logfc, applied$pval)$title
            tagList(
              plotOutput(ns(paste0("donut_plot_", i)), height = "300px"),
              checkboxInput(ns(paste0("select_up_",   i)), label = paste("Select Upregulated for",   title)),
              checkboxInput(ns(paste0("select_down_", i)), label = paste("Select Downregulated for", title)),
              hr()
            )
          })
        )
      })

      lapply(seq_len(n_comp), function(i) {
        h <- hits_for(i, applied$logfc, applied$pval)
        output[[paste0("donut_plot_", i)]] <- renderPlot({
          donut_plot(h$all_ids, h$down, h$up, h$title)
        }, width = 600, height = 300)
      })
    })

    selected_ids <- reactive({
      n_comp <- length(data()$logFC_cols)
      picked <- vapply(seq_len(n_comp), function(i)
        isTRUE(input[[paste0("select_up_", i)]]) ||
        isTRUE(input[[paste0("select_down_", i)]]),
        logical(1))
      if (!any(picked)) return(character(0))

      result <- NULL
      for (i in which(picked)) {
        h        <- hits_for(i, applied$logfc, applied$pval)
        up_ids   <- if (isTRUE(input[[paste0("select_up_",   i)]])) h$up   else character(0)
        down_ids <- if (isTRUE(input[[paste0("select_down_", i)]])) h$down else character(0)
        pair_ids <- unique(c(up_ids, down_ids))
        result   <- if (is.null(result)) pair_ids else intersect(result, pair_ids)
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
