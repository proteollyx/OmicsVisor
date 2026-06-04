# ─────────────────────────────────────────────────────────
# OmicsVisor - Box / Violin / Crossbar Plot Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
#  UI ---------------------------------------------------------
plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Box / Violin / Crossbar Plot Module"),
    p(
      "Generate plots for individual protein IDs. ",
      "The x-axis represents groupings extracted from intensity column names, ",
      "and the y-axis shows intensity values. ",
      strong("When should you use each plot?"),
      tags$ul(
        tags$li("",
                strong("Crossbar"), ": best when each group has only ",
                code("≤ 4"), " replicates. It highlights the mean and its error (",
                em("mean ± SD"), " or ", em("mean ± SEM"), "); distributional shape is less reliable here."
        ),
        tags$li("",
                strong("Boxplot / Violin"), ": preferred when each group has ",
                code("≥ 4"), " replicates, because they visualise the median, quartiles (boxplot) ",
                "or full density shape (violin) in addition to outliers."
        )
      )
    ),

    textInput(
      ns("id_selection"),
      label = "Enter Protein ID:",
      placeholder = "e.g., PARP1_P09874"
    ),
    helpText("Enter only one protein ID here."),

    selectInput(
      ns("intensity_columns"),
      "Select Intensity Columns:",
      choices  = NULL,
      multiple = TRUE
    ),

    fluidRow(
      column(2, actionButton(ns("select_all_intensity"),   "Select All",   class = "btn-sm")),
      column(2, actionButton(ns("deselect_all_intensity"), "Deselect All", class = "btn-sm"))
    ),

    h4("Select Components for Grouping Annotation"),
    uiOutput(ns("grouping_checkboxes_ui")),

    h4("Plot Type"),
    radioButtons(
      ns("plot_type"),
      label   = NULL,
      choices = c("Boxplot"   = "box",
                  "Violin"    = "violin",
                  "Crossbar"  = "cross"),
      inline  = TRUE
    ),

    conditionalPanel(
      sprintf("input['%s'] == 'cross'", ns("plot_type")),
      radioButtons(
        ns("error_type"),
        label   = "Crossbar shows:",
        choices = c("Mean ± SD"  = "sd",
                    "Mean ± SEM" = "sem"),
        inline  = TRUE
      )
    ),

    checkboxInput(ns("show_jitter"),   "Add jittered points",  value = FALSE),
    checkboxInput(ns("show_beeswarm"), "Add beeswarm points",  value = FALSE),

    h4("Download Options"),
    numericInput(ns("pdf_width"),  "Width (inches)",  value = 8, min = 4),
    numericInput(ns("pdf_height"), "Height (inches)", value = 6, min = 4),

    downloadButton(ns("download_plot"), "Download Plot"),
    br(),
    plotOutput(ns("protein_plot"), height = "600px")
  )
}

#  SERVER -----------------------------------------------------
plot_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      updateSelectInput(session, "intensity_columns",
                        choices = data()$intensity_cols)
    })

    observeEvent(input$select_all_intensity, {
      updateSelectInput(session, "intensity_columns",
                        selected = data()$intensity_cols)
    })
    observeEvent(input$deselect_all_intensity, {
      updateSelectInput(session, "intensity_columns",
                        selected = character(0))
    })

    observeEvent(input$show_jitter, {
      if (isTRUE(input$show_jitter))
        updateCheckboxInput(session, "show_beeswarm", value = FALSE)
    })
    observeEvent(input$show_beeswarm, {
      if (isTRUE(input$show_beeswarm))
        updateCheckboxInput(session, "show_jitter",  value = FALSE)
    })

    split_components <- reactive({
      req(input$intensity_columns)
      str_split_fixed(input$intensity_columns, "_|\\.", n = 5)
    })

    output$grouping_checkboxes_ui <- renderUI({
      comps <- split_components()
      tagList(lapply(seq_len(ncol(comps)), function(i)
        checkboxInput(ns(paste0("group_component_", i)),
                      label  = paste("Use Component", i),
                      value  = FALSE)
      ))
    })

    selected_ids <- reactive({
      req(input$id_selection)
      ids <- unlist(strsplit(input$id_selection, "[,;[:space:]]+"))
      ids <- ids[nzchar(ids)]
      if (length(ids) == 0) {
        showNotification("Please enter one protein ID.", type = "error")
        return(NULL)
      }
      if (length(ids) > 1) {
        showNotification("Only one protein ID is allowed per plot.", type = "error")
        return(NULL)
      }
      ids
    })

    group_annotations <- reactive({
      comps <- split_components()
      groups <- sapply(seq_len(ncol(comps)), function(i) {
        if (isTRUE(input[[paste0("group_component_", i)]]))
          comps[, i]
        else
          NULL
      })
      if (all(sapply(groups, is.null))) {
        NULL
      } else {
        apply(do.call(cbind, groups[!sapply(groups, is.null)]), 1,
              paste, collapse = "_")
      }
    })

    selected_data <- reactive({
      req(selected_ids(), input$intensity_columns)
      df <- data()$data
      df <- df[df$id %in% selected_ids(), c("id", input$intensity_columns), drop = FALSE]
      validate(need(nrow(df) > 0, "None of the requested IDs were found."))
      df_long <- tidyr::pivot_longer(
        df,
        cols      = -id,
        names_to  = "Sample",
        values_to = "Intensity"
      )
      ann <- group_annotations()
      df_long$Group <- if (is.null(ann)) df_long$Sample else ann
      df_long
    })

    make_plot <- reactive({
      req(selected_data(), input$plot_type)
      df_long <- selected_data()

      title_main  <- paste("Plot for", unique(df_long$id))
      subtitle_cb <- if (input$plot_type == "cross") {
        if (input$error_type == "sd") "Crossbars show mean ± SD"
        else                          "Crossbars show mean ± SEM"
      } else NULL

      base <- ggplot(df_long, aes(x = Group, y = Intensity, colour = Group, fill = Group)) +
        theme_minimal() +
        scale_colour_manual(values = distcols3) +
        scale_fill_manual(values = distcols3) +
        labs(x = "Grouping", y = "Intensity", title = title_main,
             subtitle = subtitle_cb) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (input$plot_type == "box") {
        base <- base + geom_boxplot(outlier.shape = NA, col = "black")
      } else if (input$plot_type == "violin") {
        base <- base + geom_violin(col = "black")
      } else {
        se_fun <- function(x) { x <- x[!is.na(x)]; sd(x) / sqrt(length(x)) }
        base <- base +
          stat_summary(fun = mean,
                       geom = "crossbar",
                       width = 0.5,
                       fatten = 1,
                       fun.min = if (input$error_type == "sd")
                         function(x) mean(x) - sd(x)
                       else
                         function(x) mean(x) - se_fun(x),
                       fun.max = if (input$error_type == "sd")
                         function(x) mean(x) + sd(x)
                       else
                         function(x) mean(x) + se_fun(x),
                       colour = "black", alpha = 0.4,
                       aes(fill = Group)) +
          scale_fill_manual(values = distcols3, guide = "none")
      }

      if (isTRUE(input$show_jitter))
        base <- base + geom_jitter(width = 0.2, alpha = 0.73, size = 3, shape = 21, col = "black")
      if (isTRUE(input$show_beeswarm))
        base <- base + ggbeeswarm::geom_beeswarm(alpha = 0.73, size = 3, shape = 21, col = "black")

      base
    })

    output$protein_plot <- renderPlot({
      make_plot()
    }, width = 800, height = 600)

    output$download_plot <- downloadHandler(
      filename = function() paste0("protein_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"),
      content = function(file) {
        ggsave(filename = file, plot = make_plot(), device = "pdf",
               width = input$pdf_width, height = input$pdf_height)
      }
    )
  })
}
