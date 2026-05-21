# ─────────────────────────────────────────────────────────
# OmicsVisor - VennDi Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
venndi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Venn Diagram Module (VennDi)"),
    p("The Venn Diagram (VennDi) module functions independently of the loaded data. It compares two comma-separated lists of identifiers, highlighting unique and overlapping elements. Copy the result to your clipboard for further use."),
    fluidRow(
      column(4,
        textInput(ns("list1_input"), "Paste List 1 (comma-separated IDs):", ""),
        textInput(ns("list2_input"), "Paste List 2 (comma-separated IDs):", ""),
        actionButton(ns("generate_lists"), "Generate Lists", class = "btn-primary"),
        hr(),
        h4("Unique to List 1"),
        actionButton(ns("copy_unique_list1_btn"), "Copy", icon = icon("copy"), class = "btn-sm"),
        verbatimTextOutput(ns("unique_list1")),
        h4("Unique to List 2"),
        actionButton(ns("copy_unique_list2_btn"), "Copy", icon = icon("copy"), class = "btn-sm"),
        verbatimTextOutput(ns("unique_list2")),
        h4("Overlap"),
        actionButton(ns("copy_overlap_btn"), "Copy", icon = icon("copy"), class = "btn-sm"),
        verbatimTextOutput(ns("overlap"))
      ),
      column(8,
        plotOutput(ns("venn_plot"), height = "500px")
      )
    )
  )
}

venndi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- reactiveValues(
      unique_list1 = NULL,
      unique_list2 = NULL,
      overlap      = NULL
    )

    observeEvent(input$generate_lists, {
      list1 <- unique(trimws(unlist(strsplit(input$list1_input, ","))))
      list2 <- unique(trimws(unlist(strsplit(input$list2_input, ","))))
      list1 <- list1[nzchar(list1)]
      list2 <- list2[nzchar(list2)]

      values$unique_list1 <- setdiff(list1, list2)
      values$unique_list2 <- setdiff(list2, list1)
      values$overlap       <- intersect(list1, list2)
    })

    output$unique_list1 <- renderText({
      req(!is.null(values$unique_list1))
      if (length(values$unique_list1) == 0) "(none)" else paste(values$unique_list1, collapse = ", ")
    })
    output$unique_list2 <- renderText({
      req(!is.null(values$unique_list2))
      if (length(values$unique_list2) == 0) "(none)" else paste(values$unique_list2, collapse = ", ")
    })
    output$overlap <- renderText({
      req(!is.null(values$overlap))
      if (length(values$overlap) == 0) "(none)" else paste(values$overlap, collapse = ", ")
    })

    # Copy buttons — use the global clipboard handler (safe against special characters)
    observeEvent(input$copy_unique_list1_btn, {
      req(!is.null(values$unique_list1))
      session$sendCustomMessage("copyToClipboard", paste(values$unique_list1, collapse = ", "))
    })
    observeEvent(input$copy_unique_list2_btn, {
      req(!is.null(values$unique_list2))
      session$sendCustomMessage("copyToClipboard", paste(values$unique_list2, collapse = ", "))
    })
    observeEvent(input$copy_overlap_btn, {
      req(!is.null(values$overlap))
      session$sendCustomMessage("copyToClipboard", paste(values$overlap, collapse = ", "))
    })

    output$venn_plot <- renderPlot({
      # Render as soon as lists have been computed (empty subsets are valid)
      req(!is.null(values$unique_list1))

      area1      <- length(values$unique_list1) + length(values$overlap)
      area2      <- length(values$unique_list2) + length(values$overlap)
      cross.area <- length(values$overlap)

      validate(need(area1 + area2 > 0, "Both lists are empty — nothing to plot."))

      grid.newpage()
      draw.pairwise.venn(
        area1      = area1,
        area2      = area2,
        cross.area = cross.area,
        category   = c("List 1", "List 2"),
        fill       = c("skyblue", "pink"),
        lty        = "blank",
        cex        = 1.5,
        cat.cex    = 1.5,
        cat.pos    = c(-20, 20),
        cat.dist   = 0.05
      )
    })
  })
}
