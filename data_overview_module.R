# ─────────────────────────────────────────────────────────
# OmicsVisor - Data Overview Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
data_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Data Overview Module"),
    p("Displays the loaded data frame in full. Users can scroll through columns, set the number of rows displayed, and search for specific strings."),
    p("Please be patient while loading of larger dataframes will take a bit."),
    p("Click rows to select them; hold Shift or Ctrl/Cmd to select multiple. Use the button below to copy their IDs."),
    fluidRow(
      column(3,
        actionButton(ns("copy_selected_ids"), "Copy selected IDs",
                     icon = icon("copy"), class = "btn-primary")
      ),
      column(9,
        verbatimTextOutput(ns("selection_preview"))
      )
    ),
    br(),
    DTOutput(ns("data_preview"))
  )
}

data_overview_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$data_preview <- renderDT({
      req(data())
      datatable(data()$data,
                selection = "multiple",
                options   = list(pageLength = 10, scrollX = TRUE))
    })

    selected_ids <- reactive({
      rows <- input$data_preview_rows_selected
      if (length(rows) == 0) return(character(0))
      df <- data()$data
      if ("id" %in% names(df)) as.character(df$id[rows]) else as.character(rows)
    })

    output$selection_preview <- renderText({
      ids <- selected_ids()
      if (length(ids) == 0) return("No rows selected.")
      paste0(length(ids), " selected: ", paste(ids, collapse = ", "))
    })

    observeEvent(input$copy_selected_ids, {
      ids <- selected_ids()
      if (length(ids) == 0) return()
      session$sendCustomMessage("copyToClipboard", paste(ids, collapse = ", "))
    })
  })
}
