# data_overview_module.R

# UI function for Data Overview Module
data_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("Data Overview Module"),
      p("Displays the loaded data frame in full. Users can scroll through columns, set the number of rows displayed, and search for specific strings."),
      p("Please be patient while loading of larger dataframes will take a bit."),
      DTOutput(ns("data_preview"))
    )
  )
}

# Server function for Data Overview Module
data_overview_server <- function(input, output, session, data) {
  ns <- session$ns
  
  output$data_preview <- renderDT({
    req(data())
    datatable(data()$data, options = list(pageLength = 10, scrollX = TRUE))
  })
}