# regex_tool_module.R

# UI function for Regex Tool
regex_tool_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      h3("Regex Tool Module"),
      p("The Regex Tool converts a list of comma-separated IDs to gene names for use in tools like Metascape. The module works independently of the loaded data frame and can make the output unique by removing redundant entries."),
      fluidRow(
        column(6,
               textAreaInput(ns("text_input"), "Input Text",
                             placeholder = "e.g., ACE_P12821, PARP1_P09874, TP53_P04637",
                             rows = 8, width = "100%")
        ),
        column(6,
               h4("Output Text"),
               verbatimTextOutput(ns("output_text")),
               actionButton(ns("copy_button"), "Copy Output", icon = icon("copy"))
        )
      ),
      fluidRow(
        column(4,
               textInput(ns("regex_pattern"), "Regex Pattern for gsub",
                         value = "_.*",  # Set default value for regex pattern
                         placeholder = "Enter regex pattern here...")
        ),
        column(4,
               checkboxInput(ns("unique_output"), "Make Unique", value = FALSE)
        )
      )
    )
  )
}

# Server function for Regex Tool
regex_tool_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to process the input text
  processed_text <- reactive({
    req(input$text_input, input$regex_pattern)
    
    # Split the input text by commas, apply gsub with the provided regex, and trim whitespace
    items <- strsplit(input$text_input, ",")[[1]]
    modified_items <- trimws(gsub(input$regex_pattern, "", items))
    
    # Make unique if the checkbox is checked
    if (input$unique_output) {
      modified_items <- unique(modified_items)
    }
    
    # Return the modified items as a line-break-separated string
    paste(modified_items, collapse = "\n")
  })
  
  # Display the processed text in the output text area
  output$output_text <- renderText({
    processed_text()
  })
  
  # Copy button functionality
  observeEvent(input$copy_button, {
    # Copy processed text to clipboard
    session$sendCustomMessage("copyToClipboard", processed_text())
  })
}
