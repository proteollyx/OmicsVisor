# venndi_module.R

# UI function for VennDi
venndi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("Venn Diagram Module (VennDi)"),
      p("The Venn Diagram (VennDi) module functions independently of the loaded data. It compares two comma-separated lists of identifiers, highlighting unique and overlapping elements. Copy the result to your clipboard for further use."),
      textInput(ns("list1_input"), "Paste List 1 (comma-separated IDs):", ""),
      textInput(ns("list2_input"), "Paste List 2 (comma-separated IDs):", ""),
      
      actionButton(ns("generate_lists"), "Generate Lists"),
      
      h4("Unique to List 1"),
      uiOutput(ns("copy_unique_list1_ui")),
      verbatimTextOutput(ns("unique_list1")),
      
      h4("Unique to List 2"),
      uiOutput(ns("copy_unique_list2_ui")),
      verbatimTextOutput(ns("unique_list2")),
      
      h4("Overlap"),
      uiOutput(ns("copy_overlap_ui")),
      verbatimTextOutput(ns("overlap")),
      
      plotOutput(ns("venn_plot"))
    )
  )
}

# Server function for VennDi
venndi_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive values to store lists
  values <- reactiveValues(
    unique_list1 = NULL,
    unique_list2 = NULL,
    overlap = NULL
  )
  
  # Generate unique and overlap lists when button is clicked
  observeEvent(input$generate_lists, {
    # Parse lists from inputs
    list1 <- unique(trimws(unlist(strsplit(input$list1_input, ","))))
    list2 <- unique(trimws(unlist(strsplit(input$list2_input, ","))))
    
    # Calculate unique and overlap items
    values$unique_list1 <- setdiff(list1, list2)
    values$unique_list2 <- setdiff(list2, list1)
    values$overlap <- intersect(list1, list2)
  })
  
  # Render text outputs with copy buttons
  output$unique_list1 <- renderText({
    paste(values$unique_list1, collapse = ", ")
  })
  output$copy_unique_list1_ui <- renderUI({
    tags$button(
      id = ns("copy_unique_list1_btn"),
      "Copy Unique List 1", icon("copy"),
      onclick = sprintf("navigator.clipboard.writeText('%s')", paste(values$unique_list1, collapse = ", "))
    )
  })
  
  output$unique_list2 <- renderText({
    paste(values$unique_list2, collapse = ", ")
  })
  output$copy_unique_list2_ui <- renderUI({
    tags$button(
      id = ns("copy_unique_list2_btn"),
      "Copy Unique List 2", icon("copy"),
      onclick = sprintf("navigator.clipboard.writeText('%s')", paste(values$unique_list2, collapse = ", "))
    )
  })
  
  output$overlap <- renderText({
    paste(values$overlap, collapse = ", ")
  })
  output$copy_overlap_ui <- renderUI({
    tags$button(
      id = ns("copy_overlap_btn"),
      "Copy Overlap", icon("copy"),
      onclick = sprintf("navigator.clipboard.writeText('%s')", paste(values$overlap, collapse = ", "))
    )
  })
  
  # Render Venn diagram
  output$venn_plot <- renderPlot({
    req(values$unique_list1, values$unique_list2, values$overlap)
    
    # Create the Venn diagram
    library(VennDiagram)
    grid.newpage()
    draw.pairwise.venn(
      area1 = length(values$unique_list1) + length(values$overlap),
      area2 = length(values$unique_list2) + length(values$overlap),
      cross.area = length(values$overlap),
      category = c("List 1", "List 2"),
      fill = c("skyblue", "pink"),
      lty = "blank",
      cex = 1.5,
      cat.cex = 1.5,
      cat.pos = c(-20, 20),
      cat.dist = 0.05
    )
  })
}
