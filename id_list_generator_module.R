# id_list_generator_module.R

id_list_generator_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("ID List Generator"),
      
      h3("ID List Generator Module"),
      p("This module allows users to generate ID lists from a gene list of interest. Paste your gene list (comma-separated or line-break-separated), select a column to search (e.g., Gene.names), and generate a list of IDs that match your criteria. The results can be copied to your clipboard."),
      p("Alternatively, you can upload your own gene set in .gmt format (e.g., from MSigDB) and browse through its predefined gene sets. Selected sets can be inserted directly into the input box for searching."),
      
      fluidRow(
        column(12,
               textAreaInput(ns("gene_list"), "Paste Gene List of Interest:", 
                             placeholder = "e.g., Gene1, Gene2, Gene3 (comma-separated or line-break-separated)", 
                             rows = 5, width = "100%")
        )
      ),
      fluidRow(
        column(6,
               selectInput(ns("search_column"), "Select Column to Search:", choices = NULL, width = "100%")
        ),
        column(6,
               checkboxInput(ns("remove_na"), "Remove NA values", value = FALSE)
        )
      ),
      fluidRow(
        column(12,
               actionButton(ns("generate_ids"), "Generate ID List")
        )
      ),
      hr(),
      fluidRow(
        column(12,
               h4("Matching IDs"),
               verbatimTextOutput(ns("id_output")),
               actionButton(ns("copy_ids"), "Copy IDs", icon = icon("copy"))
        )
      ),
      hr(),
      fluidRow(
        column(12,
               h4("Optional: Upload a .gmt file"),
               p("You can upload your own gene set file in .gmt format."),
               p(HTML('Download MSigDB gene sets from <a href="https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp" target="_blank">MSigDB Collections</a>')),
               fileInput(ns("gmt_upload"), "Upload .gmt File", accept = ".gmt"),
               
               selectizeInput(ns("predefined_sets"),
                              "Browse Gene Sets from Uploaded File",
                              choices = NULL,
                              multiple = TRUE,
                              options = list(placeholder = "Search gene sets...")),
               actionButton(ns("insert_geneset"), "Insert Selected Gene Set into Input")
        )
      ),
      hr(),
      verbatimTextOutput(ns("geneset_preview"))
    )
  )
}


id_list_generator_server <- function(input, output, session, data) {
  ns <- session$ns
  
  # Reactive to hold the uploaded gene sets
  uploaded_gene_sets <- reactiveVal(list())
  
  # Parse uploaded .gmt file
  observeEvent(input$gmt_upload, {
    req(input$gmt_upload)
    try({
      gene_sets <- read_gmt(input$gmt_upload$datapath)
      uploaded_gene_sets(gene_sets)
      updateSelectizeInput(session, "predefined_sets", choices = names(gene_sets), server = TRUE)
    }, silent = TRUE)
  })
  
  # Insert selected gene set(s) into the gene list input
  observeEvent(input$insert_geneset, {
    sets <- input$predefined_sets
    gene_sets <- uploaded_gene_sets()
    if (length(sets) == 0 || is.null(gene_sets)) return()
    
    genes <- unlist(gene_sets[sets])
    new_genes <- paste(unique(genes), collapse = ", ")
    updateTextAreaInput(session, "gene_list", value = new_genes)
  })
  
  # Other parts (search_column, matched_ids, output$id_output, etc.) remain unchanged
  observe({
    updateSelectInput(session, "search_column", choices = names(data()$data))
  })
  
  matched_ids <- reactive({
    req(input$gene_list, input$search_column)
    
    # Split, trim, and make unique
    gene_list <- unique(trimws(unlist(strsplit(input$gene_list, ",|\\n"))))
    
    selected_column <- data()$data[[input$search_column]]
    id_column <- data()$data$id
    indices <- find_genes(gene_list, selected_column)
    matching_ids <- id_column[unlist(indices)]
    
    if (input$remove_na) matching_ids <- na.omit(matching_ids)
    
    matching_ids
  })
  
  output$id_output <- renderText({
    ids <- matched_ids()
    if (length(ids) > 0) paste(ids, collapse = ", ") else "No matches found."
  })
  
  observeEvent(input$copy_ids, {
    ids_text <- paste(matched_ids(), collapse = ", ")
    session$sendCustomMessage("copyToClipboard", ids_text)
  })
}