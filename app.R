# OmicsVisor
# app.R

version <- "0.8.0.7-beta"

# load required packages
# Install pacman if it's not already installed
# if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Load libraries using pacman
# pacman::p_load(
#   shiny,
#   plotly,
#   openxlsx,
#   DT,
#   data.table,
#   dplyr,
#   VennDiagram,
#   ggrepel,
#   shinythemes,
#   pheatmap,
#   stringr,
#   shinyalert,
#   ggbeeswarm,
#   # reshape2,
#   rsconnect,
#   umap,
#   UpSetR
# )

library(shiny)
library(plotly)
library(openxlsx)
library(DT)
library(data.table)
library(dplyr)
library(VennDiagram)
library(ggrepel)
library(shinythemes)
library(pheatmap)
library(stringr)
library(shinyalert)
library(ggbeeswarm)
library(rsconnect)
library(umap)
library(UpSetR)

# run this when installing new packages:
# renv::settings$bioconductor(FALSE)
# renv::install(c(
#   "shiny", "plotly", "openxlsx", "DT", "data.table", "dplyr",
#   "VennDiagram", "ggrepel", "shinythemes", "pheatmap", "stringr",
#   "shinyalert", "ggbeeswarm", "rsconnect", "umap", "UpSetR"
# ))
# options(repos = c(CRAN = "https://cloud.r-project.org"))
# renv::activate()
# renv::status()
# renv::snapshot()

# •	✅ Records all current package versions (including shinyalert)
# •	✅ Saves it to renv.lock
# •	✅ Ensures future deployments (e.g. to Posit Connect) install the right packages

# Maximum file size
options(shiny.maxRequestSize = 443 * 1024^2) # Set to 443 MB or adjust as needed


# Source the module files
source("helper_functions.R") # add-on functions
source("data_overview_module.R")
source("volcano_plot_module.R")
source("heatmap_module.R")
source("venndi_module.R")
source("volcano_printer_module.R")
source("pca_module.R")
source("id_list_generator_module.R")
source("regex_tool_module.R")
source("documentation_module.R")
source("disclaimer_module.R")
# source("version_control_module.R") # to be implemented?
source("donut_plot_module.R")
source("boxplot_module.R")
source("scatterplot_module.R")
# source("predefined_gene_sets.R")
source("one_d_enrichment_module.R")   # add this with your other source() lines
source("gct_export_module.R")
source("upset_plot_module.R")


# Define the UI for the app
ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  title = "OmicsVisor: Data Visualisation Tool",  # Title for the browser tab
  titlePanel(
    div(
      tags$img(src = "omics_icon3.png", height = "100px", style = "margin-right: 10px;"),
      paste0("OmicsVisor: Omics Data Visualisation Tool (Version ", version)
    )
  ),
  
  tags$head(
    
    # add colour to certain tabs - here: 1D enrich
    tags$style(HTML("
    /* Inactive 1D Enrichment tab */
    #tabs li a[data-value='1DE'] {
      background-color: #fdecec !important;
      color: #7a1d1d !important;
    }

    /* Active 1D Enrichment tab */
    #tabs li.active a[data-value='1DE'] {
      background-color: #f8d7da !important;
      color: #7a1d1d !important;
      border-color: #f1b0b7 !important;
    }
  ")),
    
    # JavaScript for copying to clipboard
    tags$script(HTML('
    Shiny.addCustomMessageHandler("copyToClipboard", function(content) {
      navigator.clipboard.writeText(content).then(function() {
        console.log("Text copied to clipboard");
      }).catch(function(error) {
        console.error("Could not copy text: ", error);
      });
    });
  ')),
    
    # Favicon setup with subfolder path
    tags$link(rel = "shortcut icon", href = "favicon_io/favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon_io/favicon-16x16.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon_io/favicon-32x32.png"),
    tags$link(rel = "apple-touch-icon", href = "favicon_io/apple-touch-icon.png"),
    tags$link(rel = "manifest", href = "favicon_io/site.webmanifest")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      # Show your usual controls everywhere EXCEPT the 1DE tab
      conditionalPanel(
        "input.tabs == null || input.tabs != '1DE'",
        fileInput("upload_excel", "Upload Excel"),
        textInput("int_regex", "Intensity column regex", value = "^Imputed.")
      ),
      
      # On the 1DE tab, show the hedgehog image instead
      conditionalPanel(
        "input.tabs == '1DE'",
        div(
          style = "text-align:center; padding-top: 8px;",
          tags$img(
            src = "hedgehog_1DE.png",
            alt = "1D Enrichment",
            style = "max-width:100%; height:auto; border-radius:8px;"
          ),
          tags$div(
            "1D Enrichment",
            style = "margin-top:6px; font-weight:600; color:#555;"
          )
        )
      )
    ),
    
    mainPanel(
      # # First row of tabs
      # tabsetPanel(
      #   id = "tabs_top",
      #   tabPanel("Data Overview", data_overview_ui("data_overview_module")),
      #   tabPanel("Volcano Plot",   volcano_plot_ui("volcano_module")),
      #   tabPanel("Donut Plot",     donut_plot_ui("donut_module")),
      #   tabPanel("Heatmap",        heatmap_ui("heatmap_module")),
      #   tabPanel("PCA",            pca_ui("pca_module")),
      #   tabPanel("Boxplot",        plot_ui("boxplot_module")),
      #   tabPanel("Volcano Printer", volcano_printer_ui("volcano_printer_module")),
      #   tabPanel("logFC Scatter Plot", scatterplot_ui("scatterplot_module"))
      # ),
      # br(),
      # # Second row of tabs
      # tabsetPanel(
      #   id = "tabs_bottom",
      #   tabPanel("VennDi",             venndi_ui("venndi_module")),
      #   tabPanel("ID List Generator",  id_list_generator_ui("id_list_generator_module")),
      #   tabPanel("Regex Tool",         regex_tool_ui("regex_tool_module")),
      #   tabPanel("1D Enrichment", value = "1DE", mod_pathway_1D_ui("1DE")),
      #   tabPanel("Documentation",      documentation_ui("documentation_module")),
      #   tabPanel("Disclaimer",         disclaimer_ui("disclaimer_module"))
      # )
      tabsetPanel(
        id = "tabs",   # <-- new
        
        tabPanel("Data Overview", data_overview_ui("data_overview_module")),
        
        tabPanel("Volcano Plot",
                 volcano_plot_ui("volcano_module")),
        
        tabPanel("Donut Plot",
                 donut_plot_ui("donut_module")),
        
        tabPanel("UpSet Plot", upset_plot_ui("upset_plot_module")),
        
        tabPanel("Heatmap",
                 heatmap_ui("heatmap_module")),
        
        tabPanel("PCA", pca_ui("pca_module")),
        
        tabPanel("Volcano Printer", volcano_printer_ui("volcano_printer_module")),
        
        tabPanel("logFC Scatter Plot", scatterplot_ui("scatterplot_module")),
        
        tabPanel("VennDi", venndi_ui("venndi_module")),
        
        tabPanel("ID List Generator", id_list_generator_ui("id_list_generator_module")),
        
        tabPanel("Boxplot", plot_ui("boxplot_module")),
        
        tabPanel("Regex Tool", regex_tool_ui("regex_tool_module")),
        
        tabPanel("GCT Export", gct_export_ui("gct_export_module")),
        
        tabPanel("1D Enrichment", value = "1DE", mod_pathway_1D_ui("1DE")),
        
        tabPanel("Documentation", documentation_ui("documentation_module")),
        
        tabPanel("Disclaimer", disclaimer_ui("disclaimer_module"))
        
        # tabPanel("Version Control", version_control_ui("version_control_module"))
      )
    )
  ),
  
  # Footer
  tags$footer(
    div(
      "Developed by Oliver Popp - ", 
      tags$a(href = "mailto:oliver.popp@mdc-berlin.de", "oliver.popp@mdc-berlin.de"),
      style = "margin-bottom: 5px;"
    ),
    div(
      h4("ChatGPT-guided Helper Tool:"),
      p("Click here if you need further assistance: ",
        tags$a(href = "https://chatgpt.com/g/g-W6cUieQY1-omicsvisor-assistant", "OmicsVisor Assistant")
      ),
      style = "margin-top: 5px;"
    ),
    align = "center",
    style = "padding: 10px; font-size: 0.9em; color: #888;"
  )
  
)

# Define the server logic for the app
server <- function(input, output, session) {
  
  observeEvent(TRUE, {
    shinyalert::shinyalert(
      title = "OmicsVisor – Important Notice",
      text  = paste(
        "OmicsVisor enables rapid exploration of differential analysis results and supports the generation of a wide range of figures.",
        "",
        "While the output may in principle be suitable for publication, users are kindly advised to consult the Proteomics Technology Platform for confirmation prior to submission.",
        "",
        "As OmicsVisor is currently a beta tool, occasional bugs or unexpected behaviour may still occur.",
        sep = "\n"
      ),
      type = "warning",
      showConfirmButton = TRUE,
      confirmButtonText = "I understand"
    )
  }, once = TRUE)
  
  data <- reactive({
    req(input$upload_excel)
    
    # safer: use the original filename to detect type
    ext <- tolower(tools::file_ext(input$upload_excel$name))
    
    raw_data <- switch(
      ext,
      "xlsx" = openxlsx::read.xlsx(input$upload_excel$datapath, sheet = 1),
      "xls"  = openxlsx::read.xlsx(input$upload_excel$datapath, sheet = 1),
      "txt"  = as.data.frame(data.table::fread(input$upload_excel$datapath, sep = "\t", quote = "", na.strings = c("", "NA"))),
      "tsv"  = as.data.frame(data.table::fread(input$upload_excel$datapath, sep = "\t", quote = "", na.strings = c("", "NA"))),
      "csv"  = as.data.frame(data.table::fread(input$upload_excel$datapath, sep = ",", quote = "\"", na.strings = c("", "NA"))),
      {
        validate(need(FALSE, sprintf("Unsupported file type: .%s (expected .xlsx, .txt, .tsv, .csv)", ext)))
        return(NULL)
      }
    )
    
    # ensure data.frame
    if (is.list(raw_data) && !is.data.frame(raw_data)) {
      raw_data <- as.data.frame(do.call(cbind, raw_data))
    }
    
    # Identify columns
    logFC_cols     <- grep("logFC", names(raw_data), value = TRUE, ignore.case = TRUE)
    adjP_cols      <- grep("adj\\.?p|fdr|q\\.?val", names(raw_data), value = TRUE, ignore.case = TRUE)
    intensity_cols <- grep(input$int_regex %||% "^Intensity", names(raw_data), value = TRUE, ignore.case = TRUE)
    
    list(
      data          = raw_data,
      logFC_cols    = logFC_cols,
      adjP_cols     = adjP_cols,
      intensity_cols= intensity_cols
    )
  })
  
  # Call modules
  callModule(data_overview_server, "data_overview_module", data = data)
  callModule(volcano_plot_server, "volcano_module", data)
  callModule(heatmap_server, "heatmap_module", data)
  callModule(venndi_server, "venndi_module")
  callModule(volcano_printer_server, "volcano_printer_module", data = data)
  callModule(pca_server, "pca_module", data = data)
  callModule(id_list_generator_server, "id_list_generator_module", data = data)
  callModule(regex_tool_server, "regex_tool_module")
  callModule(documentation_server, "documentation_module")
  callModule(disclaimer_server, "disclaimer_module")
  callModule(donut_plot_server, "donut_module", data = data)
  callModule(plot_server,  "boxplot_module", data = data)
  callModule(scatterplot_server, "scatterplot_module", data = data)
  gct_export_server("gct_export_module", data = data)
  mod_pathway_1D_server("1DE")
  callModule(upset_plot_server, "upset_plot_module", data = data)
}

# options(
#   shiny.fullstacktrace = TRUE,
#   shiny.trace = TRUE
# )

# Run the application
shinyApp(ui = ui, server = server)
