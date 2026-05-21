# ─────────────────────────────────────────────────────────
# OmicsVisor - Disclaimer Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
disclaimer_ui <- function(id) {
  ns <- NS(id)
  tagList(
      titlePanel("Disclaimer"),
      
      h3("Disclaimer"),
      
      p("This is a data exploration tool and still in its beta phase. The purpose of it is to empower users to explore the pre-analysed proteomics data provided by the proteomics platform (Oliver Popp)."),
      
      p("It does not perform data quality control, data filtering, imputation, or apply any statistics. This is all done beforehand."),
      
      p("There’s no guarantee that the output is absolutely correct, and the implemented functions (e.g., heatmap or PCA) may operate with different parameters, such as alternative normalisation and clustering methods, which are not included in this version."),
      
      p("Although you can generate PDFs of your volcano plots, we still advise you to double-check them with the proteomics platform before including them in any manuscripts, presentations, or other publicly shared documents."),
      
      p("You can use the tool to curate lists, which can then be used by Oliver to explore deeper at a later stage."),
      
      h4("Additional Disclaimer Points"),
      
      p("1. **No Guarantee of Accuracy**: OmicsVisor is provided 'as is,' without any express or implied warranties of accuracy or fitness for a specific purpose. The results generated should be validated independently before being used in a public or scientific setting."),
      
      p("2. **Limited Data Interpretation**: This tool is designed for data exploration only. It is not intended to replace detailed data analysis and should not be used as the sole basis for conclusions or decision-making."),
      
      p("3. **User Responsibility**: Users are responsible for understanding the limitations of the tool and for double-checking the results with professional analysts, especially if the output will be used in publications, presentations, or other formal documents."),
      
      p("4. **Tool Updates**: OmicsVisor is in beta phase and may be updated frequently. Users are advised to keep informed about updates or changes that could impact output and functionality."),
      
      p("5. **Usage Rights**: Users should ensure they have appropriate rights to any data they upload and should avoid uploading any sensitive, confidential, or personally identifiable information.")
  )
}

disclaimer_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
