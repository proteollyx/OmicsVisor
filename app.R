# ─────────────────────────────────────────────────────────
# OmicsVisor - App
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

source("version.R")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(openxlsx)
  library(DT)
  library(data.table)
  library(dplyr)
  library(VennDiagram)
  library(ggrepel)
  library(pheatmap)
  library(stringr)
  library(shinyalert)
  library(ggbeeswarm)
  library(umap)
  library(UpSetR)
})

# Maximum file size
options(shiny.maxRequestSize = 443 * 1024^2)

# Source the module files
source("helper_functions.R")
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
source("donut_plot_module.R")
source("boxplot_module.R")
source("scatterplot_module.R")
source("one_d_enrichment_module.R")
source("gct_export_module.R")
source("upset_plot_module.R")
source("correlation_module.R")
source("about_module.R")


ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  title = tags$span(
    tags$img(src = "omics_icon3.png", height = "40px",
             style = "margin-right:12px; vertical-align:middle;"),
    tags$span("OmicsVisor",
              style = "font-weight:700; vertical-align:middle; letter-spacing:0.02em;"),
    tags$span(paste0("v", ov_version),
              style = paste0("font-size:0.78em; opacity:0.75; margin-left:8px;",
                             " vertical-align:middle; background:rgba(255,255,255,0.2);",
                             " border-radius:10px; padding:2px 8px;")),
    tags$span("Omics Data Visualisation Tool",
              style = "font-size:0.78em; opacity:0.65; margin-left:14px; vertical-align:middle;")
  ),

  tags$head(
    # Clipboard JS
    tags$script(HTML('
      Shiny.addCustomMessageHandler("copyToClipboard", function(content) {
        navigator.clipboard.writeText(content).then(function() {
          console.log("Text copied to clipboard");
        }).catch(function(error) {
          console.error("Could not copy text: ", error);
        });
      });
    ')),

    # Favicon
    tags$link(rel = "shortcut icon", href = "favicon_io/favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16",
              href = "favicon_io/favicon-16x16.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32",
              href = "favicon_io/favicon-32x32.png"),
    tags$link(rel = "apple-touch-icon", href = "favicon_io/apple-touch-icon.png"),
    tags$link(rel = "manifest", href = "favicon_io/site.webmanifest"),

    tags$style(HTML("

      /* ── 1DE tab: inactive ────────────────────────────────── */
      .nav-tabs .nav-link[data-value='1DE'] {
        background-color: #fdecec !important;
        color: #7a1d1d !important;
      }
      /* ── 1DE tab: active ──────────────────────────────────── */
      .nav-tabs .nav-link.active[data-value='1DE'] {
        background-color: #f8d7da !important;
        color: #7a1d1d !important;
        border-color: #f1b0b7 #f1b0b7 #fff !important;
      }

      /* ── Cards in sidebar: no overflow clipping ───────────── */
      .bslib-sidebar-layout > .sidebar > .sidebar-content {
        overflow-y: auto;
        overflow-x: visible;
      }

      /* ── Selectize: keep dropdown above everything ────────── */
      .selectize-dropdown { z-index: 9999 !important; }

      /* ── Single-select: truncate long selected value ─────── */
      /* Targets the inner .item div (actual text node), not    */
      /* the outer container — that's where ellipsis applies.   */
      .selectize-control.single .selectize-input {
        white-space: nowrap !important;
        overflow: hidden !important;
      }
      .selectize-control.single .selectize-input .item {
        max-width: calc(100% - 25px) !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
        white-space: nowrap !important;
        display: inline-block !important;
        vertical-align: middle !important;
      }
      .selectize-control {
        width: 100% !important;
        min-width: 0 !important;
      }

      /* ── Body-level dropdowns: sensible width bounds ─────── */
      body > .selectize-dropdown {
        min-width: 240px;
        max-width: min(560px, 90vw) !important;
      }

      /* ── Footer ───────────────────────────────────────────── */
      .ov-footer {
        border-top: 1px solid #dce3ea;
        margin-top: 24px;
        padding: 10px;
        font-size: 0.9em;
        color: #888;
        text-align: center;
      }

      /* ── Action buttons: prevent flex-stretch to full width ── */
      .action-button,
      .shiny-download-link {
        width: auto;
        align-self: flex-start;
      }

    "))
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 320,

    conditionalPanel(
      "input.tabs == null || input.tabs != '1DE'",

      card(
        card_header("Data input"),
        fileInput("upload_excel", "Upload file",
                  accept = c(".xlsx", ".xls", ".txt", ".tsv", ".csv")),
        selectizeInput(
          "int_regex_preset",
          "Intensity columns:",
          choices = c(
            "Imputed  (^Imputed)"    = "^Imputed",
            "Intensity (^Intensity)" = "^Intensity"
          ),
          selected = "^Imputed",
          options  = list(dropdownParent = "body")
        ),
        textInput("int_regex", "Custom regex:", value = "^Imputed"),
        helpText("Select a preset or type your own regex."),
        helpText(HTML(
          "<strong>Examples:</strong><br>",
          "<code>^Imputed.*(cond1|cond2)</code> &mdash; select specific conditions<br>",
          "<code>^Imputed.*(cond1|cond2).*24h</code> &mdash; add a timepoint filter<br><br>",
          "<em>Note:</em> A custom regex typed above overrides the dropdown selection."
        ))
      ),

      card(
        card_header("Swap logFC"),
        helpText(
          "Invert selected comparisons: logFC × −1 and rename x.over.y → y.over.x.",
          "t-statistic and nominal p-value columns are removed."
        ),
        uiOutput("swap_comparisons_ui"),
        downloadButton("download_swapped", "Download Processed Data",
                       class = "w-100 mt-2 btn-sm")
      )
    ),

    conditionalPanel(
      "input.tabs == '1DE'",
      div(
        style = "text-align:center; padding-top:8px;",
        tags$img(
          src   = "hedgehog_1DE.png",
          alt   = "1D Enrichment",
          style = "max-width:100%; height:auto; border-radius:8px;"
        ),
        tags$div("1D Enrichment",
                 style = "margin-top:6px; font-weight:600; color:#555;")
      )
    )
  ),

  # ── Main tabs ───────────────────────────────────────────────────────────────
  navset_card_tab(
    id = "tabs",

    nav_panel("Data Overview",      data_overview_ui("data_overview_module")),
    nav_panel("Volcano Plot",       volcano_plot_ui("volcano_module")),
    nav_panel("Donut Plot",         donut_plot_ui("donut_module")),
    nav_panel("UpSet Plot",         upset_plot_ui("upset_plot_module")),
    nav_panel("Heatmap",            heatmap_ui("heatmap_module")),
    nav_panel("PCA",                pca_ui("pca_module")),
    nav_panel("Volcano Printer",    volcano_printer_ui("volcano_printer_module")),
    nav_panel("logFC Scatter Plot", scatterplot_ui("scatterplot_module")),
    nav_panel("VennDi",             venndi_ui("venndi_module")),
    nav_panel("ID List Generator",  id_list_generator_ui("id_list_generator_module")),
    nav_panel("Boxplot",            plot_ui("boxplot_module")),
    nav_panel("Regex Tool",         regex_tool_ui("regex_tool_module")),
    nav_panel("Correlation",        correlation_ui("correlation_module")),
    nav_panel("GCT Export",         gct_export_ui("gct_export_module")),
    nav_panel("1D Enrichment",      value = "1DE", mod_pathway_1D_ui("1DE")),
    nav_panel("Documentation",      documentation_ui("documentation_module")),
    nav_panel("Disclaimer",         disclaimer_ui("disclaimer_module")),
    nav_panel("About",              about_ui("about_module"))
  ),

  # ── Footer ──────────────────────────────────────────────────────────────────
  tags$footer(
    class = "ov-footer",
    div(
      "Developed by Oliver Popp – ",
      tags$a(href = "mailto:oliver.popp@mdc-berlin.de", "oliver.popp@mdc-berlin.de")
    ),
    div(
      style = "margin-top: 4px; font-size: 0.85em; color: #aaa;",
      "Tested with Chrome. Some features may not work correctly in other browsers."
    )
  )
)


# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  observeEvent(TRUE, {
    shinyalert::shinyalert(
      title = "OmicsVisor – Important Notice",
      text  = paste(
        "OmicsVisor enables rapid exploration of differential analysis results and supports the generation of a wide range of figures.",
        "",
        "While the output may in principle be suitable for publication, users are kindly advised to consult the Proteomics Technology Platform for confirmation prior to submission.",
        sep = "\n"
      ),
      type = "info",
      showConfirmButton = TRUE,
      confirmButtonText = "I understand"
    )
  }, once = TRUE)

  # Sync preset dropdown → custom regex field
  observeEvent(input$int_regex_preset, {
    updateTextInput(session, "int_regex", value = input$int_regex_preset)
  }, ignoreInit = TRUE)

  # Raw file read — cached; does not depend on swap selection
  raw_file <- reactive({
    req(input$upload_excel)
    ext <- tolower(tools::file_ext(input$upload_excel$name))
    df <- switch(
      ext,
      "xlsx" = openxlsx::read.xlsx(input$upload_excel$datapath, sheet = 1),
      "xls"  = openxlsx::read.xlsx(input$upload_excel$datapath, sheet = 1),
      "txt"  = as.data.frame(data.table::fread(input$upload_excel$datapath, sep = "\t",   quote = "", na.strings = c("", "NA"))),
      "tsv"  = as.data.frame(data.table::fread(input$upload_excel$datapath, sep = "\t",   quote = "", na.strings = c("", "NA"))),
      "csv"  = as.data.frame(data.table::fread(input$upload_excel$datapath, sep = ",",    quote = "\"", na.strings = c("", "NA"))),
      {
        validate(need(FALSE, sprintf("Unsupported file type: .%s (expected .xlsx, .txt, .tsv, .csv)", ext)))
        return(NULL)
      }
    )
    if (is.list(df) && !is.data.frame(df)) df <- as.data.frame(do.call(cbind, df))
    df
  })

  # Comparisons available for the swap UI (detected from raw file, before any swap)
  comparisons_available <- reactive({
    df    <- raw_file()
    comps <- sub("^logFC_", "", grep("^logFC_", names(df), value = TRUE))
    sort(comps[grepl("\\.over\\.", comps)])
  })

  output$swap_comparisons_ui <- renderUI({
    comps <- comparisons_available()
    if (length(comps) == 0)
      return(helpText("No x.over.y comparisons detected in loaded data."))
    checkboxGroupInput("swap_selected", NULL, choices = comps)
  })

  # Main data reactive — applies swap then re-detects columns
  data <- reactive({
    df  <- raw_file()
    sel <- input$swap_selected
    if (length(sel) > 0) df <- swapFC(df, groups = sel)

    logFC_cols     <- grep("logFC", names(df), value = TRUE, ignore.case = TRUE)
    adjP_cols      <- grep("adj\\.?p|fdr|q\\.?val", names(df), value = TRUE, ignore.case = TRUE)
    intensity_cols <- grep(input$int_regex %||% "^Intensity", names(df), value = TRUE, ignore.case = TRUE)

    list(
      data           = df,
      logFC_cols     = logFC_cols,
      adjP_cols      = adjP_cols,
      intensity_cols = intensity_cols
    )
  })

  output$download_swapped <- downloadHandler(
    filename = function() paste0("processed_data_", Sys.Date(), ".txt"),
    content  = function(file) {
      write.table(data()$data, file, sep = "\t", quote = FALSE, row.names = FALSE)
    }
  )

  # Call modules
  data_overview_server("data_overview_module", data = data)
  volcano_plot_server("volcano_module", data = data)
  heatmap_server("heatmap_module", data = data)
  venndi_server("venndi_module")
  volcano_printer_server("volcano_printer_module", data = data)
  pca_server("pca_module", data = data)
  id_list_generator_server("id_list_generator_module", data = data)
  regex_tool_server("regex_tool_module")
  documentation_server("documentation_module")
  disclaimer_server("disclaimer_module")
  donut_plot_server("donut_module", data = data)
  plot_server("boxplot_module", data = data)
  scatterplot_server("scatterplot_module", data = data)
  gct_export_server("gct_export_module", data = data)
  mod_pathway_1D_server("1DE")
  upset_plot_server("upset_plot_module", data = data)
  correlation_server("correlation_module", data = data)
  about_server("about_module")
}

shinyApp(ui = ui, server = server)
