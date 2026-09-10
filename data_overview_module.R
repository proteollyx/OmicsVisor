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

    uiOutput(ns("upload_diagnosis")),

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

data_overview_server <- function(id, data, report = NULL) {
  moduleServer(id, function(input, output, session) {

    # What was read, and anything worth knowing about it. Reported rather than
    # enforced: only impossible values block the upload (see ov_inspect_upload).
    output$upload_diagnosis <- renderUI({
      if (is.null(report)) return(NULL)
      r <- try(report(), silent = TRUE)
      if (inherits(r, "try-error") || is.null(r)) return(NULL)

      row <- function(label, value, tone = "ok") {
        col <- switch(tone, ok = "#18682a", warn = "#8a4b00", muted = "#555")
        tags$tr(
          tags$td(style = "padding:2px 14px 2px 0; color:#555;", label),
          tags$td(style = sprintf("padding:2px 0; color:%s; font-weight:600;", col), value))
      }

      idr <- if (!r$id$present) row("Identifier", "no 'id' column", "warn")
             else row("Identifier",
                      sprintf("%s unique%s%s", format(r$id$n_unique, big.mark = ","),
                              if (r$id$n_duplicate > 0) sprintf(", %d duplicate", r$id$n_duplicate) else "",
                              if (r$id$n_missing   > 0) sprintf(", %d missing",   r$id$n_missing)   else ""),
                      if (r$id$n_duplicate > 0 || r$id$n_missing > 0) "warn" else "ok")

      intr <- if (r$intensity$n == 0)
                row("Intensity columns",
                    paste0("none matched '", r$intensity$regex, "'",
                           if (length(r$intensity$candidates))
                             paste0(" \u2014 try ", paste0("^", r$intensity$candidates, collapse = " or ")) else ""),
                    "warn")
              else row("Intensity columns",
                       sprintf("%d matched '%s', %.1f%% missing",
                               r$intensity$n, r$intensity$regex, r$intensity$pct_missing),
                       if (r$intensity$pct_missing > 30) "warn" else "ok")

      prange <- if (is.null(r$comparisons)) "n/a" else
        sprintf("%.3g \u2013 %.3g",
                suppressWarnings(min(r$comparisons$padj_min, na.rm = TRUE)),
                suppressWarnings(max(r$comparisons$padj_max, na.rm = TRUE)))

      tagList(
        div(
          style = paste("border:1px solid #dbe4ee; border-left:4px solid #1E3791;",
                        "background:#F3F7FB; border-radius:4px;",
                        "padding:10px 14px; margin:6px 0 14px 0;"),
          tags$div(style = "font-weight:700; color:#1E3791; margin-bottom:6px;",
                   "What was loaded"),
          tags$table(
            style = "font-size:0.92em; border-collapse:collapse;",
            row("Table", sprintf("%s rows \u00d7 %s columns",
                                 format(r$rows, big.mark = ","),
                                 format(r$cols, big.mark = ","))),
            idr,
            row("Comparisons", if (r$n_comparisons == 0) "none detected" else r$n_comparisons,
                if (r$n_comparisons == 0) "warn" else "ok"),
            row("Adjusted p range", prange, "muted"),
            intr,
            row("Fold-change scale", "assumed log2 \u2014 cannot be verified from the file", "muted"),
            row("Statistical method", "not supplied by the file", "muted")
          ),
          if (length(r$warnings))
            tags$ul(style = "margin:8px 0 0 0; padding-left:18px; color:#8a4b00; font-size:0.9em;",
                    lapply(r$warnings, tags$li))
        )
      )
    })

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
