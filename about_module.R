# ─────────────────────────────────────────────────────────
# OmicsVisor - About Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

# The changelog is read while the UI is being constructed, so an unguarded
# readLines() on a missing file takes the whole app down at startup rather than
# just blanking this one panel.
ov_read_changelog <- function() {
  path <- "CHANGELOG.md"
  if (!file.exists(path)) {
    alt <- file.path(dirname(sys.frame(1)$ofile %||% "."), "CHANGELOG.md")
    if (file.exists(alt)) path <- alt
  }
  if (!file.exists(path))
    return("Changelog not available in this deployment. See https://github.com/proteollyx/OmicsVisor/blob/main/CHANGELOG.md")
  tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
           error = function(e) "Changelog could not be read.")
}

about_ui <- function(id) {
  ns <- NS(id)
  tagList(
      h3("About OmicsVisor"),

      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4("Version"),
            tags$table(
              class = "table table-condensed",
              tags$tbody(
                tags$tr(
                  tags$td(tags$b("Version")),
                  tags$td(ov_version)
                ),
                tags$tr(
                  tags$td(tags$b("Release Date")),
                  tags$td(ov_release_date)
                )
              )
            )
          )
        ),
        column(
          width = 6,
          wellPanel(
            h4("Contact & Links"),
            tags$ul(
              tags$li(
                "Developer: Oliver Popp — ",
                tags$a(href = "mailto:oliver.popp@mdc-berlin.de",
                       "oliver.popp@mdc-berlin.de")
              ),
              tags$li(
                tags$a(href = "https://github.com/proteollyx/OmicsVisor",
                       "GitHub Repository", target = "_blank")
              ),
              # tags$li(
              #   "OmicsVisor Assistant (ChatGPT): ",
              #   tags$a(href = "https://chatgpt.com/g/g-W6cUieQY1-omicsvisor-assistant",
              #          "OmicsVisor Assistant", target = "_blank")
              # )
            )
          )
        )
      ),

      hr(),
      h4("Changelog"),
      tags$pre(
        style = "max-height:600px; overflow-y:auto; padding:10px;
                 border:1px solid #ddd; border-radius:4px;
                 white-space:pre-wrap; font-family:inherit; font-size:0.9em;",
        ov_read_changelog()
      )
  )
}

about_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
