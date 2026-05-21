# ─────────────────────────────────────────────────────────
# OmicsVisor - About Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

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
        paste(readLines("CHANGELOG.md", warn = FALSE), collapse = "\n")
      )
  )
}

about_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
