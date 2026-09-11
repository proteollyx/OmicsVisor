# ─────────────────────────────────────────────────────────
# OmicsVisor - About Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

# The changelog is read while the UI is being constructed, so an unguarded
# readLines() on a missing file takes the whole app down at startup rather than
# just blanking this one panel.
# Only the newest release section, not the whole file. The full changelog is
# 872 lines written for a developer and an auditor - it carries audit finding
# IDs and raw markdown - and embedding it made up a fifth of every page load
# while telling a bench scientist nothing they could act on. What a user needs
# is on this tab directly: the behaviour changes that alter results.
ov_changelog_latest <- function(path = "CHANGELOG.md") {
  if (!file.exists(path)) {
    alt <- file.path(dirname(sys.frame(1)$ofile %||% "."), "CHANGELOG.md")
    if (file.exists(alt)) path <- alt
  }
  if (!file.exists(path)) return(NULL)

  txt <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
  if (is.null(txt)) return(NULL)

  heads <- grep("^## \\[", txt)
  if (!length(heads)) return(NULL)
  start <- heads[1]
  end   <- if (length(heads) > 1) heads[2] - 1L else length(txt)
  out   <- txt[start:end]
  out   <- out[!grepl("^---\\s*$", out)]
  paste(trimws(out, "right"), collapse = "\n")
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
              )
            )
          )
        )
      ),

      hr(),
      h4("Changes that affect your results"),
      p(style = "color:#555;",
        "Most releases fix defects or add diagnostics and leave your numbers ",
        "untouched. These two do not \u2014 if you are comparing against an ",
        "analysis run before them, the difference is expected."),
      tags$ul(
        tags$li(tags$b("v1.4.0 \u2014 PCA no longer scales features by default. "),
                "Scaling weighted every feature equally regardless of its dynamic ",
                "range. Re-running a saved workflow gives a different PCA unless ",
                "\u201cScale data\u201d is ticked again. The active setting is printed ",
                "on the plot."),
        tags$li(tags$b("v1.2.0 \u2014 cutoffs are inclusive. "),
                "A feature sitting exactly on a threshold now counts as a hit ",
                "(|logFC| \u2265 cutoff, adj.P \u2264 cutoff), matching how the ",
                "cutoffs are described. Counts can differ by a few features.")
      ),

      hr(),
      h4("Latest release"),
      tags$pre(
        style = "max-height:320px; overflow-y:auto; padding:10px;
                 border:1px solid #ddd; border-radius:4px;
                 white-space:pre-wrap; font-family:inherit; font-size:0.88em;",
        ov_changelog_latest() %||% "See the full changelog on GitHub."
      ),
      p(tags$a(href = "https://github.com/proteollyx/OmicsVisor/blob/main/CHANGELOG.md",
               "Full changelog on GitHub", target = "_blank"),
        tags$span(style = "color:#777;",
                  " \u2014 every release, with the audit findings each one addresses."))
  )
}

about_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
