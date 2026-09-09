# ─────────────────────────────────────────────────────────
# OmicsVisor - Disclaimer Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────
disclaimer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Disclaimer"),

    h4("No guarantee of correctness"),
    p(strong("There is no guarantee that OmicsVisor produces correct output."),
      " It is a research tool provided as is, without warranty of accuracy or of
       fitness for any particular purpose."),
    p("Treat every number, figure and ID list it produces as something to be
       checked, not as a result. If a finding matters, verify it against the
       upstream analysis that produced your input table before you rely on it,
       present it, or publish it."),
    p("This is not a formality. Bugs have been found in this tool before and will
       be again — the ",
      tags$a(href = "https://github.com/proteollyx/OmicsVisor/blob/main/CHANGELOG.md",
             target = "_blank", "changelog"),
      " records those that have been fixed."),

    h4("What OmicsVisor does, and does not do"),
    p("OmicsVisor is a tool for ", strong("exploring"), " the results of a
       differential omics analysis that has already been performed. It reads a
       finished results table and provides linked views over it."),
    p("It does ", strong("not"), " perform quality control, filtering,
       normalisation, imputation or statistical testing. All of that happens in
       the upstream analysis that produced your input table. If those steps were
       done differently, the views here will differ accordingly."),
    p("Where a view applies a method of its own — hierarchical clustering in
       the heatmap, scaling in PCA, rank-based enrichment in 1D Enrichment — it
       uses one particular parameter choice. Other reasonable choices exist and
       may give a different picture."),

    h4("Before you publish anything"),
    p("Figures are export-ready as vector PDFs, and they are intended to be
       usable. Even so, please have any figure checked by whoever performed your
       differential analysis before it goes into a manuscript, a presentation or
       any publicly shared document. For MDC users that is the Technology
       Platform Proteomics."),
    p("The same applies to ID lists exported from the tool: they reflect the
       cutoffs you set, and cutoffs are a scientific decision."),

    h4("How this software was built"),
    p("OmicsVisor was designed, developed and tested by Oliver Popp. ",
      strong("Parts of the code were written with the help of generative AI tools"),
      " — OpenAI ChatGPT during early development, and Anthropic Claude later,
       including for the automated test suite and a number of bug fixes."),
    p("Every change was reviewed, run and tested by the author, and the scientific
       design decisions are his own. The tool is covered by an automated test suite
       of around 500 checks that runs on every change; both the suite and the full
       history are public and can be inspected at ",
      tags$a(href = "https://github.com/proteollyx/OmicsVisor",
             target = "_blank", "github.com/proteollyx/OmicsVisor"), "."),
    p("This is stated so you can weigh it for yourself. It does not change the
       point above: check the output."),

    h4("Data you upload"),
    p("Uploaded files are processed on the server hosting this application.
       Please do not upload sensitive, confidential or personally identifiable
       information, and make sure you have the right to use whatever you
       upload."),
    p("OmicsVisor itself does not retain your uploaded file beyond your session,
       and does not share it with other users. The application offers no access
       control of its own over a session."),

    h4("In short"),
    tags$ol(
      tags$li(strong("No guarantee of correctness."), " See the section at the top
        of this page — this is the most important point here."),
      tags$li(strong("Exploration, not analysis."), " The tool is not a
        substitute for a considered analysis and should not be the sole basis
        for a conclusion."),
      tags$li(strong("Your responsibility."), " Understanding the tool's
        limitations, and checking its output before publication, rests with
        you."),
      tags$li(strong("It changes."), " OmicsVisor is under active development.
        Behaviour and defaults may change between versions; the version is shown
        in the header and every change is recorded in the changelog."),
      tags$li(strong("Your data, your rights."), " Ensure you are entitled to
        use whatever you upload.")
    ),

    h4("Source, licence and citation"),
    p("OmicsVisor is open source under the MIT licence. The source code,
       changelog and full documentation are at ",
      tags$a(href = "https://github.com/proteollyx/OmicsVisor",
             target = "_blank", "github.com/proteollyx/OmicsVisor"), "."),
    p("Releases are archived on Zenodo. If OmicsVisor contributed to your work,
       please cite ",
      tags$a(href = "https://doi.org/10.5281/zenodo.22660844",
             target = "_blank", "doi.org/10.5281/zenodo.22660844"), "."),
    p("The interface images are AI-generated and are excluded from the MIT
       licence; see ASSETS.md in the repository for details."),

    p(style = "color:#666; font-size:0.92em; margin-top:18px;",
      sprintf("OmicsVisor v%s · %s · Oliver Popp, Technology Platform Proteomics, Max Delbrück Center for Molecular Medicine (MDC), Berlin",
              ov_version, ov_release_date))
  )
}

disclaimer_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
