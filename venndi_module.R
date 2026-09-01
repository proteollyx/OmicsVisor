# ─────────────────────────────────────────────────────────
# OmicsVisor - VennDi Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

# ── Centralised parsing helper ────────────────────────────────────────────────
# Converts any pasted string into a clean, unique, NA-free character vector.
# Splits simultaneously on: comma, semicolon, space, tab, \n, \r\n, or any
# combination thereof.  Double/mixed separators are collapsed by the + quantifier.
#
# Examples (all produce the same result  →  c("A","B","C")):
#   parse_input_list("A,B,C")           comma-separated
#   parse_input_list("A B C")           space-separated
#   parse_input_list("A\nB\nC")         newline-separated (Unix)
#   parse_input_list("A\r\nB\r\nC")     Windows newlines
#   parse_input_list("A\tB\tC")         tab-separated / pasted Excel column
#   parse_input_list("A;B;C")           semicolon-separated
#   parse_input_list("A, B\nC D")       mixed delimiters  →  c("A","B","C","D")
#   parse_input_list("A,,B  C")         double separators →  c("A","B","C")
#   parse_input_list("  A  ,  B  ")     padded whitespace →  c("A","B")
#   parse_input_list("")                empty string      →  character(0)
#   parse_input_list(NULL)              NULL              →  character(0)
#
# Note: spaces are treated as delimiters.  IDs that contain internal spaces
# should be separated by commas or semicolons to survive as single tokens —
# standard omics IDs (gene symbols, UniProt accessions) never contain spaces,
# so this is not a practical limitation.
parse_input_list <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  x <- paste(x, collapse = "\n")            # coerce vector input to one string
  if (!nzchar(trimws(x)))   return(character(0))
  tokens <- unlist(strsplit(x, "[,;[:space:]]+"))
  tokens <- trimws(tokens)                   # belt-and-suspenders
  unique(tokens[nzchar(tokens) & !is.na(tokens)])
}


# ── UI ────────────────────────────────────────────────────────────────────────
venndi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Venn Diagram / UpSet Module (VennDi)"),
    p("Functions independently of loaded data. Compare 2–5 ID lists.
      With 2 lists a Venn diagram is shown; with 3 or more, an UpSet plot
      is used and all intersection combinations can be extracted."),
    p(tags$b("Supported delimiters:"),
      " comma, semicolon, space, tab, or newline — any combination works.
       Paste directly from Excel columns, other tools, or type IDs separated
       by any of these delimiters."),
    fluidRow(
      column(4,
        numericInput(ns("n_lists"), "Number of lists:", value = 2, min = 2, max = 5, step = 1),
        uiOutput(ns("list_inputs_ui")),
        actionButton(ns("generate_lists"), "Generate Lists", class = "btn-primary btn-sm"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 2", ns("n_lists")),
          hr(),
          h4("Unique to List 1"),
          actionButton(ns("copy_u1"),  "Copy", icon = icon("copy"), class = "btn-sm"),
          verbatimTextOutput(ns("text_u1")),
          h4("Unique to List 2"),
          actionButton(ns("copy_u2"),  "Copy", icon = icon("copy"), class = "btn-sm"),
          verbatimTextOutput(ns("text_u2")),
          h4("Overlap"),
          actionButton(ns("copy_ovl"), "Copy", icon = icon("copy"), class = "btn-sm"),
          verbatimTextOutput(ns("text_ovl"))
        )
      ),
      column(8,
        plotOutput(ns("venn_plot"), height = "500px"),
        conditionalPanel(
          condition = sprintf("input['%s'] >= 3", ns("n_lists")),
          hr(),
          h4("Extract IDs by Intersection"),
          p("Select an intersection combination to view and copy its IDs."),
          selectizeInput(
            ns("selected_intersection"),
            label   = NULL,
            choices = character(0),
            width   = "100%",
            options = list(dropdownParent = "body")
          ),
          fluidRow(
            column(9, verbatimTextOutput(ns("intersection_ids_text"))),
            column(3,
              tags$br(),
              verbatimTextOutput(ns("intersection_n_text")),
              actionButton(ns("copy_intersection"), "Copy IDs",
                           icon = icon("copy"), class = "btn-sm w-100 mt-1")
            )
          )
        )
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
venndi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- reactiveValues(
      lists  = NULL,
      groups = NULL
    )

    # Internal helper: post-parse cleanup for vectors that are already split
    clean_ids <- function(x) {
      x <- trimws(x)
      unique(x[nzchar(x) & !is.na(x)])
    }

    # Compute mutually-exclusive intersection groups for 3+ lists.
    # Each ID receives one binary signature (membership across all lists);
    # IDs with the same signature are grouped together.
    compute_groups <- function(input_lists) {
      all_ids <- clean_ids(unique(unlist(input_lists)))
      if (length(all_ids) == 0L) return(list())
      nms <- names(input_lists)
      # vapply with an explicit template keeps this a matrix even when there is
      # only one ID; sapply() would collapse to a vector and the signatures
      # would then be computed per list instead of per ID.
      membership <- vapply(input_lists,
                           function(lst) all_ids %in% lst,
                           logical(length(all_ids)))
      membership <- matrix(membership, nrow = length(all_ids),
                           dimnames = list(NULL, nms))
      signatures <- apply(membership, 1L, function(x) paste(as.integer(x), collapse = ""))
      groups <- split(all_ids, signatures)
      groups <- groups[lengths(groups) > 0L]
      sig_to_label <- function(sig) {
        bits <- as.integer(strsplit(sig, "")[[1]])
        paste(nms[as.logical(bits)], collapse = " & ")
      }
      names(groups) <- vapply(names(groups), sig_to_label, character(1L))
      groups[order(-lengths(groups))]
    }

    # ── Clear state when the number of lists changes ───────────────────────────
    observeEvent(input$n_lists, {
      values$lists  <- NULL
      values$groups <- NULL
    }, ignoreInit = TRUE)

    # ── Dynamic list inputs ────────────────────────────────────────────────────
    # textAreaInput preserves multi-line pastes (e.g. Excel columns).
    # Existing values are recovered via isolate() so they survive n_lists changes.
    output$list_inputs_ui <- renderUI({
      n <- max(2L, min(5L, as.integer(input$n_lists %||% 2L)))
      tagList(lapply(seq_len(n), function(i) {
        current_val <- isolate(input[[paste0("list", i, "_input")]]) %||% ""
        textAreaInput(
          ns(paste0("list", i, "_input")),
          label       = paste0("List ", i, ":"),
          value       = current_val,
          rows        = 3L,
          placeholder = "Paste IDs — comma, semicolon, space, tab or newline",
          width       = "100%"
        )
      }))
    })

    # ── Generate Lists ─────────────────────────────────────────────────────────
    observeEvent(input$generate_lists, {
      n <- max(2L, min(5L, as.integer(input$n_lists %||% 2L)))

      lst <- lapply(seq_len(n), function(i) {
        parse_input_list(input[[paste0("list", i, "_input")]])
      })
      names(lst) <- paste0("List ", seq_len(n))

      # Warn about empty lists after parsing
      empty_idx <- which(lengths(lst) == 0L)
      if (length(empty_idx) > 0L) {
        showNotification(
          paste0(
            "Empty after parsing: ",
            paste(names(lst)[empty_idx], collapse = ", "),
            ". Please paste some IDs."
          ),
          type = "warning", duration = 6L
        )
      }

      values$lists  <- lst
      values$groups <- if (n >= 3L) compute_groups(lst) else NULL
    })

    # ── Populate selectize for 3+ lists ───────────────────────────────────────
    observe({
      req(!is.null(values$groups), length(values$lists) >= 3L)
      grps <- values$groups
      # Integer indices as values avoids special-character URL-encoding issues
      choices <- setNames(
        as.character(seq_along(grps)),
        paste0(names(grps), "  (n = ", lengths(grps), ")")
      )
      updateSelectizeInput(session, "selected_intersection", choices = choices)
    })

    # Clear selectize when returning to 2-list mode
    observe({
      req(!is.null(values$lists), length(values$lists) == 2L)
      updateSelectizeInput(session, "selected_intersection", choices = character(0))
    })

    # ── 2-list derived sets ────────────────────────────────────────────────────
    # setdiff / intersect operate on already-unique, already-clean vectors.
    # The Venn diagram areas are derived from the same vectors, so diagram
    # counts always match the text outputs.
    get_unique_1 <- reactive({
      req(!is.null(values$lists), length(values$lists) == 2L)
      setdiff(values$lists[[1]], values$lists[[2]])
    })
    get_unique_2 <- reactive({
      req(!is.null(values$lists), length(values$lists) == 2L)
      setdiff(values$lists[[2]], values$lists[[1]])
    })
    get_overlap <- reactive({
      req(!is.null(values$lists), length(values$lists) == 2L)
      intersect(values$lists[[1]], values$lists[[2]])
    })

    # ── 2-list text outputs ───────────────────────────────────────────────────
    output$text_u1 <- renderText({
      ids <- get_unique_1()
      if (length(ids) == 0L) "(none)" else paste(ids, collapse = ", ")
    })
    output$text_u2 <- renderText({
      ids <- get_unique_2()
      if (length(ids) == 0L) "(none)" else paste(ids, collapse = ", ")
    })
    output$text_ovl <- renderText({
      ids <- get_overlap()
      if (length(ids) == 0L) "(none)" else paste(ids, collapse = ", ")
    })

    # ── 2-list copy buttons ───────────────────────────────────────────────────
    observeEvent(input$copy_u1, {
      session$sendCustomMessage("copyToClipboard", paste(get_unique_1(), collapse = ", "))
    })
    observeEvent(input$copy_u2, {
      session$sendCustomMessage("copyToClipboard", paste(get_unique_2(), collapse = ", "))
    })
    observeEvent(input$copy_ovl, {
      session$sendCustomMessage("copyToClipboard", paste(get_overlap(), collapse = ", "))
    })

    # ── Plot ───────────────────────────────────────────────────────────────────
    # For the pairwise Venn, draw.pairwise.venn() expects TOTAL set sizes
    # (inclusive of the overlap), which is exactly length(listN).
    # The cross.area is derived from the same parsed vectors, so the geometry
    # is always numerically consistent with the text outputs.
    output$venn_plot <- renderPlot({
      req(!is.null(values$lists))
      n <- length(values$lists)

      if (n == 2L) {
        l1         <- values$lists[[1]]
        l2         <- values$lists[[2]]
        area1      <- length(l1)
        area2      <- length(l2)
        cross.area <- length(intersect(l1, l2))
        validate(need(area1 + area2 > 0L, "Both lists are empty — nothing to plot."))
        grid.newpage()
        draw.pairwise.venn(
          area1      = area1,
          area2      = area2,
          cross.area = cross.area,
          category   = c("List 1", "List 2"),
          fill       = c("skyblue", "pink"),
          lty        = "blank",
          cex        = 1.5,
          cat.cex    = 1.5,
          cat.pos    = c(-20, 20),
          cat.dist   = 0.05
        )
      } else {
        validate(need(requireNamespace("UpSetR", quietly = TRUE),
                      "Package 'UpSetR' is required."))
        all_ids <- unique(unlist(values$lists))
        validate(need(length(all_ids) > 0L, "All lists are empty — nothing to plot."))
        mat <- as.data.frame(
          lapply(values$lists, function(lst) as.integer(all_ids %in% lst)),
          stringsAsFactors = FALSE
        )
        set_sizes <- colSums(mat)
        validate(need(sum(set_sizes > 0L) >= 2L,
                      "Need at least 2 non-empty lists for an UpSet plot."))
        print(UpSetR::upset(mat, nsets = n, nintersects = 40L, order.by = "freq"))
      }
    }, width = 800, height = 500)

    # ── Intersection extractor (3+ lists) ─────────────────────────────────────
    get_selected_ids <- function() {
      req(!is.null(values$groups), length(values$lists) >= 3L)
      sel_str <- input$selected_intersection
      req(!is.null(sel_str), nzchar(sel_str))
      idx <- suppressWarnings(as.integer(sel_str))
      req(!is.na(idx), idx >= 1L, idx <= length(values$groups))
      values$groups[[idx]] %||% character(0)
    }

    output$intersection_ids_text <- renderText({
      ids <- get_selected_ids()
      if (length(ids) == 0L) "(none)" else paste(ids, collapse = ", ")
    })

    output$intersection_n_text <- renderText({
      paste("Count:", length(get_selected_ids()))
    })

    observeEvent(input$copy_intersection, {
      ids <- isolate(get_selected_ids())
      session$sendCustomMessage("copyToClipboard", paste(ids, collapse = ", "))
    })
  })
}
