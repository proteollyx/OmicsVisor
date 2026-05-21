# ─────────────────────────────────────────────────────────
# OmicsVisor - 1D Enrichment Module
# Author: Oliver Popp
# ─────────────────────────────────────────────────────────

# =========================
# =======  UI  ============ 
# =========================
mod_pathway_1D_ui <- function(id, title = "1D Enrichment") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        h3(title),
        fileInput(ns("gmt"), "Upload GMT file", accept = ".gmt"),
        fileInput(ns("gct"), "Upload GCT file (v1.2/1.3)", accept = ".gct"),
        uiOutput(ns("col_picker")),
        hr(),
        numericInput(ns("minsz"), "Min set size", value = 5, min = 1, step = 1),
        numericInput(ns("maxsz"), "Max set size (Inf = large)", value = 1e9, min = 1, step = 1),
        selectInput(ns("alt"), "Wilcoxon alternative", choices = c("two.sided","greater","less"), selected = "two.sided"),
        selectInput(ns("adj"), "Multiple testing correction", choices = p.adjust.methods, selected = "BH"),
        hr(),
        numericInput(ns("fdr"), "FDR cutoff", value = 0.05, min = 0, max = 1, step = 0.001),
        numericInput(ns("topn"), "Top N (0 = all)", value = 20, min = 0, step = 5),
        checkboxInput(ns("apply_fdr_to_topsets"), "Apply FDR filter to Top sets plot", value = TRUE),
        selectInput(ns("order"), "Order by", choices = c("|effect| then sign"="abs","effect (signed)"="signed"), selected = "abs"),
        hr(),
        h4("Plot style"),
        selectInput(ns("colormode"), "Color mode", choices = c("None (single)"="none","Direction (neg/pos)"="direction","Effect (gradient)"="effect"), selected = "effect"),
        selectInput(ns("palette"), "Palette (direction/gradient)", choices = c("Blue–Red","Blue–Orange","Purple–Green","Teal–Magenta","Greys","Viridis-like"), selected = "Blue–Red"),
        selectInput(ns("singlecol"), "Single color (if mode = None)", choices = c("Slate","Teal","Blue","Purple","Crimson","Orange","Olive"), selected = "Slate"),
        sliderInput(ns("bub_range"), "Bubble size range", min = 1, max = 20,
                    value = c(2, 10), step = 0.5),
        numericInput(ns("alpha"), "Point alpha (0–1)", value = 0.9, min = 0.1, max = 1, step = 0.05),
        numericInput(ns("base_size"), "Base font size", value = 12, min = 6, step = 1),
        numericInput(ns("x_text"), "X-axis text size", value = 12, min = 6, step = 1),
        numericInput(ns("y_text"), "Y-axis text size", value = 12, min = 6, step = 1),
        numericInput(ns("legend_title"), "Legend title size", value = 11, min = 6, step = 1),
        numericInput(ns("legend_text"), "Legend text size", value = 10, min = 6, step = 1),
        numericInput(ns("title_size"), "Title size", value = 14, min = 8, step = 1),
        hr(),
        downloadButton(ns("dl_results"), "Download results (TSV)"),
        downloadButton(ns("dl_topsets"), "Download Top sets (PDF)"),
        downloadButton(ns("dl_bubble"), "Download Bubble (PDF)")
      ),
      column(
        width = 9,
        tabsetPanel(
          tabPanel(
            "Results table",
            DT::dataTableOutput(ns("tab"))
          ),
          tabPanel(
            "Top sets plot",
            # --- explanation block ---
            div(
              class = "well",
              style = "margin-top:10px; margin-bottom:16px; padding:10px;",
              tags$b("Top Sets vs Bubble — what’s the real difference?"),
              tags$ul(
                tags$li(tags$b("Top Sets plot:")),
                tags$ul(
                  tags$li("Shows top N sets based on your ", tags$i("Order by"), " setting."),
                  tags$li("Point size = ", tags$i("\u2212log10(FDR)"), " (significance)."),
                  tags$li("FDR filter is optional (toggle: ", tags$i("Apply FDR to Top sets plot"), ")."),
                  tags$li("Purpose: a compact “best hits” view (rank-style).")
                )
              )
            ),
            plotOutput(ns("plt"), height = "720px")
          ),
          tabPanel(
            "Bubble plot",
            # --- explanation block ---
            div(
              class = "well",
              style = "margin-top:10px; margin-bottom:16px; padding:10px;",
              tags$b("Top Sets vs Bubble — what’s the real difference?"),
              tags$ul(
                tags$li(tags$b("Bubble plot:")),
                tags$ul(
                  tags$li("Always applies the FDR cutoff (only significant sets shown)."),
                  tags$li("Point size = overlap % (", tags$i("size_overlap / size_total"), "), i.e., how much of the set participates."),
                  tags$li("Purpose: emphasis on magnitude + coverage among significant sets.")
                )
              )
            ),
            plotOutput(ns("bubble"), height = "720px")
          )
        )
      )
    )
  )
}

# =========================
# ===== Server ============ 
# =========================
mod_pathway_1D_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Utilities (local, no auto-install) ----
    .ensure_pkg <- function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Package '%s' is required but not installed. Please install.packages('%s')", pkg, pkg), call. = FALSE)
      }
    }
    
    # ---- I/O helpers ----
    read_gmt_1D <- function(path) {
      con <- file(path, open = "r")
      on.exit(close(con), add = TRUE)
      res <- list()
      i <- 0L
      while (TRUE) {
        line <- readLines(con, n = 1L, warn = FALSE)
        if (length(line) == 0L) break
        if (nzchar(line)) {
          parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
          if (length(parts) >= 3L) {
            set_name <- parts[1]
            set_desc <- parts[2]
            members <- unique(parts[-(1:2)])
            i <- i + 1L
            res[[i]] <- list(name = set_name, desc = set_desc, members = members)
          }
        }
      }
      sets <- lapply(res, `[[`, "members")
      names(sets) <- vapply(res, `[[`, character(1), "name")
      attr(sets, "meta") <- data.frame(
        name = vapply(res, `[[`, character(1), "name"),
        desc = vapply(res, `[[`, character(1), "desc"),
        size = vapply(sets, length, integer(1)),
        stringsAsFactors = FALSE
      )
      sets
    }
    
    read_gct <- function(path) { # should theoretically support #1.3 - to be tested properly
      con <- file(path, open = "r")
      on.exit(close(con), add = TRUE)
      header <- readLines(con, n = 1L, warn = FALSE)
      if (!length(header)) stop("Empty GCT file.")
      if (!grepl("^#1\\.[23]", header)) stop("Unsupported or missing GCT version header (#1.2 or #1.3 expected).")
      dims <- scan(con, what = character(), nlines = 1L, quiet = TRUE)
      if (header == "#1.2") {
        if (length(dims) < 2L) stop("Malformed GCT 1.2 header line.")
        nrow <- as.integer(dims[1])
        ncol <- as.integer(dims[2])
        hdr <- readLines(con, n = 1L, warn = FALSE)
        cols <- strsplit(hdr, "\t", fixed = TRUE)[[1]]
        if (length(cols) != (2L + ncol)) stop("Column count mismatch vs GCT 1.2 header.")
        tab <- read.delim(con, header = FALSE, stringsAsFactors = FALSE, quote = "", comment.char = "", nrows = nrow)
        if (ncol(tab) != (2L + ncol)) stop("Data column count mismatch in GCT body.")
        row_ids <- tab[[1]]
        row_desc <- tab[[2]]
        mat <- as.matrix(tab[, -(1:2)])
        storage.mode(mat) <- "numeric"
        rownames(mat) <- row_ids
        colnames(mat) <- cols[-(1:2)]
        list(data = mat, row_meta = data.frame(id = row_ids, desc = row_desc, stringsAsFactors = FALSE), col_meta = NULL)
      } else {
        if (length(dims) < 4L) stop("Malformed GCT 1.3 header line.")
        nrow <- as.integer(dims[1])
        ncol <- as.integer(dims[2])
        nrmeta <- as.integer(dims[3])
        ncmeta <- as.integer(dims[4])
        hdr <- readLines(con, n = 1L, warn = FALSE)
        cols <- strsplit(hdr, "\t", fixed = TRUE)[[1]]
        tab <- read.delim(con, header = FALSE, stringsAsFactors = FALSE, quote = "", comment.char = "", nrows = nrow)
        row_id <- tab[[1]]
        row_meta <- if (nrmeta > 0) tab[, 2:(1+nrmeta), drop = FALSE] else NULL
        mat <- as.matrix(tab[, (2+nrmeta):(1+nrmeta+ncol), drop = FALSE])
        storage.mode(mat) <- "numeric"
        rownames(mat) <- row_id
        col_meta <- NULL
        if (ncmeta > 0) {
          col_meta_raw <- read.delim(con, header = FALSE, stringsAsFactors = FALSE, quote = "", comment.char = "", nrows = ncmeta)
          field <- col_meta_raw[[1]]
          vals  <- as.data.frame(t(col_meta_raw[, -1, drop = FALSE]), stringsAsFactors = FALSE)
          colnames(vals) <- field
          col_meta <- vals
        }
        if (length(cols) >= (1 + nrmeta + ncol)) colnames(mat) <- cols[(2+nrmeta):(1+nrmeta+ncol)]
        list(data = mat, row_meta = row_meta, col_meta = col_meta)
      }
    }
    
    # ---- Core stats ----
    one_d_enrichment <- function(x, sets, alternative = "two.sided", min_set_size = 5L, max_set_size = Inf, adjust.method = "BH") {
      stopifnot(is.numeric(x), !is.null(names(x)))
      x <- x[is.finite(x)]
      feats <- names(x)
      out <- lapply(names(sets), function(sname) {
        members <- intersect(sets[[sname]], feats)
        n_in <- length(members)
        n_bg <- length(feats) - n_in
        if (n_in < min_set_size || n_in > max_set_size || n_bg <= 0) return(NULL)
        xi <- x[members]
        xb <- x[setdiff(feats, members)]
        wt <- suppressWarnings(wilcox.test(xi, xb, alternative = alternative, exact = FALSE, correct = FALSE))
        n1 <- length(xi)
        n2 <- length(xb)
        ranks <- rank(c(xi, xb))
        W <- sum(ranks[seq_len(n1)])
        U <- W - n1 * (n1 + 1) / 2
        r_rb <- 2 * U / (n1 * n2) - 1
        data.frame(
          set = sname,
          size_total = length(sets[[sname]]),
          size_overlap = n_in,
          median_in = median(xi), median_bg = median(xb),
          delta_median = median(xi) - median(xb),
          rank_biserial = unname(r_rb),
          W = unname(W), U = unname(U),
          p_value = wt$p.value,
          stringsAsFactors = FALSE
        )
      })
      out <- do.call(rbind, out)
      if (is.null(out) || !nrow(out)) return(data.frame())
      out$padj <- p.adjust(out$p_value, method = adjust.method)
      out$direction <- ifelse(out$rank_biserial > 0, "higher", ifelse(out$rank_biserial < 0, "lower", "neutral"))
      out[order(out$padj, -abs(out$rank_biserial)), , drop = FALSE]
    }
    
    # ---- Small plot helpers ----
    clean_labels <- function(x) {
      x <- sub("^REACTOME_", "", x)
      x <- sub("^HALLMARK_", "", x)
      x <- sub("^KEGG_", "", x)
      x <- sub("^GO_", "", x)
      gsub("_", " ", x)
    }
    
    order_df <- function(df, mode = c("abs","signed")) {
      mode <- match.arg(mode)
      df$.ord_id <- seq_len(nrow(df))
      o <- if (mode == "abs") {
        order(-abs(df$rank_biserial), -df$rank_biserial, df$padj, df$.ord_id)
      } else {
        order(-df$rank_biserial, df$padj, df$.ord_id)
      }
      df <- df[o, , drop = FALSE]
      df$.ord_id <- NULL        # <-- drop helper column
      df
    }
    
    get_palette <- function(name) {
      switch(name,
             "Blue–Red"=c("#2166AC","#F7F7F7","#B2182B"),
             "Blue–Orange"=c("#2B8CBE","#F7F7F7","#E34A33"),
             "Purple–Green"=c("#762A83","#F7F7F7","#1B7837"),
             "Teal–Magenta"=c("#01665E","#F7F7F7","#8E0152"),
             "Greys"=c("#9E9E9E","#F7F7F7","#212121"),
             "Viridis-like"=c("#440154","#F7F7F7","#2A9D8F"),
             c("#2166AC","#F7F7F7","#B2182B"))
    }
    
    get_single_color <- function(name) {
      switch(name,
             "Slate"="#546E7A","Teal"="#00897B","Blue"="#1E88E5","Purple"="#8E24AA","Crimson"="#D81B60","Orange"="#F4511E","Olive"="#7CB342","#546E7A")
    }
    
    build_color_layers <- function(df, input){
      .ensure_pkg("ggplot2")
      mode <- input$colormode
      pal <- get_palette(input$palette)
      if (mode == "none") {
        list(aes_color=NULL, layers=list(scale_color_identity()), point_params=list(color=get_single_color(input$singlecol)))
      } else if (mode == "direction") {
        df$direction <- ifelse(df$rank_biserial >= 0, "higher", "lower")
        list(aes_color=aes(color=direction),
             layers=list(scale_color_manual(values=c(lower=pal[1], higher=pal[3]), name="Direction")),
             point_params=list())
      } else {
        list(aes_color=aes(color=rank_biserial),
             layers=list(scale_color_gradient2(low=pal[1], mid=pal[2], high=pal[3], midpoint=0, name="Effect")),
             point_params=list())
      }
    }
    
    # ---- Reactives ----
    gmt_sets <- reactive({
      req(input$gmt)
      read_gmt_1D(input$gmt$datapath)
    })
    
    gct_data <- reactive({
      req(input$gct)
      read_gct(input$gct$datapath)
    })
    
    output$col_picker <- renderUI({
      req(gct_data())
      cn <- colnames(gct_data()$data)
      selectInput(ns("col"), "Select column (1D variable)", choices = cn, selected = cn[1])
    })
    
    enr <- reactive({
      req(input$col, gct_data(), gmt_sets())
      mat <- gct_data()$data
      vec <- mat[, input$col]
      names(vec) <- rownames(mat)
      one_d_enrichment(vec, gmt_sets(),
                       alternative = input$alt,
                       min_set_size = input$minsz,
                       max_set_size = input$maxsz,
                       adjust.method = input$adj)
    })
    
    # Results table
    output$tab <- DT::renderDataTable({
      .ensure_pkg("DT")
      df <- enr()
      validate(need(nrow(df) > 0, "No results"))
      DT::datatable(df, options = list(pageLength = 25), rownames = FALSE)
    })
    
    # Consistent filtering/ordering for plots
    df_bubble <- reactive({
      .ensure_pkg("ggplot2")
      .ensure_pkg("stringr")
      df <- enr()
      validate(need(nrow(df) > 0, "No results"))
      df <- df[df$padj <= input$fdr, , drop = FALSE]
      validate(need(nrow(df) > 0, "No pathways pass the FDR cutoff"))
      df$overlap_pct <- 100 * df$size_overlap / pmax(df$size_total, 1)
      df$set_clean <- clean_labels(df$set)
      df <- order_df(df, input$order)
      if (input$topn > 0) df <- utils::head(df, input$topn)
      df$set_clean <- factor(df$set_clean, levels = rev(df$set_clean))
      df
    })
    
    df_topsets <- reactive({
      .ensure_pkg("ggplot2")
      .ensure_pkg("stringr")
      df <- enr()
      validate(need(nrow(df) > 0, "No results"))
      if (isTRUE(input$apply_fdr_to_topsets)) {
        df <- df[df$padj <= input$fdr, , drop = FALSE]
        validate(need(nrow(df) > 0, "No pathways pass the FDR cutoff"))
      }
      df$set_clean <- clean_labels(df$set)
      df <- order_df(df, input$order)
      if (input$topn > 0) df <- utils::head(df, input$topn)
      df$set_clean <- factor(df$set_clean, levels = rev(df$set_clean))
      df
    })
    
    # Top sets plot
    output$plt <- renderPlot({
      .ensure_pkg("ggplot2")
      df <- df_topsets()
      neglog10 <- -log10(pmax(df$padj, .Machine$double.xmin))
      col_info <- build_color_layers(df, input)
      ggplot(df, aes(x = rank_biserial, y = set_clean)) +
        geom_segment(aes(xend = 0, yend = set_clean), col = "darkgrey") +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
        # 1) main points (your existing dynamic colouring)
        do.call(
          geom_point,
          c(
            list(mapping = modifyList(
              aes(size = neglog10),
              if (!is.null(col_info$aes_color)) col_info$aes_color else list()
            ),
            alpha = input$alpha),
            col_info$point_params
          )
        ) +
        # 2) black outline layer (constants go OUTSIDE mapping)
        do.call(
          geom_point,
          list(
            mapping = aes(size = neglog10),
            shape   = 21,
            colour  = "black",
            fill    = NA,
            stroke  = 0.3
          )
        ) +
        scale_size_continuous(name = expression(-log[10](FDR)), range = input$bub_range) +
        col_info$layers +
        labs(
          title = "Top sets (ranked by effect ordering)",
          subtitle = sprintf("FDR %s; TopN = %s",
                             ifelse(isTRUE(input$apply_fdr_to_topsets), paste0("\u2264 ", format(input$fdr, digits = 2)), "(not applied)"),
                             ifelse(input$topn > 0, input$topn, "all")),
          x = "Effect size (rank-biserial)", y = NULL
        ) +
        theme_minimal(base_size = input$base_size) +
        theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
                       plot.title = element_text(face = "bold", size = input$title_size),
                       axis.text.x = element_text(size = input$x_text),
                       axis.text.y = element_text(size = input$y_text),
                       legend.title = element_text(size = input$legend_title),
                       legend.text  = element_text(size = input$legend_text))
    })
    
    # Bubble plot
    output$bubble <- renderPlot({
      .ensure_pkg("ggplot2")
      df <- df_bubble()
      col_info <- build_color_layers(df, input)
      ggplot(df, aes(x = rank_biserial, y = set_clean)) +
        geom_segment(aes(xend = 0, yend = set_clean), col = "darkgrey") +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
        # 1) main points (dynamic colour comes from col_info$aes_color)
        do.call(
          geom_point,
          c(
            list(
              mapping = modifyList(
                aes(size = overlap_pct),
                if (!is.null(col_info$aes_color)) col_info$aes_color else list()
              ),
              alpha = input$alpha
            ),
            col_info$point_params
          )
        ) +
        # 2) black outline layer (constants go OUTSIDE mapping)
        do.call(
          geom_point,
          list(
            mapping = aes(size = overlap_pct),
            shape   = 21,
            colour  = "black",
            fill    = NA,
            stroke  = 0.3
          )
        ) +
        scale_size_continuous(name = "Overlap (%)", range = input$bub_range) +
        col_info$layers +
        labs(
          title = "Significant pathways — bubble plot",
          subtitle = sprintf("FDR \u2264 %.3g; TopN = %s", input$fdr, ifelse(input$topn > 0, input$topn, "all")),
          x = "Effect size (rank-biserial)", y = NULL
        ) +
        theme_minimal(base_size = input$base_size) +
        theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
                       plot.title = element_text(face = "bold", size = input$title_size),
                       axis.text.x = element_text(size = input$x_text),
                       axis.text.y = element_text(size = input$y_text),
                       legend.title = element_text(size = input$legend_title),
                       legend.text  = element_text(size = input$legend_text))
    })
    
    # ---- Downloads ----
    output$dl_results <- downloadHandler(
      filename = function() sprintf("1d_enrichment_%s.tsv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        utils::write.table(enr(), file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    )
    output$dl_topsets <- downloadHandler(
      filename = function() sprintf("topsets_%s.pdf", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        .ensure_pkg("ggplot2")
        df <- df_topsets()
        neglog10 <- -log10(pmax(df$padj, .Machine$double.xmin))
        col_info <- build_color_layers(df, input)
        p <- ggplot(df, aes(x = rank_biserial, y = set_clean)) +
          geom_segment(aes(xend = 0, yend = set_clean), col = "darkgrey") +
          geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
          # 1) main points (your existing dynamic colouring)
          do.call(
            geom_point,
            c(
              list(mapping = modifyList(
                aes(size = neglog10),
                if (!is.null(col_info$aes_color)) col_info$aes_color else list()
              ),
              alpha = input$alpha),
              col_info$point_params
            )
          ) +
          # 2) black outline layer (constants go OUTSIDE mapping)
          do.call(
            geom_point,
            list(
              mapping = aes(size = neglog10),
              shape   = 21,
              colour  = "black",
              fill    = NA,
              stroke  = 0.3
            )
          ) +
          scale_size_continuous(name = expression(-log[10](FDR)), range = input$bub_range) +
          col_info$layers +
          labs(title = "Top sets (ranked by effect ordering)",
                        subtitle = sprintf("FDR %s; TopN = %s",
                                           ifelse(isTRUE(input$apply_fdr_to_topsets), paste0("\u2264 ", format(input$fdr, digits = 2)), "(not applied)"),
                                           ifelse(input$topn > 0, input$topn, "all")),
                        x = "Effect size (rank-biserial)", y = NULL) +
          theme_minimal(base_size = input$base_size) +
          theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
                         plot.title = element_text(face = "bold", size = input$title_size),
                         axis.text.x = element_text(size = input$x_text),
                         axis.text.y = element_text(size = input$y_text),
                         legend.title = element_text(size = input$legend_title),
                         legend.text  = element_text(size = input$legend_text))
        ggsave(file, p, width = 11, height = 6.5, units = "in")
      }
    )
    output$dl_bubble <- downloadHandler(
      filename = function() sprintf("bubble_%s.pdf", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        .ensure_pkg("ggplot2")
        df <- df_bubble()
        col_info <- build_color_layers(df, input)
        p <- ggplot(df, aes(x = rank_biserial, y = set_clean)) +
          geom_segment(aes(xend = 0, yend = set_clean), col = "darkgrey") +
          geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
          # 1) main points (dynamic colour comes from col_info$aes_color)
          do.call(
            geom_point,
            c(
              list(
                mapping = modifyList(
                  aes(size = overlap_pct),
                  if (!is.null(col_info$aes_color)) col_info$aes_color else list()
                ),
                alpha = input$alpha
              ),
              col_info$point_params
            )
          ) +
          # 2) black outline layer (constants go OUTSIDE mapping)
          do.call(
            geom_point,
            list(
              mapping = aes(size = overlap_pct),
              shape   = 21,
              colour  = "black",
              fill    = NA,
              stroke  = 0.3
            )
          ) +
          scale_size_continuous(name = "Overlap (%)", range = input$bub_range) +
          col_info$layers +
          labs(title = "Significant pathways — bubble plot",
                        subtitle = sprintf("FDR \u2264 %.3g; TopN = %s", input$fdr, ifelse(input$topn > 0, input$topn, "all")),
                        x = "Effect size (rank-biserial)", y = NULL) +
          theme_minimal(base_size = input$base_size) +
          theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
                         plot.title = element_text(face = "bold", size = input$title_size),
                         axis.text.x = element_text(size = input$x_text),
                         axis.text.y = element_text(size = input$y_text),
                         legend.title = element_text(size = input$legend_title),
                         legend.text  = element_text(size = input$legend_text))
        ggsave(file, p, width = 11, height = 6.5, units = "in")
      }
    )
  })
}