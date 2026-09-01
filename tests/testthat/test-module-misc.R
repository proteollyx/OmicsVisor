# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: data overview, ID list generator, regex tool,
#                     1D enrichment, and the static modules
# ─────────────────────────────────────────────────────────

# ── Data Overview ────────────────────────────────────────────────────────────

test_that("data overview reports the IDs of the selected rows", {
  d <- sim_omics(n_features = 20)
  testServer(data_overview_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(data_preview_rows_selected = c(2L, 5L))
    expect_equal(selected_ids(), d$id[c(2, 5)])
    expect_match(output$selection_preview, "^2 selected: ")
  })
})

test_that("data overview says so when nothing is selected", {
  d <- sim_omics(n_features = 20)
  testServer(data_overview_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(data_preview_rows_selected = integer(0))
    expect_length(selected_ids(), 0)
    expect_equal(output$selection_preview, "No rows selected.")
  })
})

test_that("data overview falls back to row numbers when there is no id column", {
  d <- sim_omics(n_features = 10)
  d$id <- NULL
  testServer(data_overview_server, args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(data_preview_rows_selected = c(1L, 3L))
    expect_equal(selected_ids(), c("1", "3"))
  })
})


# ── ID List Generator ────────────────────────────────────────────────────────

test_that("ID list generator maps gene symbols to feature IDs", {
  d <- sim_omics(n_features = 50)
  testServer(id_list_generator_server,
             args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(gene_list = paste(d$Genes[1:3], collapse = ", "),
                      search_column = "Genes",
                      remove_na = FALSE, ignore_case = FALSE)
    expect_setequal(matched_ids(), d$id[1:3])
  })
})

test_that("ID list generator does not emit NA for genes with no match", {
  # find_genes() returns NA for a miss; id_column[NA] then yields NA, which is
  # pasted straight into the output the user copies.
  d <- sim_omics(n_features = 50)
  testServer(id_list_generator_server,
             args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(gene_list = paste(c(d$Genes[1], "NOT_A_GENE"),
                                        collapse = ", "),
                      search_column = "Genes",
                      remove_na = FALSE, ignore_case = FALSE)
    expect_setequal(matched_ids(), d$id[1])
    expect_false(any(is.na(matched_ids())))
    expect_false(grepl("NA", output$id_output, fixed = TRUE))
  })
})

test_that("ID list generator honours ignore case", {
  d <- sim_omics(n_features = 20)
  testServer(id_list_generator_server,
             args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(gene_list = tolower(d$Genes[1]), search_column = "Genes",
                      remove_na = TRUE, ignore_case = FALSE)
    expect_length(matched_ids(), 0)
    session$setInputs(ignore_case = TRUE)
    expect_equal(matched_ids(), d$id[1])
  })
})

test_that("ID list generator accepts newline-separated input", {
  d <- sim_omics(n_features = 20)
  testServer(id_list_generator_server,
             args = list(data = reactive(ov_bundle(d))), {
    session$setInputs(gene_list = paste(d$Genes[1:3], collapse = "\n"),
                      search_column = "Genes",
                      remove_na = TRUE, ignore_case = FALSE)
    expect_setequal(matched_ids(), d$id[1:3])
  })
})


# ── Regex Tool ───────────────────────────────────────────────────────────────

test_that("regex tool strips the accession suffix from IDs", {
  testServer(regex_tool_server, args = list(), {
    session$setInputs(text_input = "ACE_P12821, PARP1_P09874, TP53_P04637",
                      regex_pattern = "_.*", unique_output = FALSE)
    expect_equal(processed_text(), "ACE\nPARP1\nTP53")
  })
})

test_that("regex tool can de-duplicate its output", {
  testServer(regex_tool_server, args = list(), {
    session$setInputs(text_input = "A_1, A_2, B_1",
                      regex_pattern = "_.*", unique_output = TRUE)
    expect_equal(processed_text(), "A\nB")
  })
})


# ── 1D Enrichment ────────────────────────────────────────────────────────────

test_that("1D enrichment reads a GMT file and drops the description column", {
  testServer(mod_pathway_1D_server, args = list(), {
    sets <- read_gmt_1D(sim_write_gmt())
    expect_named(sets, c("SET_ALPHA", "SET_BETA", "SET_TINY"))
    expect_length(sets$SET_ALPHA, 30)
    expect_equal(attr(sets, "meta")$size, c(30L, 41L, 2L))
  })
})

test_that("1D enrichment rejects a file without a GCT version header", {
  bad <- tempfile(fileext = ".gct")
  writeLines(c("not a gct", "1\t1"), bad)
  testServer(mod_pathway_1D_server, args = list(), {
    expect_error(read_gct(bad), "GCT version header")
  })
})

test_that("1D enrichment detects a planted up-shifted gene set", {
  testServer(mod_pathway_1D_server, args = list(), {
    set.seed(99)
    genes <- sprintf("GENE%04d", 1:500)
    x <- stats::rnorm(500)
    names(x) <- genes
    x[1:40] <- x[1:40] + 3            # SET_HOT is strongly shifted upward

    sets <- list(SET_HOT  = genes[1:40],
                 SET_COLD = genes[100:160],
                 SET_TINY = genes[1:2])

    res <- one_d_enrichment(x, sets, min_set_size = 5)
    expect_true("SET_HOT" %in% res$set)
    expect_false("SET_TINY" %in% res$set)        # below min_set_size
    expect_gt(res$rank_biserial[res$set == "SET_HOT"], 0.5)
    expect_lt(res$padj[res$set == "SET_HOT"], 0.01)
    expect_equal(res$direction[res$set == "SET_HOT"], "higher")
  })
})

test_that("1D enrichment ignores non-finite values in the ranking vector", {
  testServer(mod_pathway_1D_server, args = list(), {
    genes <- sprintf("GENE%04d", 1:200)
    x <- stats::rnorm(200); names(x) <- genes
    x[c(1, 2, 3)] <- c(NA, Inf, -Inf)
    sets <- list(S1 = genes[1:50], S2 = genes[100:150])
    res <- one_d_enrichment(x, sets, min_set_size = 5)
    expect_true(all(is.finite(res$p_value)))
    expect_true(all(is.finite(res$median_in)))
    expect_equal(res$size_overlap[res$set == "S1"], 47L)   # 3 dropped
  })
})

test_that("1D enrichment returns an empty frame when no set qualifies", {
  testServer(mod_pathway_1D_server, args = list(), {
    genes <- sprintf("GENE%04d", 1:50)
    x <- stats::rnorm(50); names(x) <- genes
    res <- one_d_enrichment(x, list(S1 = genes[1:2]), min_set_size = 5)
    expect_equal(nrow(res), 0L)
  })
})

test_that("1D enrichment p-value adjustment methods are all accepted", {
  testServer(mod_pathway_1D_server, args = list(), {
    genes <- sprintf("GENE%04d", 1:300)
    x <- stats::rnorm(300); names(x) <- genes
    sets <- list(S1 = genes[1:40], S2 = genes[50:120], S3 = genes[200:280])
    for (m in c("BH", "bonferroni", "holm", "none")) {
      res <- one_d_enrichment(x, sets, adjust.method = m)
      expect_equal(nrow(res), 3L)
      expect_true(all(res$padj >= res$p_value - 1e-12))
    }
  })
})


# ── Static modules ───────────────────────────────────────────────────────────

test_that("the static modules build their UI and start their server", {
  for (nm in c("documentation", "disclaimer", "about")) {
    ui_fn     <- get(paste0(nm, "_ui"))
    server_fn <- get(paste0(nm, "_server"))
    expect_s3_class(ui_fn(nm), "shiny.tag.list")
    expect_no_error(testServer(server_fn, args = list(), { session$flushReact() }))
  }
})

test_that("every module UI function returns a renderable tag list", {
  ui_fns <- c("data_overview_ui", "volcano_plot_ui", "donut_plot_ui",
              "upset_plot_ui", "heatmap_ui", "pca_ui", "volcano_printer_ui",
              "scatterplot_ui", "venndi_ui", "id_list_generator_ui",
              "plot_ui", "regex_tool_ui", "correlation_ui", "gct_export_ui",
              "mod_pathway_1D_ui", "documentation_ui", "disclaimer_ui",
              "about_ui")
  for (fn in ui_fns) {
    tags <- get(fn)("test_id")
    expect_true(inherits(tags, "shiny.tag.list") || inherits(tags, "shiny.tag"),
                info = fn)
    expect_no_error(as.character(tags))
  }
})
