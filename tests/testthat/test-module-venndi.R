# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: venndi_module.R
# ─────────────────────────────────────────────────────────

test_that("venndi splits two lists into unique / unique / overlap", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 2,
                      list1_input = "A,B,C,D",
                      list2_input = "C,D,E",
                      generate_lists = 1)
    expect_equal(get_unique_1(), c("A", "B"))
    expect_equal(get_unique_2(), "E")
    expect_equal(get_overlap(),  c("C", "D"))
  })
})

test_that("venndi accepts mixed delimiters and duplicate entries", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 2,
                      list1_input = "A, B\nC\tD;E  A",
                      list2_input = "E",
                      generate_lists = 1)
    expect_equal(get_unique_1(), c("A", "B", "C", "D"))
    expect_equal(get_overlap(),  "E")
  })
})

test_that("venndi renders a pairwise Venn even when one list is empty", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 2, list1_input = "A,B,C",
                      list2_input = "", generate_lists = 1)
    expect_no_error(output$venn_plot)
    expect_equal(get_unique_1(), c("A", "B", "C"))
    expect_length(get_overlap(), 0)
  })
})

test_that("venndi refuses to plot when both lists are empty", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 2, list1_input = "", list2_input = "",
                      generate_lists = 1)
    expect_error(output$venn_plot, class = "shiny.silent.error")
  })
})

test_that("venndi builds mutually exclusive groups for three lists", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 3,
                      list1_input = "A,B,C,D",
                      list2_input = "C,D,E",
                      list3_input = "D,E,F",
                      generate_lists = 1)
    g <- values$groups
    expect_equal(g[["List 1 & List 2 & List 3"]], "D")
    expect_equal(g[["List 1 & List 2"]],          "C")
    expect_equal(g[["List 2 & List 3"]],          "E")
    expect_setequal(g[["List 1"]], c("A", "B"))
    expect_equal(g[["List 3"]], "F")

    # Every ID is in exactly one group.
    expect_false(anyDuplicated(unlist(g, use.names = FALSE)) > 0)
    expect_setequal(unlist(g, use.names = FALSE), c("A","B","C","D","E","F"))
  })
})

test_that("venndi handles a single shared ID across three lists", {
  # sapply() collapses to a vector when there is only one ID, which breaks the
  # membership matrix and the signature split.
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 3, list1_input = "A", list2_input = "A",
                      list3_input = "", generate_lists = 1)
    g <- values$groups
    expect_length(unlist(g, use.names = FALSE), 1L)
    expect_equal(unname(unlist(g)), "A")
    expect_equal(names(g), "List 1 & List 2")
  })
})

test_that("venndi renders an UpSet plot for three or more lists", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 4,
                      list1_input = paste(LETTERS[1:10], collapse = ","),
                      list2_input = paste(LETTERS[5:15], collapse = ","),
                      list3_input = paste(LETTERS[8:20], collapse = ","),
                      list4_input = paste(LETTERS[1:5],  collapse = ","),
                      generate_lists = 1)
    expect_no_error(output$venn_plot)
  })
})

test_that("venndi refuses an UpSet plot with fewer than two non-empty lists", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 3, list1_input = "A,B,C",
                      list2_input = "", list3_input = "", generate_lists = 1)
    expect_error(output$venn_plot, class = "shiny.silent.error")
  })
})

test_that("venndi clears state when the number of lists changes", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 2, list1_input = "A,B", list2_input = "B,C",
                      generate_lists = 1)
    expect_length(values$lists, 2)
    session$setInputs(n_lists = 3)
    expect_null(values$lists)
  })
})

test_that("venndi intersection extractor returns the selected group's IDs", {
  testServer(venndi_server, args = list(), {
    session$setInputs(n_lists = 3,
                      list1_input = "A,B,C,D", list2_input = "C,D,E",
                      list3_input = "D,E,F", generate_lists = 1)
    target <- which(names(values$groups) == "List 1 & List 2 & List 3")
    session$setInputs(selected_intersection = as.character(target))
    expect_equal(output$intersection_ids_text, "D")
    expect_equal(output$intersection_n_text,   "Count: 1")
  })
})
