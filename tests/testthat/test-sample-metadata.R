# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: optional sample metadata (audit OV-UX-17 / 2.7)
#
# Grouping is derived by splitting intensity column names on "_" or ".",
# which works for the standard in-house export and asks nothing of the user,
# but is fragile for anything else. An explicit table now supersedes the
# heuristic when supplied; the heuristic remains the default.
#
# The reporting matters as much as the matching. A metadata file that
# silently applies to only some samples would be worse than none, because
# the resulting grouping would still look deliberate.
# ─────────────────────────────────────────────────────────

# Plain tempfile(): withr::local_tempfile() would tie the file's lifetime to
# this helper's own frame, deleting it before the caller can read it.
meta_csv <- function(txt, ext = ".csv") {
  p <- tempfile(fileext = ext)
  writeLines(txt, p)
  p
}

INT <- c("Imputed.WT_01", "Imputed.WT_02", "Imputed.KO_01", "Imputed.KO_02")

full_meta <- function() meta_csv(c(
  "sample,condition,batch,replicate",
  "Imputed.WT_01,WT,B1,1",
  "Imputed.WT_02,WT,B2,2",
  "Imputed.KO_01,KO,B1,1",
  "Imputed.KO_02,KO,B2,2"))

test_that("a complete metadata table matches every sample", {
  m <- ov_read_sample_metadata(full_meta(), "meta.csv", samples = INT)
  expect_equal(m$attributes, c("condition", "batch", "replicate"))
  expect_setequal(m$matched, INT)
  expect_length(m$unmatched, 0)
  expect_length(m$extra, 0)
  expect_length(m$warnings, 0)
})

test_that("grouping by one attribute gives that attribute's levels", {
  m <- ov_read_sample_metadata(full_meta(), "meta.csv", samples = INT)
  expect_equal(ov_metadata_groups(m, INT, "condition"), c("WT", "WT", "KO", "KO"))
  expect_equal(ov_metadata_groups(m, INT, "batch"),     c("B1", "B2", "B1", "B2"))
})

test_that("grouping by several attributes concatenates them in order", {
  m <- ov_read_sample_metadata(full_meta(), "meta.csv", samples = INT)
  expect_equal(ov_metadata_groups(m, INT, c("condition", "batch")),
               c("WT_B1", "WT_B2", "KO_B1", "KO_B2"))
})

test_that("samples with no metadata row fall back to their own name, and are reported", {
  p <- meta_csv(c("sample,condition",
                  "Imputed.WT_01,WT",
                  "Imputed.KO_01,KO"))
  m <- ov_read_sample_metadata(p, "meta.csv", samples = INT)
  expect_setequal(m$unmatched, c("Imputed.WT_02", "Imputed.KO_02"))
  expect_true(any(grepl("no metadata row", m$warnings)))
  expect_equal(ov_metadata_groups(m, INT, "condition"),
               c("WT", "Imputed.WT_02", "KO", "Imputed.KO_02"))
})

test_that("metadata rows matching no sample are reported and ignored", {
  p <- meta_csv(c("sample,condition",
                  "Imputed.WT_01,WT", "Imputed.WT_02,WT",
                  "Imputed.KO_01,KO", "Imputed.KO_02,KO",
                  "Imputed.GHOST_09,XX"))
  m <- ov_read_sample_metadata(p, "meta.csv", samples = INT)
  expect_equal(m$extra, "Imputed.GHOST_09")
  expect_true(any(grepl("match no intensity column", m$warnings)))
  expect_length(m$unmatched, 0)
})

test_that("duplicate sample rows are reduced to the first and reported", {
  p <- meta_csv(c("sample,condition",
                  "Imputed.WT_01,WT", "Imputed.WT_01,MISLABELLED",
                  "Imputed.WT_02,WT", "Imputed.KO_01,KO", "Imputed.KO_02,KO"))
  m <- ov_read_sample_metadata(p, "meta.csv", samples = INT)
  expect_true(any(grepl("Duplicate sample name", m$warnings)))
  expect_equal(ov_metadata_groups(m, INT, "condition")[1], "WT")
})

test_that("a table without a sample column is rejected with an actionable message", {
  p <- meta_csv(c("name,condition", "Imputed.WT_01,WT"))
  expect_error(ov_read_sample_metadata(p, "meta.csv"), "needs a 'sample' column")
})

test_that("a table with no attributes beyond sample is rejected", {
  p <- meta_csv(c("sample", "Imputed.WT_01"))
  expect_error(ov_read_sample_metadata(p, "meta.csv"), "no attributes")
})

test_that("an empty table is rejected", {
  p <- meta_csv("sample,condition")
  expect_error(ov_read_sample_metadata(p, "meta.csv"), "empty")
})

test_that("the sample column is matched case-insensitively and trimmed", {
  p <- meta_csv(c("Sample , condition",
                  " Imputed.WT_01 ,WT", "Imputed.WT_02,WT",
                  "Imputed.KO_01,KO", "Imputed.KO_02,KO"))
  m <- ov_read_sample_metadata(p, "meta.csv", samples = INT)
  expect_setequal(m$matched, INT)
})

test_that("TSV metadata is read as well as CSV", {
  p <- meta_csv(c("sample\tcondition", "Imputed.WT_01\tWT", "Imputed.WT_02\tWT",
                  "Imputed.KO_01\tKO", "Imputed.KO_02\tKO"), ext = ".tsv")
  m <- ov_read_sample_metadata(p, "meta.tsv", samples = INT)
  expect_setequal(m$matched, INT)
  expect_equal(m$attributes, "condition")
})

# ── in the module ───────────────────────────────────────────────────────────

test_that("supplied metadata supersedes filename parsing", {
  d <- data.frame(id = sprintf("P%03d", 1:30), stringsAsFactors = FALSE)
  for (c in INT) d[[c]] <- rnorm(30, 20, 1)

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = d, intensity_cols = INT))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = INT,
                        row_selection = "all", id_selection = "",
                        group_component_2 = TRUE)     # filename heuristic on
      expect_equal(group_annotations(), c("WT", "WT", "KO", "KO"))

      session$setInputs(
        sample_metadata = list(name = "meta.csv", datapath = full_meta()),
        metadata_group_cols = "batch")
      # The explicit table wins even though the checkbox is still ticked.
      expect_equal(group_annotations(), c("B1", "B2", "B1", "B2"))
    }
  )
})

test_that("the module reports how many samples the metadata matched", {
  d <- data.frame(id = sprintf("P%03d", 1:30), stringsAsFactors = FALSE)
  for (c in INT) d[[c]] <- rnorm(30, 20, 1)
  partial <- meta_csv(c("sample,condition", "Imputed.WT_01,WT"))

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = d, intensity_cols = INT))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = INT,
                        row_selection = "all", id_selection = "",
                        sample_metadata = list(name = "m.csv", datapath = partial))
      html <- as.character(output$metadata_status$html)
      expect_match(html, "1 of 4 samples matched")
      expect_match(html, "no metadata row")
    }
  )
})

test_that("the manifest records which grouping was used", {
  d <- data.frame(id = sprintf("P%03d", 1:30), stringsAsFactors = FALSE)
  for (c in INT) d[[c]] <- rnorm(30, 20, 1)
  seen <- new.env(); reg <- function(m, v) assign(m, v, envir = seen)

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = d, intensity_cols = INT)), register = reg),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = INT,
                        row_selection = "all", id_selection = "")
      session$flushReact()
      expect_match(get("PCA / UMAP", envir = seen)$grouping, "column names")

      session$setInputs(sample_metadata = list(name = "meta.csv", datapath = full_meta()),
                        metadata_group_cols = "condition")
      session$flushReact()
      s <- get("PCA / UMAP", envir = seen)
      expect_match(s$grouping, "meta.csv")
      expect_equal(s$grouped_by, "condition")
    }
  )
})

test_that("a malformed metadata file does not take the module down", {
  d <- data.frame(id = sprintf("P%03d", 1:30), stringsAsFactors = FALSE)
  for (c in INT) d[[c]] <- rnorm(30, 20, 1)
  bad <- meta_csv(c("name,condition", "x,y"))

  notes <- character(0)
  local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible(NULL) },
    .package = "shiny")

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = d, intensity_cols = INT))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = INT,
                        row_selection = "all", id_selection = "",
                        group_component_2 = TRUE,
                        sample_metadata = list(name = "bad.csv", datapath = bad))
      # Falls back to the heuristic rather than erroring.
      expect_equal(group_annotations(), c("WT", "WT", "KO", "KO"))
    }
  )
  expect_true(any(grepl("sample' column", notes)))
})

test_that("the heatmap honours supplied metadata for its column annotations", {
  d <- data.frame(id = sprintf("P%03d", 1:30), stringsAsFactors = FALSE)
  for (c in INT) d[[c]] <- rnorm(30, 20, 1)
  seen <- new.env(); reg <- function(m, v) assign(m, v, envir = seen)

  shiny::testServer(
    heatmap_server,
    args = list(data = reactive(list(data = d, intensity_cols = INT)), register = reg),
    {
      session$setInputs(intensity_columns = INT, scale_rows = FALSE,
                        cluster_rows = FALSE, cluster_columns = FALSE,
                        use_custom_limits = FALSE,
                        sample_metadata = list(name = "meta.csv", datapath = full_meta()),
                        metadata_group_cols = "condition")
      expect_equal(group_annotations(), c("WT", "WT", "KO", "KO"))
      html <- as.character(output$metadata_status$html)
      expect_match(html, "4 of 4 samples matched")
      session$flushReact()
      expect_match(get("Heatmap", envir = seen)$grouping, "meta.csv")
    }
  )
})

test_that("the heatmap falls back to component parsing without metadata", {
  d <- data.frame(id = sprintf("P%03d", 1:30), stringsAsFactors = FALSE)
  for (c in INT) d[[c]] <- rnorm(30, 20, 1)
  seen <- new.env(); reg <- function(m, v) assign(m, v, envir = seen)

  shiny::testServer(
    heatmap_server,
    args = list(data = reactive(list(data = d, intensity_cols = INT)), register = reg),
    {
      session$setInputs(intensity_columns = INT, scale_rows = FALSE,
                        cluster_rows = FALSE, cluster_columns = FALSE,
                        use_custom_limits = FALSE)
      expect_null(output$metadata_status)
      session$flushReact()
      expect_match(get("Heatmap", envir = seen)$grouping, "column names")
    }
  )
})
