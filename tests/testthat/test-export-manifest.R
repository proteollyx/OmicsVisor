# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: export manifest (audit OV-REP-04)
#
# The manifest's value depends entirely on it being honest about its own
# limits. It must record what the app can vouch for - which file, which
# build, which environment - and must state plainly that everything
# upstream of the workbook is unknown to it. A manifest that implied more
# provenance than exists would be worse than none, so the disclaimers are
# tested as strictly as the facts.
# ─────────────────────────────────────────────────────────

manifest_fixture <- function() {
  data.frame(
    id                   = c("P1", "P2", "P3"),
    Imputed.WT_01        = c(10, 11, 12),
    Imputed.KO_01        = c(12, 11, 10),
    logFC_KO.over.WT     = c(2, 0, -2),
    adj.P.Val_KO.over.WT = c(0.001, 0.9, 0.01),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

test_that("the manifest records the build it was produced by", {
  m <- paste(ov_manifest(), collapse = "\n")
  expect_match(m, "OmicsVisor export manifest")
  expect_match(m, ov_version, fixed = TRUE)
  expect_match(m, ov_release_date, fixed = TRUE)
})

test_that("the manifest hashes the input file so it can be identified later", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("some content", tmp)
  m <- paste(ov_manifest(file_name = "results.xlsx", file_path = tmp),
             collapse = "\n")
  expect_match(m, "results.xlsx", fixed = TRUE)
  expect_match(m, "SHA-256")
  expect_match(m, "[0-9a-f]{64}")
})

test_that("the recorded hash is the real SHA-256 of the file", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("some content", tmp)
  expected <- digest::digest(file = tmp, algo = "sha256")
  m <- ov_manifest(file_path = tmp)
  expect_true(any(grepl(expected, m, fixed = TRUE)))
})

test_that("two different files get two different hashes", {
  a <- withr::local_tempfile(); b <- withr::local_tempfile()
  writeLines("alpha", a); writeLines("beta", b)
  ha <- grep("SHA-256", ov_manifest(file_path = a), value = TRUE)
  hb <- grep("SHA-256", ov_manifest(file_path = b), value = TRUE)
  expect_false(identical(ha, hb))
})

test_that("a missing file degrades gracefully instead of erroring", {
  expect_silent(m <- ov_manifest(file_path = "/no/such/file.xlsx"))
  expect_match(paste(m, collapse = "\n"), "not available")
})

test_that("the manifest carries the inspection results when given them", {
  rep <- ov_inspect_upload(manifest_fixture(), "^Imputed")
  m   <- paste(ov_manifest(report = rep, int_regex = "^Imputed"), collapse = "\n")
  expect_match(m, "KO.over.WT", fixed = TRUE)
  expect_match(m, "3 unique")
  expect_match(m, "\\^Imputed")
})

test_that("upload warnings are carried into the manifest", {
  df <- manifest_fixture()
  df$id[2] <- "P1"
  rep <- ov_inspect_upload(df, "^Imputed")
  m   <- paste(ov_manifest(report = rep), collapse = "\n")
  expect_match(m, "Warnings raised on upload")
  expect_match(m, "duplicate", ignore.case = TRUE)
})

test_that("no warnings section appears when the upload was clean", {
  rep <- ov_inspect_upload(manifest_fixture(), "^Imputed")
  m   <- paste(ov_manifest(report = rep), collapse = "\n")
  expect_false(grepl("Warnings raised on upload", m))
})

test_that("the environment is pinned to the versions that produced the output", {
  m <- paste(ov_manifest(), collapse = "\n")
  expect_match(m, "Platform")
  expect_match(m, paste0("R:\\s+", R.version$major))
  for (p in c("shiny", "openxlsx", "ggplot2", "umap"))
    expect_match(m, paste0(p, ":\\s+\\d"))
})

test_that("the manifest states what it cannot know", {
  # These disclaimers are the substance of the finding, not decoration.
  m <- paste(ov_manifest(), collapse = "\n")
  expect_match(m, "does NOT record", fixed = TRUE)
  expect_match(m, "search engine")
  expect_match(m, "normalisation")
  expect_match(m, "statistical test")
  expect_match(m, "multiple-testing correction")
  expect_match(m, "log2 is assumed and never verified", fixed = TRUE)
  # Per-module settings were on this list until the module settings registry
  # landed. They are now recorded, so claiming ignorance of them would be
  # false in the other direction.
  expect_false(grepl("per-module settings", m))
})

test_that("the manifest is plain text with no unresolved placeholders", {
  m <- ov_manifest(report = ov_inspect_upload(manifest_fixture(), "^Imputed"))
  expect_type(m, "character")
  expect_false(any(grepl("NULL|NA_character_|character\\(0\\)", m)))
})


# ── the download handler ─────────────────────────────────────────────────────

test_that("the Data Overview manifest download writes a readable file", {
  df  <- manifest_fixture()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("stand-in for the workbook", tmp)

  shiny::testServer(
    data_overview_server,
    args = list(
      data      = reactive(list(data = df)),
      report    = reactiveVal(ov_inspect_upload(df, "^Imputed")),
      file_info = reactiveVal(list(name = "results.xlsx", datapath = tmp))
    ),
    {
      txt <- readLines(output$download_manifest, warn = FALSE)
      expect_gt(length(txt), 20)
      joined <- paste(txt, collapse = "\n")
      expect_match(joined, "OmicsVisor export manifest")
      expect_match(joined, "results.xlsx", fixed = TRUE)
      expect_match(joined, "does NOT record", fixed = TRUE)
    }
  )
})

test_that("the manifest downloads even before a file has been uploaded", {
  # It still pins the build and environment, which is the part that matters
  # when someone asks months later which version produced a figure.
  shiny::testServer(
    data_overview_server,
    args = list(data = reactive(list(data = manifest_fixture()))),
    {
      txt <- paste(readLines(output$download_manifest, warn = FALSE),
                   collapse = "\n")
      expect_match(txt, ov_version, fixed = TRUE)
      expect_match(txt, "not recorded")
    }
  )
})


# ── module settings in the manifest (audit OV-REP-15) ───────────────────────
# The response to the audit committed to recording the module, the comparison,
# the cutoffs *and their inclusivity*, PCA centring and scaling, the UMAP seed,
# retained and excluded feature counts, and the within-module p-adjustment.
# These check that commitment is actually met.

test_that("registered module settings appear in the manifest", {
  mods <- list(
    "Volcano Printer" = list(comparison = "KO.over.WT", logFC_cutoff = 1,
                             adj_P_cutoff = 0.05,
                             boundaries = "inclusive (|logFC| >= cutoff, adj.P <= cutoff)"),
    "PCA / UMAP"      = list(method = "UMAP", umap_seed = 42L,
                             features_retained = 1500L, features_excluded = 500L)
  )
  m <- paste(ov_manifest(modules = mods), collapse = "\n")
  expect_match(m, "Module settings")
  expect_match(m, "Volcano Printer")
  expect_match(m, "KO.over.WT", fixed = TRUE)
  expect_match(m, "inclusive", fixed = TRUE)
  expect_match(m, "umap_seed:\\s+42")
  expect_match(m, "features_excluded:\\s+500")
})

test_that("logical settings read as yes/no rather than TRUE/FALSE", {
  m <- paste(ov_manifest(modules = list(Heatmap = list(row_z_score = TRUE,
                                                       cluster_rows = FALSE))),
             collapse = "\n")
  expect_match(m, "row_z_score:\\s+yes")
  expect_match(m, "cluster_rows:\\s+no")
})

test_that("modules the user never opened are absent, not reported at defaults", {
  m <- paste(ov_manifest(modules = list(Heatmap = list(row_z_score = TRUE))),
             collapse = "\n")
  expect_match(m, "Heatmap")
  expect_false(grepl("Volcano Printer", m))
})

test_that("no Module settings section appears when nothing was registered", {
  expect_false(any(grepl("Module settings", ov_manifest())))
  expect_false(any(grepl("Module settings", ov_manifest(modules = list()))))
})

test_that("the manifest records the commit, or says plainly that it cannot", {
  m <- paste(ov_manifest(), collapse = "\n")
  expect_match(m, "Commit:")
  commit <- sub(".*Commit:\\s+", "", regmatches(m, regexpr("Commit:[^\n]*", m)))
  expect_true(grepl("^[0-9a-f]{7,}$", commit) || grepl("not available", commit))
})

test_that("ov_register_settings is a no-op without a register function", {
  expect_silent(ov_register_settings(NULL, "X", list(a = 1)))
  expect_null(ov_register_settings(NULL, "X", list(a = 1)))
})

test_that("ov_register_settings drops unset values rather than printing NULL", {
  got <- NULL
  reg <- function(m, v) got <<- v
  ov_register_settings(reg, "PCA", list(method = "PCA", umap_seed = NULL))
  expect_named(got, "method")
})

test_that("a module registers the settings it was actually run with", {
  d <- sim_omics(n_features = 40)
  b <- ov_bundle(d)
  seen <- new.env()
  reg  <- function(m, v) assign(m, v, envir = seen)

  shiny::testServer(
    donut_plot_server,
    args = list(data = reactive(b), register = reg),
    {
      session$setInputs(logfc_cutoff = 1.5, pval_cutoff = 0.01,
                        apply_cutoff = 1)
      session$flushReact()
    }
  )
  s <- get("Donut", envir = seen)
  expect_equal(s$logFC_cutoff, 1.5)
  expect_equal(s$adj_P_cutoff, 0.01)
  expect_match(s$boundaries, "inclusive")
})

test_that("PCA registers centring, scaling and the retention counts", {
  d   <- sim_omics(n_features = 40, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(d), value = TRUE)
  d[1:10, int[1]] <- NA
  seen <- new.env()
  reg  <- function(m, v) assign(m, v, envir = seen)

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = d, intensity_cols = int)),
                register = reg),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = int,
                        row_selection = "all", id_selection = "",
                        pca_center = TRUE, pca_scale = FALSE)
      session$flushReact()
    }
  )
  s <- get("PCA / UMAP", envir = seen)
  expect_equal(s$method, "PCA")
  expect_true(s$pca_centred)
  expect_false(s$pca_scaled)
  expect_equal(s$features_retained, 30L)
  expect_equal(s$features_excluded, 10L)
  expect_null(s$umap_seed)          # not a UMAP run; must not be invented
})
