# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: dimension-reduction retention (audit OV-NUM-09)
#
# PCA and UMAP need a complete matrix, so complete-case filtering silently
# decides how much of the data the ordination is actually based on. Two things
# are tested here: that the accounting is correct, and that the feature labels
# attached to the PCA loadings survive every row filter in step with the
# rotation matrix they name.
# ─────────────────────────────────────────────────────────

test_that("retention counts complete features and those dropped", {
  m <- matrix(1, 10, 3, dimnames = list(NULL, c("S1", "S2", "S3")))
  m[1:4, 2] <- NA
  r <- ov_dr_retention(m)
  expect_equal(r$n_in, 10)
  expect_equal(r$n_complete, 6)
  expect_equal(r$n_dropped, 4)
  expect_equal(r$pct_retained, 60)
  expect_equal(sum(r$complete), 6)
})

test_that("non-finite values count as missing, not just NA", {
  # Inf arises from log-ratioing all-or-nothing quantification and breaks
  # prcomp just as surely as NA does.
  m <- matrix(1, 5, 2, dimnames = list(NULL, c("S1", "S2")))
  m[1, 1] <- Inf; m[2, 2] <- NaN; m[3, 1] <- NA
  r <- ov_dr_retention(m)
  expect_equal(r$n_complete, 2)
})

test_that("recoverable counts features missing in exactly one sample", {
  m <- matrix(1, 10, 3, dimnames = list(NULL, c("S1", "S2", "S3")))
  m[1:4, 2] <- NA        # recoverable by dropping S2
  m[5, c(1, 3)] <- NA    # recoverable by dropping neither alone
  r <- ov_dr_retention(m)
  ps <- r$per_sample
  expect_equal(ps$recoverable[ps$sample == "S2"], 4L)
  expect_equal(ps$recoverable[ps$sample == "S1"], 0L)
  expect_equal(ps$recoverable[ps$sample == "S3"], 0L)
  expect_equal(r$worst_sample$sample, "S2")
})

test_that("no worst sample is nominated when nothing is recoverable", {
  m <- matrix(1, 6, 3, dimnames = list(NULL, c("S1", "S2", "S3")))
  m[1, c(1, 2)] <- NA          # missing in two samples
  r <- ov_dr_retention(m)
  expect_equal(r$n_dropped, 1)
  expect_null(r$worst_sample)
})

test_that("a fully complete matrix reports 100% and drops nothing", {
  m <- matrix(rnorm(30), 10, 3, dimnames = list(NULL, c("S1", "S2", "S3")))
  r <- ov_dr_retention(m)
  expect_equal(r$n_dropped, 0)
  expect_equal(r$pct_retained, 100)
  expect_null(r$worst_sample)
  expect_true(all(r$recoverable == 0 | is.na(r$recoverable)))
})

test_that("per-sample missingness is ordered worst first", {
  m <- matrix(1, 10, 3, dimnames = list(NULL, c("S1", "S2", "S3")))
  m[1:2, 1] <- NA; m[1:5, 3] <- NA
  r <- ov_dr_retention(m)
  expect_equal(r$per_sample$sample[1], "S3")
  expect_equal(r$per_sample$n_missing, c(5L, 2L, 0L))
})

test_that("retention survives a matrix where every feature is dropped", {
  m <- matrix(NA_real_, 4, 2, dimnames = list(NULL, c("S1", "S2")))
  r <- ov_dr_retention(m)
  expect_equal(r$n_complete, 0)
  expect_equal(r$pct_retained, 0)
})


# ── the loadings-label bug this restructuring fixes ──────────────────────────

test_that("PCA loadings stay labelled with the features they belong to", {
  # Before this fix a separate reactive re-derived the ids from the unfiltered
  # frame, so the zero-variance drop applied inside the scaled PCA left the
  # label vector longer than the rotation matrix. Usually that errored; when
  # the lengths happened to divide evenly, cbind() recycled instead and every
  # loading was silently attributed to the wrong feature.
  set.seed(42)
  n <- 12
  df <- sim_omics(n_features = n, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(df), value = TRUE)
  df[3, int] <- 15      # constant -> dropped by the scaled PCA
  df[7, int] <- 15

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = df, intensity_cols = int))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = int,
                        row_selection = "all", id_selection = "",
                        pca_scale = TRUE, pca_center = TRUE)
      res <- pca_results()
      expect_equal(length(res$ids), nrow(res$pca$rotation))
      # the two constant features must be absent, everything else present
      expect_false(any(df$id[c(3, 7)] %in% res$ids))
      expect_setequal(res$ids, df$id[-c(3, 7)])
    }
  )
})

test_that("dr_data reports retention alongside the matrix it filtered", {
  df  <- sim_omics(n_features = 20, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(df), value = TRUE)
  df[1:5, int[1]] <- NA

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = df, intensity_cols = int))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = int,
                        row_selection = "all", id_selection = "")
      dd <- dr_data()
      expect_equal(nrow(dd$mat), 15)
      expect_equal(length(dd$ids), 15)
      expect_equal(dd$retention$n_dropped, 5)
      expect_equal(dd$retention$worst_sample$sample, int[1])
      expect_equal(dd$retention$worst_sample$recoverable, 5L)
      # ids must correspond to the surviving rows, not the first 15
      expect_setequal(dd$ids, df$id[6:20])
    }
  )
})

test_that("the retention panel states the count and names the worst sample", {
  df  <- sim_omics(n_features = 20, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(df), value = TRUE)
  df[1:5, int[1]] <- NA

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = df, intensity_cols = int))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = int,
                        row_selection = "all", id_selection = "")
      html <- as.character(output$dr_retention$html)
      expect_match(html, "15 of 20 features complete")
      expect_match(html, "5 dropped")
      expect_match(html, paste0("Excluding ", int[1], " alone"))
      expect_match(html, "Missing values per sample")
    }
  )
})

test_that("the panel is quiet about recovery when the matrix is complete", {
  df  <- sim_omics(n_features = 20, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(df), value = TRUE)

  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = df, intensity_cols = int))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = int,
                        row_selection = "all", id_selection = "")
      html <- as.character(output$dr_retention$html)
      expect_match(html, "20 of 20 features complete")
      expect_false(grepl("Excluding", html))
      expect_false(grepl("Missing values per sample", html))
    }
  )
})
