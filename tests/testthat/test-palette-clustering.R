# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: colour semantics, clustering choices, PCA scaling
# (audit 2.8 and 2.9)
#
# Three changes that alter what a reader sees rather than what the numbers
# are, which is why each is recorded in the manifest as well as tested here.
# ─────────────────────────────────────────────────────────

# ── palette semantics ───────────────────────────────────────────────────────

test_that("z-scored data gets a diverging palette with a light midpoint", {
  # The midpoint is meaningful here - it is the row mean - so blue/red reads
  # as below/above average.
  pal <- ov_heatmap_palette(scaled = TRUE, n = 101L)
  expect_length(pal, 101L)
  mid <- grDevices::col2rgb(pal[51])
  expect_true(all(mid > 200))                       # near-white centre
  expect_lt(sum(grDevices::col2rgb(pal[1])^2),
            sum(grDevices::col2rgb(pal[101])^2))    # dark blue -> firebrick
})

test_that("unscaled intensities get a sequential palette, not a diverging one", {
  # A diverging palette asserts a meaningful midpoint. On raw log intensities
  # white would land wherever the selected samples happen to centre, so the
  # same protein changes colour with the selection.
  pal <- ov_heatmap_palette(scaled = FALSE, n = 101L)
  lum <- function(c) sum(grDevices::col2rgb(c) * c(0.299, 0.587, 0.114))
  l <- vapply(pal, lum, numeric(1))
  # Monotonically darkening: order is implied, no midpoint is.
  expect_true(all(diff(l) <= 1e-6))
  expect_gt(l[1], l[101])
})

test_that("the two palettes are genuinely different", {
  expect_false(identical(ov_heatmap_palette(TRUE), ov_heatmap_palette(FALSE)))
})

# ── clustering choices ──────────────────────────────────────────────────────

test_that("every offered distance measure produces a usable dendrogram", {
  set.seed(3)
  m <- matrix(rnorm(200), 20, 10, dimnames = list(paste0("f", 1:20), paste0("s", 1:10)))
  for (d in OV_HEATMAP_DISTANCES) {
    dd <- ov_cluster_dist(m, d)
    expect_s3_class(dd, "dist")
    expect_true(all(is.finite(dd)), info = paste("non-finite distances for", d))
    expect_no_error(stats::hclust(dd, method = "complete"))
  }
})

test_that("every offered linkage works with the default distance", {
  set.seed(4)
  m <- matrix(rnorm(200), 20, 10)
  dd <- ov_cluster_dist(m, "euclidean")
  for (l in OV_HEATMAP_LINKAGES)
    expect_no_error(stats::hclust(dd, method = l))
})

test_that("correlation distance survives a zero-variance feature", {
  # Constant rows have undefined correlation. Left as NA they take hclust()
  # down; they are treated as maximally distant instead.
  m <- matrix(rnorm(100), 10, 10)
  m[3, ] <- 5
  dd <- ov_cluster_dist(m, "pearson")
  expect_true(all(is.finite(dd)))
  expect_no_error(stats::hclust(dd, method = "average"))
})

test_that("distance and linkage actually change the dendrogram", {
  # If they did not, offering them would be theatre.
  set.seed(5)
  m <- matrix(rnorm(300), 30, 10)
  a <- stats::hclust(ov_cluster_dist(m, "euclidean"), method = "complete")
  b <- stats::hclust(ov_cluster_dist(m, "manhattan"), method = "complete")
  c <- stats::hclust(ov_cluster_dist(m, "euclidean"), method = "ward.D2")
  expect_false(identical(a$merge, b$merge))
  expect_false(identical(a$merge, c$merge))
})

test_that("the heatmap records its distance, linkage and colour semantics", {
  d   <- sim_omics(n_features = 40, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(d), value = TRUE)
  seen <- new.env(); reg <- function(m, v) assign(m, v, envir = seen)

  shiny::testServer(
    heatmap_server,
    args = list(data = reactive(list(data = d, intensity_cols = int)),
                register = reg),
    {
      session$setInputs(intensity_columns = int, scale_rows = TRUE,
                        cluster_rows = TRUE, cluster_columns = TRUE,
                        dist_method = "pearson", linkage = "ward.D2",
                        use_custom_limits = FALSE)
      session$flushReact()
    }
  )
  s <- get("Heatmap", envir = seen)
  expect_equal(s$distance, "pearson")
  expect_equal(s$linkage, "ward.D2")
  expect_match(s$colour_scale, "diverging")
})

test_that("the recorded colour semantics follow the scaling setting", {
  d   <- sim_omics(n_features = 40, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(d), value = TRUE)
  seen <- new.env(); reg <- function(m, v) assign(m, v, envir = seen)

  shiny::testServer(
    heatmap_server,
    args = list(data = reactive(list(data = d, intensity_cols = int)),
                register = reg),
    {
      session$setInputs(intensity_columns = int, scale_rows = FALSE,
                        cluster_rows = FALSE, cluster_columns = FALSE,
                        use_custom_limits = FALSE)
      session$flushReact()
    }
  )
  expect_match(get("Heatmap", envir = seen)$colour_scale, "sequential")
})

# ── PCA scaling default (audit 2.8) ─────────────────────────────────────────

test_that("PCA no longer scales by default", {
  ui <- as.character(pca_ui("pca"))
  # The checkbox must not be pre-ticked.
  scale_box <- regmatches(ui, regexpr('id="pca-pca_scale"[^>]*', ui))
  expect_length(scale_box, 1)
  expect_false(grepl("checked", scale_box))
  # Centring is still on: it is the part that is unambiguously right.
  centre_box <- regmatches(ui, regexpr('id="pca-pca_center"[^>]*', ui))
  expect_true(grepl("checked", centre_box))
})

test_that("the change of default is stated in the interface", {
  expect_match(as.character(pca_ui("pca")), "v1\\.4\\.0")
})

test_that("the PCA plot states the preprocessing that produced it", {
  d   <- sim_omics(n_features = 60, groups = list(WT = 3, KO = 3))
  int <- grep("^Imputed", names(d), value = TRUE)
  shiny::testServer(
    pca_server,
    args = list(data = reactive(list(data = d, intensity_cols = int))),
    {
      session$setInputs(dr_method = "PCA", intensity_columns = int,
                        row_selection = "all", id_selection = "",
                        pca_center = TRUE, pca_scale = FALSE,
                        pca_x_pc = "PC1", pca_y_pc = "PC2",
                        point_size = 3, label_size = 3)
      sub <- create_dr_plot()$labels$subtitle
      expect_match(sub, "centred")
      expect_match(sub, "not scaled")
      expect_match(sub, "60 features")

      session$setInputs(pca_scale = TRUE)
      expect_match(create_dr_plot()$labels$subtitle, "scaled to unit variance")
    }
  )
})

test_that("the palette note appears only when groups exceed the palette", {
  int_few <- sprintf("Imputed.G%02d_01", 1:4)
  int_many <- sprintf("Imputed.G%02d_01", 1:12)
  mk <- function(int) {
    d <- data.frame(id = sprintf("P%03d", 1:40), stringsAsFactors = FALSE)
    for (c in int) d[[c]] <- rnorm(40, 20, 1)
    list(data = d, intensity_cols = int)
  }

  shiny::testServer(pca_server, args = list(data = reactive(mk(int_few))), {
    session$setInputs(dr_method = "PCA", intensity_columns = int_few,
                      row_selection = "all", id_selection = "",
                      color_scheme = "okabe", pca_center = TRUE, pca_scale = FALSE)
    expect_null(output$palette_note)
  })

  shiny::testServer(pca_server, args = list(data = reactive(mk(int_many))), {
    session$setInputs(dr_method = "PCA", intensity_columns = int_many,
                      row_selection = "all", id_selection = "",
                      color_scheme = "okabe", pca_center = TRUE, pca_scale = FALSE)
    html <- as.character(output$palette_note$html)
    expect_match(html, "12 groups exceed")
    expect_match(html, "colourblind-safe property no longer holds")
  })
})
