# Changelog

All notable changes to OmicsVisor are documented in this file.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and
OmicsVisor adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The version recorded in `version.R` is authoritative; the newest section below
must always match it (enforced by `tests/testthat/test-version.R`).

---

## [1.1.0] - 2026-09-01

First public release. Adds a full automated test suite and fixes every defect
it uncovered across the eighteen modules.

### Added
- Automated test suite (`tests/testthat/`, 360+ assertions) covering the helper
  functions, the upload/column-detection pipeline and all eighteen modules via
  `shiny::testServer()`, including simulated fixtures for missing values,
  duplicate IDs, non-numeric intensity columns, non-syntactic column names,
  all-NA columns, infinite values and single-row tables
- Real-data smoke harness (`tests/manual/real_data_smoke.R`) that drives every
  module against local `*QQ_Results*.xlsx` files and reports errors and warnings
- Version-consistency tests: `version.R`, `CHANGELOG.md` and `CITATION.cff`
  can no longer drift apart
- `LICENSE` (MIT), `CITATION.cff` and `CONTRIBUTING.md`
- GitHub Actions workflow running the test suite on every push and pull request
- `ov_read_upload()` and `ov_detect_columns()` in `helper_functions.R`: the
  upload and column-classification logic previously inlined in `app.R`, now
  testable without starting a server
- `ov_expand_palette()` and `ov_pdf_device()` helpers
- Warning on upload when the file has no `id` column, explaining that the app
  is ID-driven
- **Live feedback on the intensity-column regex.** The sidebar now reports how
  many columns the current pattern matched and, when none did, names real
  columns from the loaded file and proposes a prefix to try. Across a 29-file
  sample of real result tables, 15 matched nothing with the default `^Imputed`
  preset — Perseus and some MaxQuant exports name sample columns plainly
  (`ctr_PeC_A`) — which left the Heatmap, PCA, Boxplot and Correlation tabs
  silently empty with no indication why
- `ov_intensity_candidates()` and `ov_common_prefix()` helpers behind that hint

### Fixed
- **`%||%` returned the fallback for any vector of length > 1.** The operator
  tested `!isTRUE(nzchar(a))`, which is `FALSE` for multi-element vectors. The
  visible symptom was the GCT Export "Gene symbol column" dropdown listing
  every column in the table instead of the gene-like ones whenever the data had
  two or more gene columns (e.g. DIA-NN output with both `Genes` and
  `PG.Genes`). The definition also shadowed base R's `%||%` app-wide.
- **Boxplot: grouping crashed when all five components were selected.**
  `sapply()` simplifies to a matrix once every component returns a same-length
  vector, and `do.call(cbind, <matrix>)` then failed with "second argument must
  be a list".
- **Boxplot: duplicate IDs aborted the plot** with a tibble recycling error.
  The first matching row is now used and the ambiguity is reported.
- **Boxplot: violin plots failed silently** when each group held a single
  value, leaving a blank panel; the module now explains what to change.
- **Heatmap: non-numeric intensity columns crashed the module.** Exports that
  write `NaN` or `Filtered` into the matrix produce a character matrix, and
  `scale()` then aborted with "'x' must be numeric or complex" outside any
  `tryCatch`. Columns are coerced to numeric with a notification.
- **PCA/UMAP: sample labels were silently renamed.** `as.data.frame(lapply(...))`
  applied `make.names()`, so a column such as `Imputed 1` was plotted as
  `Imputed.1`.
- **PCA/UMAP: fixed palettes aborted the plot** with "Insufficient values in
  manual scale" as soon as there were more groups than colours (Okabe-Ito holds
  8, Brewer Set2 holds 8, Set1 holds 9). Palettes are now interpolated.
- **PCA/UMAP: group annotations desynchronised** from the plotted samples when
  a sample was dropped for having no finite values; dropped samples are now
  reported.
- **Correlation: single-row tables crashed** — `apply(..., 2, as.numeric)`
  drops to a vector, so setting row names failed.
- **ID List Generator: unmatched genes were reported as the literal `NA`.**
  `find_genes()` returns `NA` for a miss and `id[NA]` yields `NA`, which was
  pasted into the output the user copies.
- **Volcano Printer: `NA` labels appeared on the plot** for features with a
  missing logFC or adj.P value.
- **VennDi: three-or-more-list comparison was wrong when only one ID was
  shared**, because `sapply()` collapsed the membership matrix to a vector.
- **Donut Plot: the ID selection ignored the applied cutoffs.** The donuts are
  drawn from a snapshot taken on "Apply Cutoff" while the ID list read the live
  inputs, so editing a cutoff without re-applying it returned IDs that
  disagreed with the plotted counts.
- **An invalid intensity-column regex took the whole app down.** A partially
  typed pattern such as `^Imputed[` aborted the shared `data` reactive and with
  it every module; it now falls back to "no match".
- **PDF exports transliterated `≤`, `≥` and `—`.** The default `pdf()` device
  is single-byte; exports now use `cairo_pdf` where available.
- **The About tab could prevent the app from starting.** `about_ui()` read
  `CHANGELOG.md` at UI-build time with an unguarded relative path.
- Scatterplot: an all-NA comparison column no longer aborts the plot through
  `cor(use = "complete.obs")`.
- Scatterplot: when nothing clears the cutoffs — routine on real data — the
  named colour scale had no matching levels and every render warned "No shared
  levels found between `names(values)` of the manual scale and the data's
  colour values". The colour-mapped layers are now added only when they have
  data.
- An empty or header-only upload is rejected with a plain explanation instead
  of a bare `0 x 0` table propagating into every module.
- Donut Plot: a `logFC_` column with no matching `adj.P.Val_` column is shown
  as all-"Other" rather than as an empty donut.

### Changed
- Donut Plot: the hit-selection logic, previously spelled out at three separate
  call sites (which is how the v1.0.4 NA fix reached one of them but not the
  others), is now a single function used by the plots, the checkboxes and the
  ID export
- Replaced the deprecated `aes_string()` in the PCA and Scatterplot modules
- README rewritten for public release; its duplicate changelog section removed
  in favour of this file
- `.gitignore` extended to cover R session state and local result files

## [1.0.4] - 2026-06-30

### Fixed
- Donut Plot: features with `NA` logFC or adj.P.Val values were counted as
  significant hits. `NA < 0.05` is `NA`, and subsetting with `NA` inserts `NA`
  elements that `length()` counts, inflating each donut by twice the number of
  missing-value rows per comparison. Added `!is.na()` guards to the filtering
  sites in the module.

### Changed
- README: added a changelog section documenting the donut fix
- README: removed the `protigy_ov` section (it belongs with `protigy_ov.R`)

## [1.0.3] - 2026-06-29

### Added
- Sidebar: regex usage examples, and a note that a custom pattern overrides the
  dropdown selection
- Donut Plot: descriptive subtitle
- Volcano Printer: manual x/y axis limits with a clipping warning; cutoffs shown
  in the plot subtitle and in the PDF filename; comparison name used as the plot
  title
- Boxplot: manual y-axis limits with a clipping warning
- Scatterplot: cutoffs appended to the subtitle; comparison names and cutoffs
  included in the PDF filename
- Footer: browser compatibility note (tested with Chrome)

## [1.0.1] - 2026-06-04

### Added
- PCA: scree plot, and a PC loadings table with CSV download

### Changed
- All modules: standardised `btn-sm` on action and download buttons
- Boxplot: removed the "Generate Plot" button; the plot now refreshes
  reactively, with explicit `renderPlot` dimensions
- VennDi: input parser rewritten to split on comma, semicolon, space, tab or
  newline; switched to `textAreaInput`; list values are preserved when the
  number of lists changes; empty lists are reported

### Fixed
- `app.R`: `.shiny-download-link` added to the auto-width CSS rule so every
  `downloadButton` respects the bslib flex layout
- Heatmap: "invalid quartz() device size" fixed by giving `renderPlot` explicit
  width/height/res and removing `outputOptions(suspendWhenHidden)`; added
  `validate()` guards and `tryCatch`
- UpSet Plot: the "Extract IDs" dropdown no longer diverges from the plot when
  `min_set_size` or `n_intersects` change
- PCA: plot overlap fixed by wrapping `plotOutput`s in `fluidRow`/`column(12)`

## [1.0.0] - 2026-05-21

### Added
- Correlation module: ranked correlation plot against a reference feature across intensity columns
- About module
- Global "Swap logFC" feature in sidebar: invert selected comparisons (logFC × −1, rename x.over.y → y.over.x), remove t-stat and P.Value columns, download processed data
- Data Overview: row selection → copy selected IDs to clipboard
- VennDi: safe clipboard copy via message handler; empty-subset Venn diagrams now render correctly
- Heatmap: custom color scale min/max limits

### Changed
- Migrated from `shinythemes` to `bslib` (Bootstrap 5): `page_sidebar`, `card`, `navset_card_tab`
- Sidebar reorganised into grouped cards ("Data input", "Swap logFC")
- PCA axis labels now show % explained variance per selected PC
- ID List Generator: defaults to "Genes" column; added ignore-case checkbox
- Correlation module: reference feature always searched by exact `id` match
- Adj.P-value cutoff inputs across all modules: step set to 0.01
- Volcano Printer: fixed "invalid quartz() device size" error on macOS

### Fixed
- Selectize dropdown overflow and clipping across all modules
- Volcano Plot copy buttons: replaced XSS-vulnerable inline onclick with safe `sendCustomMessage`
- VennDi: Venn diagram no longer silently fails when any subset is empty

### Removed
- ChatGPT helper tool link from footer
- `shinythemes` dependency (replaced by `bslib`)
- `rsconnect` unused import

## [0.8.0.8-beta] - 2026-04-16

### Fixed
- ID selection in the volcano module generating NA values

## [0.8.0.7-beta]

### Added
- Disclaimer added to startup popup window

### Changed
- Version string now defined at the top of `app.R`

### Removed
- Various commented-out code blocks

## [0.8.0.6-beta]

### Fixed
- PCA plotting issue resolved by updating ggplot2

## [0.8.0.5-beta]

### Fixed
- Further server-side bug fixes for PCA module (`gg_par` and `margin_auto` identified as culprits)
- `cairo_pdf` tested for UpSet plot download (reverted — did not work)

## [0.8.0.3-beta]

### Fixed
- Bug fixes for PCA module not working on server

## [0.8.0.2-beta]

### Changed
- Default intensity regex changed to `^Imputed.`
- Minor code optimisations in the PCA module

### Fixed
- `renv` snapshot updated after reinstalling `umap` package

## [0.8.0.1-beta]

### Changed
- Updated documentation module

## [0.8.0.0-beta]

### Added
- UMAP plot added to PCA module
- UpSet plot module
- Volcano plot module: click points to add labels

### Changed
- GCT export updated: select logFC and/or t-statistic columns; collapse/export all columns at once
- Additional functionalities added to PCA module

## [0.7.0.3-beta]

### Fixed
- Regular expression handling in boxplot module

## [0.7.0.2-beta]

### Added
- Line segments added to bubbles in 1D enrichment plot

## [0.7.0.1-beta]

### Added
- 1D enrichment tool module
- GCT export module

## [0.6.3.0-beta]

### Added
- Crossbar plot feature in boxplot module

### Changed
- Boxplot module rewritten

## [0.6.1.1-beta]

### Added
- logFC scatter plot functionality
- GMT-file based ID list generation in ID List Generator

### Changed
- Switched to `pacman` package loading
- Optimised layout for ID List Generator

## [0.5.9.1-beta]

### Fixed
- PCA PDF download

## [0.5.9-beta]

### Added
- User-defined row names in heatmap based on multiple columns
- Select-all columns button added to Boxplot/Violin module

### Changed
- Module order updated
- Donut plots: corrected display of lists including number of items
- Jitter and beeswarm selections in boxplots made mutually exclusive
- Layout changes in boxplot module

## [0.5.7-beta]

### Added
- Select-all intensity columns for PCA
- Volcano plot: copy IDs from both sides simultaneously

### Changed
- Volcano and Volcano Printer: selecting a comparison now auto-selects logFC and adj.P columns

## [0.5.5-beta]

### Added
- Select All / Deselect All buttons for intensity columns in heatmap
- `component_order` reactive to track grouping annotation checkbox order
- Option to cluster rows in heatmap

### Changed
- Group labels now reflect the order in which components are checked
- When column clustering is off, columns are sorted alphabetically by group label

## [0.5.1-beta]

### Changed
- Boxplot x-axis labels rotated

## [0.5.0-beta]

### Added
- Boxplot module

## [0.4.1-beta]

### Fixed
- `grep("adj.P", ...)` replaced with `grep("adj.P.Val", ...)` in `app.R` for consistency across modules

## [0.4.0-beta]

### Added
- ID List Generator module with `find_genes()` helper function
- Inline documentation added to all modules
- Data Overview moved into its own module

## [0.3.5-beta]

### Added
- Browser tab title

## [0.3.4-beta]

### Changed
- Heatmap colour scale: white set as midpoint when Z-score scaling is applied

## [0.3.3-beta]

### Added
- Favicon

## [0.3.2-beta]

### Changed
- Default intensity regex changed from `Intensity` to `^Intensity`
- Heatmap and PCA: string-split component regex changed from `_` to `_|\.`

### Fixed
- Documentation updated

## [0.3.1-beta]

### Fixed
- Donut tool: reads `logFC` and `adj.P` without underscore prefix

## [0.3.0-beta]

### Added
- Donut plot module

## [0.2.0-beta]

### Added
- `stringr` library for `str_split_fixed`
- Support for using string-split components as heatmap annotation columns
- Extended heatmap export function
- Same scaling/clustering approach applied to PCA plot
- New colour palette for PCA
- Select All checkbox

### Changed
- Scaling and clustering applied explicitly in `heatmap_data()` rather than inside `pheatmap()`
- Unified display and exported data using consistent `heatmap_data()`

## [0.1.1-beta]

### Added
- Author attribution
- ChatGPT helper link: OmicsVisor Assistant
- Maximum file size set to 443 MB

### Changed
- App icon updated
- Heatmap: switched to `pheatmap`
