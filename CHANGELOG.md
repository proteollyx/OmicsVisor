# Changelog

All notable changes to OmicsVisor are documented in this file.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

---

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
