# OmicsVisor

[![tests](https://github.com/proteollyx/OmicsVisor/actions/workflows/tests.yml/badge.svg)](https://github.com/proteollyx/OmicsVisor/actions/workflows/tests.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

**OmicsVisor** is a modular R Shiny application for interactive exploration and
visualisation of differential omics analysis results (proteomics,
transcriptomics and related data types).

You upload one results table — features in rows, pairwise comparisons and
sample intensities in columns — and get eighteen linked views over it. The app
is **ID-driven**: every module keys off the same `id` column, so a list of IDs
copied out of the Volcano Plot can be pasted straight into the Heatmap, PCA or
Volcano Printer.

---

## Features

| Module | Description |
|---|---|
| Data Overview | Browse the full dataset; select rows and copy their IDs |
| Volcano Plot | Interactive volcano plots with click-to-label and ID export |
| Donut Plot | Up/down-regulated fractions per comparison, with cross-comparison ID selection |
| UpSet Plot | Multi-set intersection view over all comparisons, with per-intersection ID export |
| Heatmap | Intensity heatmap with clustering, z-score scaling and custom colour limits |
| PCA / UMAP | Dimensionality reduction with % variance labels, scree plot and PC loadings |
| Volcano Printer | Publication-quality volcano PDF export |
| logFC Scatter Plot | Cross-comparison logFC scatter with significance colouring |
| VennDi | 2–5 list comparison (Venn for 2, UpSet for 3+); independent of the loaded data |
| ID List Generator | Map gene symbols to feature IDs; supports GMT upload (MSigDB) |
| Boxplot / Violin / Crossbar | Per-feature intensity plots with beeswarm or jitter overlay |
| Regex Tool | Test and apply regex patterns to an ID list |
| Correlation | Rank every feature by correlation against one reference feature |
| GCT Export | Export logFC / t-statistic matrices in GCT v1.2 format for GSEA/MSigDB |
| 1D Enrichment | Rank-based 1D enrichment analysis over a GCT + GMT pair |
| Documentation / Disclaimer / About | In-app usage guide, disclaimer and version info |

**Global features**

- Upload `.xlsx`, `.xls`, `.txt`, `.tsv` or `.csv` (up to 443 MB)
- **Swap logFC** — invert selected comparisons (logFC × −1, rename
  `x.over.y` → `y.over.x`, drop the invalidated t and nominal p columns) and
  download the processed table
- Configurable intensity-column regex, with presets for `^Imputed` and
  `^Intensity`
- Collapsible sidebar (bslib)

---

## Installation

```r
# 1. Clone the repository
#    git clone https://github.com/proteollyx/OmicsVisor.git

# 2. Open OmicsVisor.Rproj in RStudio, or set the working directory
setwd("path/to/OmicsVisor")

# 3. Restore the package environment
install.packages("renv")
renv::restore()

# 4. Launch
shiny::runApp()
```

**Requirements:** R ≥ 4.2 (developed and tested on R 4.6). Dependencies are
pinned in `renv.lock`.

Key packages: `shiny`, `bslib`, `plotly`, `DT`, `pheatmap`, `ggplot2`,
`ggrepel`, `ggbeeswarm`, `openxlsx`, `data.table`, `dplyr`, `tidyr`, `stringr`,
`VennDiagram`, `UpSetR`, `umap`, `shinyalert`.

---

## Input format

OmicsVisor expects a single tabular sheet where each **row** is a feature
(protein, gene, peptide, …) and columns follow this convention:

| Column | Required | Meaning |
|---|---|---|
| `id` | **yes** | Unique identifier per feature — the key every module joins on |
| `Genes` | recommended | Gene symbol(s); `;`-separated for protein groups |
| `logFC_<comparison>` | for comparison views | log₂ fold change |
| `adj.P.Val_<comparison>` | for comparison views | BH-adjusted p-value |
| `t_<comparison>`, `P.Value_<comparison>` | optional | t-statistic, nominal p-value |
| intensity columns | for matrix views | Matched by the sidebar regex (default `^Imputed`) |

A minimal valid table:

| id | Genes | logFC_KO.over.WT | adj.P.Val_KO.over.WT | Imputed.WT_01 | Imputed.KO_01 |
|---|---|---|---|---|---|
| TP53_P04637 | TP53 | 1.82 | 0.003 | 21.4 | 23.2 |
| EGFR_P00533 | EGFR | −0.44 | 0.610 | 19.8 | 19.4 |

Comparison names should use the `x.over.y` form. This is what lets the Swap
logFC feature rename a comparison when it inverts it, and it is how the
comparison dropdowns are populated. Other names still work, but Swap logFC will
skip them.

### Notes and current limitations

- **Unique IDs.** Duplicated IDs are reported where detected, but several
  modules assume uniqueness. Deduplicate upstream.
- **PCA, UMAP and clustering need complete data.** Features with any missing
  value are dropped from dimensionality reduction, and hierarchical clustering
  cannot run on a matrix with missing cells. Use imputed intensities for these
  views; the app tells you how many features it dropped.
- **logFC is assumed to be log₂.** A cutoff of 1 means a two-fold change.
- **PTM data is only partly supported.** Site-level tables load and the
  comparison views work, but nothing in the app is PTM-aware (no
  site-localisation filtering, no protein-level roll-up).
- **The intensity regex must match your column names.** If the sidebar reports
  no intensity columns, the matrix-based modules (Heatmap, PCA, Boxplot,
  Correlation) will stay empty. Set a custom pattern to match whatever prefix
  your export uses.

---

## Development

```sh
Rscript tests/testthat.R          # the automated suite (no browser needed)
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for the test layout, the manual
real-data harness, and the release procedure.

---

## Important notice

OmicsVisor enables rapid exploration of differential analysis results and
supports the generation of a wide range of figures. While output may in
principle be suitable for publication, users are advised to consult their local
Proteomics Technology Platform for confirmation prior to submission.

---

## Changelog

See [CHANGELOG.md](CHANGELOG.md).

---

## Citation

If you use OmicsVisor in your research, please cite it. Machine-readable
metadata is in [CITATION.cff](CITATION.cff).

> Popp, O. (2026). *OmicsVisor: an interactive Shiny application for exploring
> differential omics results* (v1.1.0). GitHub.
> https://github.com/proteollyx/OmicsVisor

---

## License

MIT — see [LICENSE](LICENSE).

## Author

Oliver Popp — [oliver.popp@mdc-berlin.de](mailto:oliver.popp@mdc-berlin.de)
Max Delbrück Center for Molecular Medicine (MDC), Berlin
