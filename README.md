# OmicsVisor

**OmicsVisor** is a modular R Shiny application for interactive exploration and visualisation of differential omics analysis results (proteomics, transcriptomics, etc.).

---

## Features

| Module | Description |
|---|---|
| Data Overview | Browse the full dataset; select rows and copy IDs |
| Volcano Plot | Interactive volcano plots with point labelling and ID export |
| Donut Plot | Visualise up/down-regulated fractions per comparison |
| UpSet Plot | Multi-set intersection visualisation |
| Heatmap | Intensity heatmap with clustering, z-score scaling, and custom colour limits |
| PCA / UMAP | Dimensionality reduction with % variance labels |
| Volcano Printer | Publication-quality volcano PDF export |
| logFC Scatter Plot | Cross-comparison logFC scatter with significance colouring |
| VennDi | Two-list Venn diagram (independent of loaded data) |
| ID List Generator | Search for gene/protein IDs; supports GMT file upload |
| Boxplot / Violin | Per-feature intensity plots with beeswarm/jitter overlay |
| Regex Tool | Test and apply regex patterns to the dataset |
| Correlation | Ranked correlation of all features against a reference feature |
| GCT Export | Export logFC / t-statistic matrices in GCT format for GSEA/MSigDB |
| 1D Enrichment | Phenotype-based 1D enrichment analysis |
| Documentation | In-app usage guide |

**Global features**

- Upload `.xlsx`, `.txt`, `.tsv`, or `.csv` files (up to 443 MB)
- Global "Swap logFC" — invert selected comparisons (logFC × −1, rename x.over.y → y.over.x) and download processed data
- Sidebar collapse toggle (built-in bslib)

---

## Requirements

- R ≥ 4.2
- The app uses [`renv`](https://rstudio.github.io/renv/) to manage dependencies. All required packages are recorded in `renv.lock`.

Key packages: `shiny`, `bslib`, `plotly`, `DT`, `pheatmap`, `ggplot2`, `ggrepel`, `ggbeeswarm`, `openxlsx`, `data.table`, `dplyr`, `stringr`, `VennDiagram`, `UpSetR`, `umap`, `shinyalert`

---

## Installation

```r
# 1. Clone the repository
# git clone https://github.com/proteollyx/OmicsVisor.git

# 2. Open the project in RStudio (or set the working directory)
setwd("path/to/OmicsVisor")

# 3. Restore the package environment
install.packages("renv")
renv::restore()

# 4. Launch the app
shiny::runApp()
```

---

## Input Format

OmicsVisor expects a tabular file where:

- Each **row** is a feature (protein, gene, peptide, …)
- One column is named **`id`** — a unique identifier per feature
- Pairwise comparisons follow the naming convention:
  - `logFC_<comparison>` — log₂ fold change
  - `adj.P.Val_<comparison>` — BH-adjusted p-value
  - Optionally: `t_<comparison>`, `P.Value_<comparison>`
- Intensity columns match a user-defined regex (default: `^Imputed`)

Example column names: `id`, `Genes`, `logFC_KO.over.WT`, `adj.P.Val_KO.over.WT`, `Imputed_Sample1`, `Imputed_Sample2`

---

## Usage Notes

- The app is **ID-driven**: most modules expect an `id` column for cross-module compatibility (copy IDs from Volcano → paste into Heatmap, etc.)
- PCA and Heatmap work best with **imputed / complete** intensity matrices (no missing values)
- logFC values are assumed to be in **log₂ space** (fold change of 1 = 2× linear change)

---

## Important Notice

OmicsVisor enables rapid exploration of differential analysis results and supports generation of a wide range of figures. While output may in principle be suitable for publication, users are advised to consult their local Proteomics Technology Platform for confirmation prior to submission.

---

## Citation

If you use OmicsVisor in your research, please cite:

> Popp, O. (2026). *OmicsVisor: an interactive Shiny application for omics data visualisation*. GitHub. https://github.com/proteollyx/OmicsVisor

---

## License

MIT License — see [LICENSE](LICENSE) for details.

---

## Author

Oliver Popp — [oliver.popp@mdc-berlin.de](mailto:oliver.popp@mdc-berlin.de)
