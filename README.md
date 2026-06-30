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

## Generating OmicsVisor-Compatible Input with `protigy_ov`

`protigy_ov` is a companion R function (see `protigy_ov.R`) that runs pairwise differential tests on a log₂-intensity matrix and writes the result table directly in the format OmicsVisor expects. It supports moderated t-tests (limma) and Welch t-tests, with optional numeric or categorical covariates.

### Design matrix format

The design matrix is a tab-separated file with one row per sample. The first column maps sample names to columns in your data matrix; the second column assigns group labels.

**Simple two-group design**

| Run | Experiment |
|---|---|
| sample_col1 | groupA |
| sample_col2 | groupA |
| sample_col3 | groupB |
| sample_col4 | *(empty — excluded)* |

**With a numeric covariate** (e.g. passage number, batch score)

| Run | Experiment | Cofactor |
|---|---|---|
| sample_col1 | groupA | 2 |
| sample_col2 | groupB | 3 |
| sample_col3 | groupA | 2 |

**With categorical covariates** (e.g. sex and timepoint)

| Run | Experiment | sex | timepoint |
|---|---|---|---|
| sample_col1 | groupA | male | tp2 |
| sample_col2 | groupB | female | tp3 |
| sample_col3 | groupA | female | tp2 |

Rules:
- `Run` values must match **column names in the data exactly**
- Leave `Experiment` empty to exclude a sample from all tests and QC plots
- Numeric columns are used as continuous regressors; character columns are automatically factor-coded (dummy variables via `model.matrix`)
- Multiple covariates of mixed type are supported: `covariate_cols = c("sex", "timepoint")`
- Covariates are only used with `method = "modT"` (limma); a warning is issued if combined with `method = "welch"`

### Usage

```r
source("protigy_ov.R")

# All pairwise moderated t-tests, QC plots to PDF, results to file
res <- protigy_ov(
  data        = my_df,
  design      = "experiment_design.tsv",
  qc_pdf      = "QC.pdf",
  output_file = "results.txt"
)

# Welch t-test, selected comparisons only
res <- protigy_ov(
  data                = my_df,
  design              = "experiment_design.tsv",
  method              = "welch",
  select_comparisons  = c("groupB.over.groupA"),
  output_file         = "results_welch.txt"
)

# Moderated t-test with sex + timepoint as categorical covariates
res <- protigy_ov(
  data           = my_df,
  design         = "experiment_design_covar.tsv",
  covariate_cols = c("sex", "timepoint"),
  output_file    = "results_covar.txt"
)
```

### Output

The output file can be uploaded directly to OmicsVisor. Columns follow the required naming convention:

| Column | Description |
|---|---|
| `id` | Unique feature identifier |
| `logFC_B.over.A` | log₂ fold change (B = numerator group) |
| `t_B.over.A` | t-statistic |
| `P.Value_B.over.A` | Nominal p-value |
| `adj.P.Val_B.over.A` | BH-adjusted p-value (FDR) |
| *(intensity columns)* | Original column names from the data matrix |

Set the **Intensity columns** regex in the OmicsVisor sidebar to match the prefix of your intensity column names (e.g. `^Imputed`, `^Intensity`, or a run-name prefix).

---

## Usage Notes

- The app is **ID-driven**: most modules expect an `id` column for cross-module compatibility (copy IDs from Volcano → paste into Heatmap, etc.)
- PCA and Heatmap work best with **imputed / complete** intensity matrices (no missing values)
- logFC values are assumed to be in **log₂ space** (fold change of 1 = 2× linear change)

---

## Important Notice

OmicsVisor enables rapid exploration of differential analysis results and supports generation of a wide range of figures. While output may in principle be suitable for publication, users are advised to consult their local Proteomics Technology Platform for confirmation prior to submission.

---

## Changelog

### v1.0.4
- **Bug fix — Donut Plot:** Features with `NA` logFC or adj.P.Val values were falsely counted as significant hits. In R, `NA < 0.05` returns `NA`, and subsetting with `NA` inserts `NA` elements that `length()` counts. This inflated donut segment counts by 2× the number of missing-value rows per comparison (one phantom entry each for up- and downregulated). Fixed by adding `!is.na()` guards in all three filtering sites of the module, consistent with the Volcano Plot's existing handling.

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
