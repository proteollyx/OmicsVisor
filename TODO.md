# TODO

Status as of v1.1.0 (2026-09-01). Items closed by that release are marked with
where they were addressed.

## Open

### Documentation (in-app)
The README now covers most of these; the in-app **Documentation** tab still
repeats the older text and should be brought in line.

- [ ] Show a minimum example input table in-app when no data is loaded
- [ ] Explain what the Intensity column regex is for, in-app
- [ ] Explain what donut plots are useful for
- [ ] Add the "vector graphics can be edited downstream" note next to the PDF
      export buttons

### Features
- [ ] Volcano / Volcano Printer: lock the adj.P selection to the chosen logFC
      column so it cannot be changed independently
- [ ] Histograms of adjusted p-values
- [ ] Option to impute on the fly for the matrix-based plots
- [ ] Barplot, or crossbar + points
- [ ] EasyPubPlot export
- [ ] Protein lists: Crapome, kinases, common contaminants
- [ ] Combine data frames from several experiments
- [ ] QC views: all-by-all logFC correlation, Gingras-style t-SNE map
- [ ] Login / usage statistics

### Known rough edges
- [ ] ID List Generator: input box is too small (layout)
- [ ] Plotly point labels do not stay pinned when the plot is re-rendered
- [ ] PTM data is not first-class: site tables load and the comparison views
      work, but nothing is PTM-aware (no localisation filter, no protein
      roll-up)

## Closed in v1.1.0

- [x] How to cite? — `CITATION.cff` plus a Citation section in the README
- [x] Explain how the input data frame should look — README "Input format",
      with a minimal worked example table
- [x] Explain why and how the tool is ID-driven — README intro and input format
- [x] Explain that the format is not built for PTMs yet — README "Notes and
      current limitations"
- [x] Disclaimer about PCA with non-imputed data / NAs — README limitations,
      and the PCA module already reports how many features it dropped
- [x] 1D enrichment finite-value filter — `one_d_enrichment()` filters with
      `x <- x[is.finite(x)]` before splitting into `xi`/`xb`, so both are
      already finite; covered by a regression test

## Closed earlier

- [x] Volcano: select a comparison and have logFC/adjP follow automatically
- [x] Same for the Volcano Printer module
- [x] Copy IDs from the left side, right side, or both at once
- [x] Select Label Columns for the heatmap, as in the volcano module
- [x] Donut: show the number of IDs selected at the given cutoffs
- [x] Boxplot/Violin: select-all columns button
- [x] Jitter or beeswarm, mutually exclusive
- [x] Volcano Printer refresh of selected IDs — obsolete; ID selection is a
      manual text input, so there is nothing to refresh
- [x] "Select Row Name Column" — replaced by the multi-select
      "Select Label Columns for Row Names"
