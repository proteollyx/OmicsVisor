# TODO

## Open

- [x] Volcano Printer: does not refresh selected IDs (obsolete — ID selection is manual text input; no auto-refresh needed)
- [ ] Select only logFC and adjust adj.p-value selection accordingly without letting user change it
- [ ] Optional blue line and disclaimer that vector graphics can be edited according to needs
- [ ] Explain what donut plots can be useful for
- [ ] ID list generator: box size increase (layout issue)
- [ ] Why does plotly not fix the label? (is this possible?)
- [ ] Explain on overview how the format of the dataframe should look, with option to display an example
- [ ] Explain why and how the tool is ID-driven and how the IDs are built
- [ ] Explain why it only works with this format — not made for PTMs yet
- [ ] Add a disclaimer why PCAs might not work with non-imputed data / NAs in the matrix
- [ ] Add histograms for adj.p
- [ ] Add a short explainer for what the Intensity column regex is for
- [ ] If no data is loaded, show how a minimum input table should look like
- [ ] How to cite?
- [x] Select Row Name Column: removed — replaced by "Select Label Columns for Row Names" multi-select
- [ ] Login tool for stats
- [ ] Option to impute for certain plots
- [ ] EasyPubPlot export functionality
- [ ] Barplot or crossbar + points?
- [ ] Implement 1D enrichment finite-value filter: `xi <- xi[is.finite(xi)]` / `xb <- xb[is.finite(xb)]`

## Planned Features

- [ ] LogFC Scatterplots — QC: All-by-all-correlation, Gingras tSNE map
- [ ] Histograms of adjusted p-values
- [ ] Protein lists: Crapome, Kinases, Common contaminants
- [ ] Combine dataframes

## Done

- [x] Volcano: select only comparison and automatically select logFC and adjP accordingly
- [x] Same for Volcano Printer Module
- [x] Copy IDs from left side, right side, or both sides at once
- [x] Select Label Columns: as in volcano plot module, also for heatmap module
- [x] Donut: show number of IDs selected with the given cutoffs
- [x] Boxplot/Violin Plot Module: add the select all columns as well
- [x] Show jitter or beeswarm (mutually exclusive)
