## Version control

### Version 0.1.1-beta

- added author
- changed icon
- implementation of pheatmap
- added ChatGPT helper: https://chatgpt.com/g/g-W6cUieQY1-omicsvisor-assistant
- maximum file size set to 443 MB

### Version 0.2.0-beta

- Applied Scaling and Clustering Explicitly to heatmap_data() and not in the pheatmap function
- Unified Display and Exported Data with Consistent heatmap_data()
- added stringr library for str_split_fixed
- Allows the usage of components from a string_split to assign annotation columns for the heatmap
- extended export function for the heatmap added
- same implementations added for PCA plot
- new colour set for PCA
- a select all checkbox was added

### Version 0.3.0-beta

- Added donut functionality

### Version 0.3.1-beta

- Updated how donut tool reads logFC and adj.P (without underscore)

### Version 0.3.2-beta
- default intensity regex changed fom "Intensity" to "^Intensity""
- updated documentation
- regex for heatmap and pca str_split components changed from "_" to "_|\\."

### Version 0.3.3-beta
- include favicon

### Version 0.3.4-beta
- heatmap colour scale - white to midpoint when scaling (z-score) applied

### Version 0.3.5-beta
- Title for the browser tab added

### Version 0.4.0-beta
- Added id list generator module and added find_genes function to helper_functions.R
- Added documentation to each module
- Data Overview now in own module

### Version 0.4.1-beta
- grep("adj.P", ...) replaced with grep("adj.P.Val" ...) in app.R to homogenise across all modules

### Version 0.5.0-beta
- added boxplot functionality

### Version 0.5.1-beta
- rotation of boxplot x axis labels

### Version 0.5.5-beta
- Added two buttons in the heatmap tool below the Select Intensity Columns input, plus the corresponding observeEvent blocks to select or deselect all matched columns.
- Introduced component_order <- reactiveVal(character(0)) to track the order in which the user checks the “Use Component X” boxes.
- When a box is checked, that component index moves to the end of the vector; when unchecked, it’s removed.
- Now generates combined labels based on the actual order in component_order, rather than iterating over components in numeric order.
- If cluster_columns is off, the code orders columns by the combined group labels (alphabetically). This ensures columns with the same group label cluster together (while respecting the order in which the user specified the components).
- Option for clustering rows included

### Version 0.5.7-beta
- added the select all intensity columns functionality also for PCAs
- volcano plots: only comparison selected -- adj.p and logFC column selected automatically
- same done for the volcano printer module
- in the volcano plot module it is now possible to copy both sides at once

### Version 0.5.9-beta
- changed the order of the modules
- heatmaps allow now user-defined row-names based on information from multiple column
- Donut plots corrected way of showing the lists including display of number of items
- selection of jitter and beeswarm in boxplots are now mutually exclusive
- Boxplot/Violin Plot Module added the select all columns as well
- layout changes in boxplot module

### Version 0.5.9.1-beta
- repaired pca pdf download

### Version 0.6.1.1-beta
- switched to pacman package loading
- Optimised layout for ID List Generator
- Added logFC scatter plot functionality
- Added gmt-file based ID list generation for the ID List generator

### Version 0.6.3.0-beta
- Added crossbar plot feature to boxplot_module.R and rewrote the whole module

### Version 0.7.0.1-beta
- Added 1D enrichment tool module
- Added gct-export module

### Version 0.7.0.2-beta
- Added line segments to the bubbles in 1D

### Version 0.7.0.3-beta
- Fix in how regular expressions are handled in boxplot module

### Version 0.8.0.0-beta
- Added UMAP plot to pca module and added more functionalities to the module
- Updated gct-export function allowing for selecting logFC and/or t_statistic columns and collapsing/exporting all columns at the same time
- Volcano plot module allows clicking points to label
- added upset plot module


### Version 0.8.0.1-beta
- Updated documentation module


### Version 0.8.0.2-beta
- Set ^Imputed. as default regex
- renv::settings$bioconductor(FALSE); renv::snapshot() after installing umap package again
- minor code optimisations in the PCA module

### Version 0.8.0.3-beta
- Bug fixes because PCA module won't work on server

### Version 0.8.0.5-beta
- Further bug fixes because PCA module won't work on server (gg_par and margin_auto are the culprits)
- cairo_pdf implemented for upsetplot download and removed again (didn't work)

### Version 0.8.0.6-beta
- fixed the PCA plotting issue by updating ggplot2

### Version 0.8.0.7-beta
- Added disclaimer to popup window
- version can be added at top of the app.R now
- some commented code removed