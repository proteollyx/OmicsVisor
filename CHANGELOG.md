# Changelog

All notable changes to OmicsVisor are documented in this file.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and
OmicsVisor adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The version recorded in `version.R` is authoritative; the newest section below
must always match it (enforced by `tests/testthat/test-version.R`).

---

## [1.4.0] - 2026-09-11

**Release C of the September 2026 independent quality audit remediation**, and
the last of the three. Every item the response to the audit left open is now
closed.

> **Behaviour change.** **PCA no longer scales features to unit variance by
> default.** A saved workflow re-run after upgrading will produce a different
> PCA unless "Scale data" is re-ticked. The audit's argument for this is sound —
> autoscaling gives a protein with negligible dynamic range the same weight as a
> highly variable one, and for already-normalised log intensities centring alone
> is the better default, which is also what `prcomp()` itself does. The response
> to the audit made the change conditional on a release note and a visible
> indication of which setting produced a plot; both are now in place. The PCA
> subtitle states centring, scaling and feature count, and the manifest records
> all three.

### Fixed
- **GCT 1.3 files with column metadata were parsed wrongly** (audit OV-ENR-12).
  In GCT 1.3 the column-metadata rows sit between the column header and the
  data; the reader took the data first. Any 1.3 file that used column metadata —
  which is the point of 1.3 over 1.2 — had those metadata rows parsed as
  features, named after the metadata fields and carrying no numbers, while an
  equal number of real features fell off the end. Because non-finite scores are
  filtered downstream the phantom features simply vanished, so on a large file
  the loss was nearly invisible. Two further defects surfaced while building the
  fixtures: the 1.2 branch was selected by exact string comparison, so a version
  line padded with trailing tabs (which real writers emit) went down the 1.3
  branch and failed; and `strsplit()` discards trailing empty fields, so a blank
  final sample name was reported as a dimension mismatch rather than the blank
  it is.

### Added
- **GCT conformance fixtures** covering the audit's full list, and `ov_read_gct()`
  extracted from the module so it can be tested at all. It now checks declared
  dimensions against what is present rather than trusting them, and reports
  duplicate row IDs, duplicate and blank sample names and non-numeric cells.
- **Optional sample-metadata table** for PCA, UMAP and the heatmap (audit
  OV-UX-17). A CSV, TSV or XLSX with a `sample` column and any attributes
  (condition, batch, replicate) supersedes filename parsing when supplied; the
  heuristic remains the default because it asks nothing of the user and works
  for the standard export. Which samples matched is reported prominently —
  metadata that silently applied to only some samples would be worse than none,
  because the grouping would still look deliberate.
- **User-selectable distance and linkage** for heatmap clustering, recorded in
  the manifest (audit 2.9). Both change the dendrogram and therefore which
  groups a reader believes cluster together. Correlation distances are offered
  alongside metric ones, with zero-variance features treated as maximally
  distant rather than poisoning the distance matrix with `NA`.
- **A locked-environment CI job** (audit OV-REP-14) restoring `renv.lock` at the
  pinned R 4.6.0, so exact reproducibility is tested alongside forward
  compatibility. It verifies every restored package matches the lockfile before
  running the suite.
- **An HYE124 sign-convention oracle** (response section 3.7). Known species
  ratios — human 1:1, yeast 2:1, E. coli 1:4 — pin fold-change signs and axis
  directions end to end, and confirm `swapFC()` inverts the biology as well as
  the numbers. Deliberately not used for quantitative accuracy, which would test
  the upstream pipeline rather than this tool.

### Changed
- **Heatmap colour follows the data's meaning** (audit 2.9). A diverging palette
  asserts that its midpoint means something: true for row z-scores, where zero
  is the row mean, false for raw log intensities, where white lands wherever the
  selected samples happen to centre — so the same protein changed colour
  depending on which samples were on screen. Unscaled data now gets a sequential
  palette.
- **The Okabe-Ito palette is labelled honestly.** It is colourblind-safe because
  of the specific eight colours it contains; interpolating past eight keeps
  plots from failing but produces colours the palette never claimed to
  distinguish. The interface now says so when the group count exceeds capacity.
- **Bubble size in 1D Enrichment relabelled** from "Overlap (%)" to "Set
  coverage (%)", stating that it is measured members over original GMT members.
  A small bubble is the warning that a result rests on a fraction of the set
  rather than the pathway as annotated.
- **The gene-set database is recorded** in the manifest by filename, SHA-256 and
  set count, since MSigDB sets change between releases and the set name alone
  does not identify what was tested.

---

## [1.3.0] - 2026-09-10

**Release B of the September 2026 independent quality audit remediation.**
Where Release A fixed defects, Release B is largely about telling the user what
the app actually knows — and, just as importantly, what it does not.

One behaviour change to be aware of: **a file whose adjusted p-values fall
outside `[0, 1]` is now refused rather than loaded.** Such a file was never
valid — the usual cause is a mis-mapped column — but it would previously have
loaded and plotted convincingly. None of the 30 real result files checked
before adopting this rule trips it. This is released as a minor version
because the input contract is unchanged; only its enforcement is new.

### Added
- **Upload validation, failing closed on impossible values** (audit OV-STAT-06).
  An adjusted p-value outside `[0, 1]` is not a probability, and the realistic
  cause — a mis-mapped column, such as a t-statistic read as `adj.P.Val` —
  otherwise produces a perfectly plausible-looking volcano. Such files are now
  refused, naming the offending comparison. Bounds are inclusive: `0` arises
  from permutation tests and `1` from BH adjustment, so neither is rejected.
- **A "What was loaded" panel on the Data Overview tab.** Reports table shape,
  identifier status, comparison count, observed adjusted-p range, intensity
  columns and their missingness — plus the two things the file cannot tell the
  app: that the fold-change scale is *assumed* to be log2 and cannot be
  verified, and that the statistical method is not supplied (OV-REP-04,
  OV-NUM-08). Everything merely unusual is reported here rather than refused:
  duplicate ids, infinite fold changes and adjusted p-values of exactly zero
  all occur in legitimate exports. The reject/report split was checked against
  30 real result files, one per research group; none trips a reject condition.
- **A dimension-reduction retention panel** (audit OV-NUM-09). PCA and UMAP need
  a complete matrix, so every feature missing in any selected sample is dropped
  — routinely most of the data. A PCA of 400 features looks exactly as
  convincing as one of 8,000, and the previous warning was a notification that
  vanished after eight seconds. The panel is permanent, turns amber below 50%
  retention, and shows per-sample missingness on demand. Because the loss is
  rarely uniform, it also names the single sample whose exclusion would recover
  the most features — usually the actionable fix. It additionally compares the
  **abundance of retained against discarded features**, with a density plot,
  because the count alone cannot tell you whether the filter was benign:
  dropout in label-free proteomics is intensity-dependent, so the surviving
  proteome is usually not a random subsample.
- **A downloadable session manifest** (audit OV-REP-04). Records the app version
  and release date, the timestamp, the input file's name, size and SHA-256, the
  detected comparisons, any upload warnings, and the R and package versions. It
  is equally explicit about what it cannot certify — the search engine,
  normalisation, imputation, statistical test, correction method, fold-change
  base all happen outside the app. Claiming provenance that does not exist
  would be worse than offering none.
- **Per-module settings in the manifest.** Each module records what it was
  actually last run with — the comparison, the cutoffs *and their inclusivity*,
  PCA centring and scaling, the UMAP seed, retained and excluded feature
  counts, the within-module p-adjustment method — so an exported figure can be
  reconstructed. A module the user never opened registers nothing and is absent
  from the manifest rather than reported at defaults it was never run with. The
  git commit is recorded too when the app runs from a checkout; a Connect
  deployment has no `.git`, and the manifest says so rather than printing
  something misleading.
- **`validation/competitive_null_calibration.R`**, quantifying how far the 1D
  enrichment p-values are from calibrated (audit OV-ENR-05). The module runs a
  *competitive* Wilcoxon rank-sum test, which assumes features vary
  independently — they do not, and co-regulation within a set is precisely what
  makes it worth testing. On a null where nothing is truly shifted, the
  rejection rate at a nominal 5% is **4.7–4.9%** when features really are
  independent, but **16–54%** at a within-set correlation of only 0.05 and
  **47–81%** at 0.3, worsening with set size. The Results table now carries
  this explanation and quotes these figures; a test checks them against the
  committed simulation output, so the claim and the measurement cannot drift
  apart. Users are directed to treat the FDR column as a ranking device rather
  than a false discovery rate. Full write-up in
  `validation/competitive_null_calibration.md`.
- Tests asserting that every module identifies the `id` column **by name, never
  by position**, using a fixture whose first column is `Genes`. This behaviour
  was correct as of v1.2.0 but untested — before the UpSet fix in that release,
  a table with `id` in any other position would have had its intersections
  keyed on the first column instead, silently disagreeing with every other view.

### Fixed
- **PCA loadings could be labelled with the wrong features.** A separate
  reactive re-derived the loadings labels from the unfiltered table, mirroring
  the complete-case logic but *not* the zero-variance drop applied inside a
  scaled PCA. Whenever a scaled PCA dropped a constant feature the label vector
  was longer than the rotation matrix: usually `cbind()` errored and took the
  loadings table down, but when the two lengths happened to divide evenly it
  recycled instead and every loading was silently attributed to the wrong
  feature. Both outcomes were reproduced before fixing. The duplicated
  derivation is removed rather than patched — `dr_data()` now returns the
  matrix, its identifiers and the retention accounting together, and every
  later row filter subsets the ids in lockstep.

## [1.2.0] - 2026-09-10

**Release A of the September 2026 independent quality audit remediation.** All
eleven verified defects from the audit's confirmed-findings list are fixed, each
with a regression test. Every finding was reproduced against the source before
being changed.

One behaviour change to be aware of: **hit boundaries are now inclusive**, so
counts can shift by a small number of features sitting exactly on a cutoff. See
the first entry below.

### Changed
- **One shared definition of "hit", used by every module** (audit OV-VIZ-07).
  Each module previously implemented its own threshold check and they disagreed:
  UpSet used `>=`/`<=` while Volcano, Volcano Printer, Donut and the logFC Scatter
  used `>`/`<`. A feature sitting exactly on a cutoff was therefore a hit in one
  view and not in another. All five now call `ov_is_hit()` in
  `helper_functions.R`.
- **Boundaries are now inclusive** (`|logFC| >= cutoff`, `adj.P <= cutoff`). This
  matches how the cutoffs are described in the interface and printed in plot
  subtitles. Four of the five modules were previously exclusive, so **hit counts
  can change for features sitting exactly on a cutoff** — normally a handful at
  most, and only at the boundary itself. This is a correctness fix, not a
  silent change of behaviour.
- `ov_is_hit()` also centralises the validity guards: missing, non-finite, and
  out-of-range values never count as hits, and an adjusted p-value outside
  `[0, 1]` is treated as invalid rather than compared. The returned vector is
  never `NA`, since callers index with it.
- **Feature Correlation** now uses the same rule. It had the identical
  inconsistency inside a single expression — `adj.p.value < threshold`
  (exclusive) combined with `abs(r) >= threshold` (inclusive). The effect size
  there is a correlation coefficient rather than a fold change, but the rule is
  the same shape.
- **1D Enrichment** FDR filtering now uses the same rule, with no effect-size
  threshold.
- The Heatmap no longer describes row z-scoring as normalising intensities
  (audit OV-UX-13). For a non-coding audience that read as a substitute for
  upstream proteomics normalisation, which it is not.

- **UMAP is reproducible.** The module never set `config$random_state`, so the
  same data and settings produced a different embedding on every run and the
  exported coordinates recorded nothing about which one it was (audit
  OV-REP-04). A seed input has been added, defaulting to 1, and the seed is
  written into the exported coordinate file.
- **A constant feature no longer takes down a scaled PCA** (audit OV-NUM-08).
  `prcomp(scale. = TRUE)` cannot rescale a constant column, and a feature that
  is constant across the selected samples is routine — single-value imputation
  upstream produces them readily. Zero-variance features are now excluded from
  scaled PCA, with a count reported. Unscaled PCA keeps them.
- **Constant rows no longer produce `NaN` in a z-scored heatmap** (audit
  OV-NUM-09). `scale()` divides by the row standard deviation, so a constant
  feature yielded `NaN` in every cell, which then propagated into clustering
  and rendering. Such rows are now shown at zero with a count reported.
- **Gene queries are matched exactly instead of being compiled as regular
  expressions** (audit OV-UX-18). Searching for an identifier containing a
  regex metacharacter previously returned the wrong features *and* missed the
  right one — looking for a gene named `A+B` returned `AB` and `AAB` while
  missing `A+B` itself. Both a false positive and a false negative, silently,
  in the module that produces the ID lists every other view consumes. The audit
  rated this Low–Medium; it is treated here as high.
- **Legacy `.xls` is rejected with an actionable message instead of failing at
  read time** (audit OV-IO-11). Both `.xls` and `.xlsx` were routed to
  `openxlsx::read.xlsx`, which reads only the XLSX format, so `.xls` was
  advertised in the file picker and then failed. It has been removed from the
  accepted extensions; the error asks the user to save as `.xlsx`.

### Fixed
- **`swapFC()` destroyed statistics belonging to comparisons the user never
  selected** (audit OV-CORR-01, High). Reversing one comparison deleted the
  `t_` and `P.Value_` columns of *every* comparison in the table, and that
  table is offered for download — so it was data loss, not merely a display
  problem. The justification recorded in v1.0.0, that these were "invalidated
  by the direction swap", was also wrong: reversing a two-sided contrast
  negates the effect and the t-statistic, while the two-sided p-value and its
  adjusted counterpart are unchanged. `swapFC()` now transforms only the
  selected comparisons — negating `logFC` and `t`, leaving `P.Value` and
  `adj.P.Val` alone — and refuses to proceed if the reversed name would collide
  with a comparison that already exists.
- **UpSet presented fold-change-only sets as if they had been tested for
  significance** (audit OV-VIZ-02, High). When a `logFC_` column had no
  matching `adj.P.Val_` column the significance filter was silently skipped,
  while the interface continued to describe the sets as filtered on both. The
  module now refuses to build the plot, naming the offending columns. A
  default-off option, *"Allow fold-change-only sets (statistical significance
  NOT assessed)"*, is available for the rare case where that is genuinely
  wanted.
- **UpSet took whatever the first column happened to be as the identifier**
  (audit OV-UX-14), with no check that it was named `id`, non-missing or
  unique — duplicates silently inflated intersection sizes. It now requires a
  real `id`, consistent with every other module.
- **A zero adjusted p-value made the most significant feature vanish from the
  volcano** (audit OV-NUM-03, High). `-log10(0)` is `Inf`, and plotting
  libraries discard non-finite points, so the top hit disappeared with only a
  console warning — the plot itself looked entirely normal. Adjusted p-values
  are now floored at the smallest representable double, so an exact zero plots
  at a finite maximum. Values outside `[0, 1]` are excluded and reported with a
  visible count rather than silently producing `NaN` or a negative ordinate.
  Zero adjusted p-values are not hypothetical: they arise from numerical
  underflow and from rounding in exported tables.
- **1D Enrichment inserted a phantom all-NA pathway row** whenever an adjusted
  p-value was `NA`. `df[df$padj <= cutoff, ]` returns an all-`NA` row for every
  `NA` in the filter, which then reached the results table and both plots. This
  is the same defect class as the donut-plot NA inflation fixed in v1.0.4;
  routing the filter through `ov_is_hit()` removes it.

### Removed
- Dead commented-out ID-selection code in the Volcano module, which encoded the
  old exclusive convention and would have misled a future reader.

## [1.1.4] - 2026-09-09

Audit remediation, item A1: transparency. First of the changes arising from the
September 2026 independent quality audit.

### Added
- **The Disclaimer tab now states plainly that there is no guarantee of correct
  output.** This was previously one item in a five-point list, phrased as a warranty
  disclaimer — the form of words readers skim. It is now the first section on the page,
  says directly that there is no guarantee the tool produces correct output, asks the
  reader to verify anything that matters against the upstream analysis, and notes that
  bugs have been found before and are recorded in the changelog.
- **The Disclaimer tab now discloses that parts of the code were written with
  generative AI assistance** — OpenAI ChatGPT during early development, Anthropic
  Claude later, including the test suite and a number of bug fixes. Previously the
  application disclosed only that the *interface images* were AI-generated; the code
  disclosure existed solely in the README, which a user of the deployed application
  never sees. The disclosure is paired with the fact that every change was reviewed and
  tested and that the ~500-check test suite is public and inspectable.
- The startup notice carries both points in brief, pointing at the Disclaimer tab.

### Changed
- `ASSETS.md`: records MDC Technology Transfer's explicit confirmation that publication
  under a fully open licence is appropriate — no possible future patent, no third-party
  material, and no reason to restrict commercial use. They also noted that a
  non-commercial restriction would not have limited the MDC's own use, since the MDC
  owns the intellectual property.

## [1.1.3] - 2026-09-08

### Added
- Zenodo archiving and a citable DOI. The concept DOI
  `10.5281/zenodo.22660844` always resolves to the newest release; each release
  additionally receives its own version DOI. Added as a README badge, in the
  citation block, and as the `doi:` field of `CITATION.cff`.

### Changed
- **The startup notice now appears once per browser instead of on every page
  load.** It was fired by `observeEvent(TRUE, ..., once = TRUE)`, which is once
  per *session* — so a daily user saw it every time and had long stopped
  reading it. A `localStorage` flag now records the acknowledgement. The gate
  fails towards *showing* the notice: if storage is blocked (private browsing)
  the client reports "not seen" and the notice appears. The key is versioned so
  it can be made to reappear after a material change of wording.
- **Reworded the startup notice for a mixed audience.** The application is now
  reachable without a login, so "consult the Proteomics Technology Platform"
  meant nothing to an external user. It now names the substantive limitation
  (no quality control, filtering, imputation or statistics — those happen
  upstream), keeps the MDC-specific pointer for MDC users, and adds a line
  about uploaded data being processed on the server.
- **Rewrote the Disclaimer tab.** It described the tool as "still in its beta
  phase" and referred to the author in the third person, both written for an
  internal audience before the tool was public. It now also carries the licence,
  the Zenodo DOI, a note that the interface images sit outside the MIT grant,
  and a version footer read from `version.R` so it cannot go stale.

### Fixed
- **The Disclaimer tab displayed literal markdown.** Its numbered points were
  written as `p("1. **No Guarantee of Accuracy**: ...")`, but `p()` does not
  process markdown, so the asterisks were shown verbatim in the browser.
  Replaced with `tags$ol()` and `strong()`.

## [1.1.2] - 2026-09-08

Licensing and metadata release, following review by MDC Research Data
Management and Technology Transfer.

### Added
- `.zenodo.json`: explicit Zenodo deposition metadata, so the DOI record carries
  the correct creator affiliation, licence and description rather than being
  inferred. Zenodo prefers this file over `CITATION.cff` when both are present.

### Changed
- `LICENSE`: the copyright line now names the Max Delbrück Center alongside the
  author. Copyright in software written by MDC staff in the course of their
  duties rests with the institution; MDC Technology Transfer advised recording
  it this way.
- `ASSETS.md`: records the outcome of the September 2026 licensing review by
  MDC Research Data Management and Technology Transfer — open publication under
  MIT confirmed appropriate, AI-generated images to stay outside the licence
  grant.
- README: MDC named as the affiliation in the citation block and in a dedicated
  *Author and affiliation* section, per the MDC Rules of Good Scientific
  Practice (2023), which require the MDC to be named where intellectual work
  was developed at the MDC or using MDC resources.
- Losslessly re-encoded the favicon PNGs (401.6 KB -> 346.4 KB, -13.7%). Every
  file was verified pixel-identical to its original before replacement. Further
  reduction would require lossy colour quantisation: the source icon holds
  64,487 distinct RGBA colours, so it is photographic rather than flat, and a
  palette conversion would not be lossless.

## [1.1.1] - 2026-09-03

### Added
- `ASSETS.md`: file-by-file provenance for the image assets, recording that the
  application icon, the 1D Enrichment illustration and the six favicons derived
  from the icon were generated with OpenAI ChatGPT, are excluded from the MIT
  grant, and carry no asserted copyright. The favicons had not previously been
  identified as derived from the AI-generated icon.
- README: *Development and AI assistance* section, recording that early
  development was assisted by OpenAI ChatGPT and the v1.1.0 test suite and
  fixes by Anthropic Claude, with every change reviewed and tested by the
  author

### Changed
- Resized the two interface images to the resolution at which they are
  actually displayed (`omics_icon3.png` 1024x1024 -> 256x256,
  `hedgehog_1DE.png` 1536x1024 -> 640x426). They were 82% of the deployment
  bundle while being rendered at 40 px and ~320 px respectively; the bundle
  drops from 3.58 MB to 1.03 MB with no visible change. Full-resolution
  originals remain in the git history.
- `CITATION.cff`: author affiliation now names Technology Platform Proteomics

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
- `ov_intensity_candidates()`, `ov_intensity_prefix_groups()` and
  `ov_common_prefix()` helpers behind that hint. The hint ranks *blocks* of
  similarly-named numeric columns rather than listing loose candidates: on a
  phospho peptide-collapse export with 101 columns it proposes `^Ori.` (20)
  and `^Intensity.` (19) instead of burying them among `PTM_*`, `n_valid_*`
  and `sparse_*`

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
