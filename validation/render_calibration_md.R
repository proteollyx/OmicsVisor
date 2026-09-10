# Renders validation/competitive_null_calibration.md from the committed CSV.
# Kept separate from the simulation so the write-up can be regenerated without
# re-running several minutes of Monte Carlo, and so there is exactly one code
# path from numbers to prose - nothing in the document is transcribed by hand.
#
#   Rscript validation/render_calibration_md.R

g <- utils::read.csv("validation/competitive_null_calibration.csv",
                     stringsAsFactors = FALSE)
n_sim <- g$n_sim[1]; mcse <- g$mcse[1]
sizes <- sort(unique(g$size)); rhos <- sort(unique(g$rho))

cell <- function(rho, size) {
  v <- g$type_I[g$rho == rho & g$size == size]
  sprintf("%.1f%%", 100 * v)
}
rng <- function(rho, digits = 0) {
  v <- range(g$type_I[g$rho == rho])
  # One decimal for the calibrated row, where rounding to whole percent would
  # collapse the interval to an uninformative "5-5%".
  fmt <- paste0("%.", digits, "f\u2013%.", digits, "f%%")
  sprintf(fmt, 100 * v[1], 100 * v[2])
}

lines <- c(
  "# Calibration of the 1D enrichment test under a correlated null",
  "",
  sprintf("Generated from `competitive_null_calibration.csv` (%s simulations per cell).",
          format(n_sim, big.mark = ",")),
  "Regenerate with `Rscript validation/competitive_null_calibration.R`.",
  "",
  "## What is being measured",
  "",
  "The 1D Enrichment module runs a **competitive** Wilcoxon rank-sum test: the",
  "values of a set's members against the values of every other feature. That",
  "test assumes features vary independently. In omics data they do not — members",
  "of a complex or a pathway are co-regulated, and co-regulation is exactly what",
  "makes a set worth testing in the first place.",
  "",
  "Positive within-set correlation inflates the null variance of the rank-sum",
  "statistic, so nominal p-values come out too small and the BH-adjusted values",
  "understate the false discovery rate.",
  "",
  "The null model here has **no feature truly shifted**. Set members share an",
  "exchangeable correlation ρ through a common latent factor; background features",
  "are independent. A correctly calibrated test would reject at 5% of the time",
  "regardless of ρ.",
  "",
  "## Rejection rate at a nominal 5%",
  "",
  paste0("| Within-set ρ | ", paste0(sprintf("set size %d", sizes), collapse = " | "), " |"),
  paste0("|---|", paste(rep("---", length(sizes)), collapse = "|"), "|"),
  vapply(rhos, function(r)
    paste0("| ", sprintf("%.2f", r), " | ",
           paste(vapply(sizes, function(s) cell(r, s), character(1)), collapse = " | "),
           " |"), character(1)),
  "",
  sprintf("Monte Carlo standard error on a 5%% rate is %.4f, so differences of a", mcse),
  "percentage point or less should not be read into.",
  "",
  "## What it means",
  "",
  sprintf("- With genuinely independent features (ρ = 0) the test is well calibrated: %s.",
          rng(0, digits = 1)),
  sprintf("- At a within-set correlation of only **0.05** the rejection rate is **%s**.",
          rng(0.05)),
  sprintf("- At **0.3**, plausible for a tightly co-regulated complex, it is **%s**.",
          rng(0.3)),
  "- The inflation grows with both correlation and set size, so the largest,",
  "  most biologically interesting sets are the least trustworthy.",
  "",
  "Correlations of 0.05 to 0.1 are unremarkable in real quantitative proteomics.",
  "The practical consequence is that the reported FDR is not a false discovery",
  "rate, and should not be quoted as one.",
  "",
  "## What the module does about it",
  "",
  "The Results table carries this explanation in the interface, quoting the",
  "figures above. `tests/testthat/test-enrichment-calibration.R` checks the",
  "quoted numbers against this CSV, so the claim and the measurement cannot",
  "drift apart.",
  "",
  "The recommendation given to users is to treat the FDR column as a ranking",
  "device, to weigh the `rank_biserial` effect size and `delta_median` alongside",
  "rank, and to confirm anything intended for publication with a method that",
  "models inter-feature correlation — CAMERA, or a sample-permutation test that",
  "preserves the correlation structure.",
  "",
  "## Limitations of this simulation",
  "",
  "- Exchangeable correlation within a set is a simplification; real sets have",
  "  block and hub structure that this does not reproduce.",
  "- Background features are treated as independent, which is optimistic. Real",
  "  background correlation would change the picture further.",
  "- Only the null is simulated, so this quantifies false positives, not power.",
  ""
)
writeLines(lines, "validation/competitive_null_calibration.md")
cat("written: validation/competitive_null_calibration.md\n")
