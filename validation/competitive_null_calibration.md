# Calibration of the 1D enrichment test under a correlated null

Generated from `competitive_null_calibration.csv` (5,000 simulations per cell).
Regenerate with `Rscript validation/competitive_null_calibration.R`.

## What is being measured

The 1D Enrichment module runs a **competitive** Wilcoxon rank-sum test: the
values of a set's members against the values of every other feature. That
test assumes features vary independently. In omics data they do not — members
of a complex or a pathway are co-regulated, and co-regulation is exactly what
makes a set worth testing in the first place.

Positive within-set correlation inflates the null variance of the rank-sum
statistic, so nominal p-values come out too small and the BH-adjusted values
understate the false discovery rate.

The null model here has **no feature truly shifted**. Set members share an
exchangeable correlation ρ through a common latent factor; background features
are independent. A correctly calibrated test would reject at 5% of the time
regardless of ρ.

## Rejection rate at a nominal 5%

| Within-set ρ | set size 20 | set size 50 | set size 200 |
|---|---|---|---|
| 0.00 | 4.9% | 4.8% | 4.7% |
| 0.05 | 16.2% | 28.8% | 53.5% |
| 0.10 | 25.9% | 42.7% | 66.5% |
| 0.30 | 47.0% | 64.0% | 81.1% |
| 0.50 | 59.3% | 72.5% | 85.7% |

Monte Carlo standard error on a 5% rate is 0.0031, so differences of a
percentage point or less should not be read into.

## What it means

- With genuinely independent features (ρ = 0) the test is well calibrated: 4.7–4.9%.
- At a within-set correlation of only **0.05** the rejection rate is **16–54%**.
- At **0.3**, plausible for a tightly co-regulated complex, it is **47–81%**.
- The inflation grows with both correlation and set size, so the largest,
  most biologically interesting sets are the least trustworthy.

Correlations of 0.05 to 0.1 are unremarkable in real quantitative proteomics.
The practical consequence is that the reported FDR is not a false discovery
rate, and should not be quoted as one.

## What the module does about it

The Results table carries this explanation in the interface, quoting the
figures above. `tests/testthat/test-enrichment-calibration.R` checks the
quoted numbers against this CSV, so the claim and the measurement cannot
drift apart.

The recommendation given to users is to treat the FDR column as a ranking
device, to weigh the `rank_biserial` effect size and `delta_median` alongside
rank, and to confirm anything intended for publication with a method that
models inter-feature correlation — CAMERA, or a sample-permutation test that
preserves the correlation structure.

## Limitations of this simulation

- Exchangeable correlation within a set is a simplification; real sets have
  block and hub structure that this does not reproduce.
- Background features are treated as independent, which is optimistic. Real
  background correlation would change the picture further.
- Only the null is simulated, so this quantifies false positives, not power.

