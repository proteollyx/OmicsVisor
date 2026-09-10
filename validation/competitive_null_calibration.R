# ─────────────────────────────────────────────────────────
# OmicsVisor - calibration of the 1D enrichment test under a correlated null
#
# The 1D Enrichment module runs a competitive Wilcoxon rank-sum test: the
# values of a set's members against the values of every other feature. That
# test assumes the features are independent. In omics they are not - members
# of a complex or a pathway are co-regulated, and co-regulation is exactly
# what makes a set worth testing.
#
# Positive within-set correlation inflates the variance of the rank-sum
# statistic under the null, so nominal p-values come out too small and the
# BH-adjusted values understate the false discovery rate. This script
# measures how much, so the module can state a number rather than a caveat.
#
# Null model: no feature is truly shifted. Set members share an exchangeable
# correlation rho via a common latent factor; background features are
# independent. Rejection at a nominal 5% should occur 5% of the time.
#
#   Rscript validation/competitive_null_calibration.R [n_sim]
#
# Runtime is a few minutes at the default 2000 simulations. The recorded
# results are committed in validation/competitive_null_calibration.md.
# ─────────────────────────────────────────────────────────

args   <- commandArgs(trailingOnly = TRUE)
n_sim  <- if (length(args)) as.integer(args[1]) else 2000L
alpha  <- 0.05
n_bg   <- 5000L                      # background features
rhos   <- c(0, 0.05, 0.1, 0.3, 0.5)
sizes  <- c(20L, 50L, 200L)

set.seed(20260910)

# One competitive Wilcoxon, exactly as the module computes it
competitive_p <- function(x_in, x_bg) {
  suppressWarnings(
    stats::wilcox.test(x_in, x_bg, alternative = "two.sided",
                       exact = FALSE, correct = FALSE)$p.value)
}

simulate_one <- function(m, rho) {
  # Exchangeable correlation rho within the set, via a shared latent factor.
  z_common <- stats::rnorm(1)
  x_in <- sqrt(rho) * z_common + sqrt(1 - rho) * stats::rnorm(m)
  x_bg <- stats::rnorm(n_bg)
  competitive_p(x_in, x_bg)
}

grid <- expand.grid(size = sizes, rho = rhos, KEEP.OUT.ATTRS = FALSE)
grid$type_I <- NA_real_
grid$median_p <- NA_real_

for (i in seq_len(nrow(grid))) {
  ps <- vapply(seq_len(n_sim),
               function(k) simulate_one(grid$size[i], grid$rho[i]),
               numeric(1))
  grid$type_I[i]   <- mean(ps < alpha)
  grid$median_p[i] <- stats::median(ps)
  cat(sprintf("size %3d  rho %.2f  ->  type-I %.3f  (nominal %.2f)\n",
              grid$size[i], grid$rho[i], grid$type_I[i], alpha))
}

grid$inflation <- grid$type_I / alpha

cat("\n")
print(grid, row.names = FALSE, digits = 3)

# Monte Carlo standard error on each rate, so the table is not over-read
mcse <- sqrt(alpha * (1 - alpha) / n_sim)
cat(sprintf("\nn_sim = %d, Monte Carlo SE on a 5%% rate = %.4f\n", n_sim, mcse))

# Committed so the figures quoted in the 1D Enrichment UI can be checked
# against the run that produced them, rather than drifting from it.
grid$n_sim <- n_sim
grid$mcse  <- mcse
utils::write.csv(grid, "validation/competitive_null_calibration.csv",
                 row.names = FALSE)
cat("\nwritten: validation/competitive_null_calibration.csv\n")

# Regenerate the write-up from the numbers just produced, so the prose can
# never quote a different run than the table it sits next to.
source("validation/render_calibration_md.R")
