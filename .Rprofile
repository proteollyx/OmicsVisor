source("renv/activate.R")

# OmicsVisor depends on CRAN only: all 23 non-base packages it uses are CRAN
# packages, and none of the 117 entries in renv.lock comes from Bioconductor.
#
# Pin the repositories explicitly anyway, because this is where a corrupt
# lockfile came from. R's etc/repositories file templates the Bioconductor
# version into five BioC repository URLs, and on this R build the substitution
# yields the literal string "TRUE"
# (https://bioconductor.org/packages/TRUE/bioc). renv::snapshot() records
# getOption("repos") into renv.lock, so those unusable URLs were written into
# the lockfile, and renv::restore() then failed outright with "invalid version
# specification 'TRUE'" - meaning the environment described as pinned could not
# be reproduced at all. It went unnoticed because nothing here needs
# Bioconductor, so the repositories were never consulted.
#
# Setting this after activation keeps the defect from being re-snapshotted
# regardless of what the surrounding R installation reports. If a Bioconductor
# package is ever genuinely required, add it here with a real version.
local({
  repos <- getOption("repos")
  keep  <- !grepl("bioconductor\\.org", repos)
  options(repos = c(CRAN = "https://cloud.r-project.org", repos[keep & names(repos) != "CRAN"]))
})
