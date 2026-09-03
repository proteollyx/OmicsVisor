# ─────────────────────────────────────────────────────────
# OmicsVisor - Version
# Author: Oliver Popp
#
# THIS FILE IS THE SINGLE SOURCE OF TRUTH FOR THE VERSION.
#
# Release checklist (see CONTRIBUTING.md for the full procedure):
#   1. Bump ov_version / ov_release_date below.
#   2. Add the matching section at the top of CHANGELOG.md, using the exact
#      heading form "## [<version>] - <YYYY-MM-DD>".
#   3. Run the test suite:  Rscript tests/testthat.R
#      test-version.R fails if this file and CHANGELOG.md disagree, so the two
#      cannot drift apart again.
#   4. Commit, then tag:    git tag -a v<version> -m "OmicsVisor v<version>"
#   5. Push:                git push && git push --tags
#
# Versioning follows Semantic Versioning 2.0.0 (https://semver.org):
#   MAJOR  incompatible change to the expected input format or module contract
#   MINOR  new module or capability, or a behaviour change that is backwards
#          compatible for existing input files
#   PATCH  bug fixes and documentation only
# ─────────────────────────────────────────────────────────

ov_version      <- "1.1.1"
ov_release_date <- "2026-09-03"
