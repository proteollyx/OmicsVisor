# Contributing to OmicsVisor

Thanks for taking a look. OmicsVisor is a single-author research tool, so the
process is deliberately light — but the two rules below are what keep releases
reproducible.

## Getting set up

```r
install.packages("renv")
renv::restore()      # installs the exact package versions in renv.lock
shiny::runApp()
```

Requires R >= 4.2 (developed and tested on R 4.6).

## Running the tests

```sh
Rscript tests/testthat.R
```

The suite uses `shiny::testServer()`, so it exercises every module's reactive
graph without a browser. It must be green before any commit that touches app
code.

To additionally check the app against real result tables — which are never
committed to this repository — point the manual harness at a directory:

```sh
Rscript tests/manual/real_data_smoke.R /path/to/projects
# or:  OV_TEST_DATA=/path/to/projects Rscript tests/manual/real_data_smoke.R
```

It scans recursively for `*QQ_Results*.xlsx`, drives every module against each
one, and prints any error or unexpected warning.

## Adding a test

Fixtures live in `tests/testthat/helper-omicsvisor.R`. `sim_omics()` builds a
well-formed table; the `sim_*` variants cover the awkward cases (missing
values, duplicate IDs, character intensity columns, non-syntactic column names,
all-NA columns, infinite values, single rows, no comparisons, no intensity
columns). Wrap any of them in `ov_bundle()` to get the list that `app.R` hands
to each module.

Every bug fix should come with a test that fails before the fix.

## Releasing

`version.R` is the single source of truth. `tests/testthat/test-version.R`
fails if `CHANGELOG.md` or `CITATION.cff` disagrees with it, so the three
cannot drift apart.

1. Bump `ov_version` and `ov_release_date` in `version.R`.
2. Add the matching section at the top of `CHANGELOG.md`, using the exact
   heading form `## [<version>] - <YYYY-MM-DD>`.
3. Update `version:` and `date-released:` in `CITATION.cff`.
4. `Rscript tests/testthat.R` — must pass.
5. Commit, then:
   ```sh
   git tag -a v<version> -m "OmicsVisor v<version>"
   git push && git push --tags
   ```
6. Publish a GitHub Release against the tag, using the CHANGELOG section as the
   release notes.

### Which number to bump

OmicsVisor follows [Semantic Versioning](https://semver.org):

| Bump  | When |
|-------|------|
| MAJOR | the expected input format or a module contract changes incompatibly |
| MINOR | a new module or capability, or a backwards-compatible behaviour change |
| PATCH | bug fixes and documentation only |

## Reporting a bug

Open an issue with the module name, what you did, what you expected, and what
happened. A minimal table that reproduces it helps enormously — please strip
any unpublished project data first.
