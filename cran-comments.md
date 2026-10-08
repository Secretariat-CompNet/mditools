## Update

This is an update from 0.1.0 to 0.1.1: bug fixes (lower memory use in
`mdi_aggregate()`, hierarchy level ordering in `mdi_hier_apply()`, confidence
intervals in `mdi_regress()`, a rewrite of `mdi_make_conc()` that removes a
join failure on large concordances), Oracle import in `mdi_import_data()`
(suggested packages only), and a few exported helpers for packages that
build on mditools. See NEWS.md.

## R CMD check results

<!-- fill in from the final runs -->
0 errors | 0 warnings | 0 notes

## Check environments

<!-- fill in -->
- macOS (x86_64), R 4.6.0
- Windows (R-devel), via `devtools::check_win_devel()`
- Windows (R-release), via `devtools::check_win_release()`
- GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## Notes on suggested packages

- `arrow` is only used in `mdi_import_data()` behind
  `requireNamespace("arrow", quietly = TRUE)`; the parquet tests use
  `skip_if_not_installed("arrow")`.
- `RODBC` and `askpass` are only used in `mdi_import_data()` with
  `format = "oracle"`, behind `requireNamespace()`; no test needs a database.

## Reverse dependencies

There are no reverse dependencies on CRAN.
