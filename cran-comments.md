## Update

This is an update from 0.1.0 to 0.1.1: bug fixes (lower memory use in
`mdi_aggregate()`, hierarchy level ordering in `mdi_hier_apply()`, confidence
intervals in `mdi_regress()`, a rewrite of `mdi_make_conc()` that removes a
join failure on large concordances, `mdi_clustering()`'s `pam` and `mclust`
methods, and `mdi_clustering()` no longer changing the caller's random
numbers), Oracle import in `mdi_import_data()`
(suggested packages only), and a few exported helpers for packages that
build on mditools. See NEWS.md.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Check environments

- local macOS (x86_64), R 4.6.0: 0 errors | 0 warnings | 1 note, local only
  (README.md and NEWS.md can't be checked without pandoc installed)
- win-builder, R-release 4.6.1 (2026-06-24 ucrt): Status OK
- win-builder, R-devel (2026-10-05 r90641 ucrt): Status OK
- GitHub Actions: ubuntu-latest (R release and R devel), macos-latest and
  windows-latest (R release): all passing

## Notes on suggested packages

- `arrow` is only used in `mdi_import_data()` behind
  `requireNamespace("arrow", quietly = TRUE)`; the parquet tests use
  `skip_if_not_installed("arrow")`.
- `RODBC` and `askpass` are only used in `mdi_import_data()` with
  `format = "oracle"`, behind `requireNamespace()`; no test needs a database.

## Reverse dependencies

There are no reverse dependencies on CRAN.
