## Package summary

`mditools` provides analysis and aggregation tools for statistical agencies
(NSIs) and users at research data centers. It includes methods for production
function estimation (ACF, LP, OLS, WDRG, CS, DPGMM), capital stock measurement
(PIM), disclosure control, data import across multiple file formats, industry
classification harmonization, and firm-level clustering. The package is designed
for use in microdata research infrastructure projects such as the Microdata
Infrastructure (MDI) initiative.

## Check environments

- macOS Sequoia 15.7 (x86_64), R 4.6.0 — local
- Windows (R-devel), via `devtools::check_win_devel()` — *to be added*
- Ubuntu (R-release), via R-hub — *to be added*

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes on suggested packages

- `arrow` is listed in `Suggests` and is only used in `mdi_import_data()` behind
  a `requireNamespace("arrow", quietly = TRUE)` guard. Tests for the parquet
  format use `skip_if_not_installed("arrow")` and are skipped when `arrow` is
  not available.

## Reverse dependencies

This is the first CRAN submission of this package. There are no reverse
dependencies.