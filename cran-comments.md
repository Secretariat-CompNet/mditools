## Package summary

`mditools` supports the full analysis pipeline for researchers working with
firm-level microdata. It includes data tools for panel preparation (import,
outlier detection, classification harmonization), analytical methods (production
function estimation via ACF, LP, OLS, WDRG, CS, and DPGMM; capital stock
measurement via PIM; markups, intensity measures, distributions, regression,
and clustering), and disclosure tools for tagging aggregated outputs with
dominance and observation counts before publication.

## Check environments

- macOS Sequoia 15.7 (x86_64), R 4.6.0 — 0 errors | 0 warnings | 0 notes
- Windows (R-devel, 2026-06-15 r90156), via `devtools::check_win_devel()` — 0 errors | 0 warnings | 1 note
- Windows (R-release 4.5.3), via `devtools::check_win_release()` — 0 errors | 0 warnings | 1 note
- Windows (R-oldrel 4.5.3), via `devtools::check_win_oldrelease()` — 0 errors | 0 warnings | 1 note

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

The single NOTE on Windows contains three items:

1. **"Possibly misspelled words: Microdata, microdata"** — these are intentional domain
   terms (microdata = firm-level record data), not spelling errors.

2. **"Possibly invalid URLs (404)"** — the GitHub repository
   `https://github.com/Secretariat-CompNet/mditools` was private at check time.
   It has been made public prior to submission and the URLs now resolve correctly.

3. **"Possibly invalid file URI: LICENSE"** — a relative `LICENSE` link in README.md
   that does not resolve outside the repository context. Removed before submission.

## Notes on suggested packages

- `arrow` is listed in `Suggests` and is only used in `mdi_import_data()` behind
  a `requireNamespace("arrow", quietly = TRUE)` guard. Tests for the parquet
  format use `skip_if_not_installed("arrow")` and are skipped when `arrow` is
  not available.

## Reverse dependencies

This is the first CRAN submission of this package. There are no reverse
dependencies.