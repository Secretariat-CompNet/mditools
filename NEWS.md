# mditools 0.1.1

## Bug fixes

* `mdi_aggregate()` no longer copies its input when it only summarises it
  (`mrg = FALSE`), and reads column types in place instead of copying the
  columns. Its peak memory on a large table drops from about 2.3 to 0.6
  times the table's size.
* `mdi_hier_apply()` orders hierarchy levels by their number, so `h_10`
  comes after `h_2` (they were ordered alphabetically).
* `mdi_regress()` computes confidence intervals with the requested
  covariance estimator (`vcov`), matching the standard errors.
* `mdi_make_conc()` is rewritten: codes linked across years form one
  harmonized group (connected components), which removes the many-to-many
  join that stopped on large concordances. Each group is named after its
  smallest code in its latest year, with a `D` suffix when that year is
  before the last one. Group membership is unchanged; some group names
  differ from 0.1.0. `year_list` now defaults to the concordance's full
  year range, and a year without concordance rows is an error.

## New features

* `mdi_import_data()` reads tables from an Oracle database
  (`format = "oracle"`), through RODBC with the password asked
  interactively (askpass); both packages are suggested, not required.
* `mdi_aggregate()` and `mdi_jointdist()` take `domVar` and `domNr` for the
  dominance check, and `mdi_jointdist()` takes `minNumObs`.
* `mdi_outlier()` accepts several grouping variables in `group`.
* `check_dt()`, `check_string()`, `check_choice()`, `check_char_vec()` and
  `hier_levels()` are exported, for packages that build on mditools.

## Documentation

* Help pages use Markdown; `panel_lag()` and `panel_lag_L()` are
  documented.

# mditools 0.1.0

* First CRAN release.
