# Functions that depend on the NSI runtime globals (dirOUTPUT, MD_idInfo,
# CountryCode, etc.) cannot be integration-tested without that environment.
# These tests assert the functions are exported and have the expected formals,
# acting as a canary for accidental signature changes.


test_that("mdi_import_data is exported with expected arguments", {
  expect_true(is.function(mdi_import_data))
  fmls <- names(formals(mdi_import_data))
  expect_true(
    all(c("dir", "file", "format", "col_list", "char_columns") %in% fmls)
  )
})

test_that("mdi_regress is exported with expected arguments", {
  expect_true(is.function(mdi_regress))
  fmls <- names(formals(mdi_regress))
  expect_true(all(c("DT", "formula", "model", "vcov", "weights") %in% fmls))
})

test_that("mdi_estimate_prodfun is exported with expected arguments", {
  expect_true(is.function(mdi_estimate_prodfun))
  fmls <- names(formals(mdi_estimate_prodfun))
  expect_true(all(c("DT", "methods") %in% fmls))
})

test_that("mdi_disclose_crit is exported", {
  expect_true(is.function(mdi_disclose_crit))
})

test_that("mdi_make_conc is exported", {
  expect_true(is.function(mdi_make_conc))
})

test_that("mdi_hier_apply is exported", {
  expect_true(is.function(mdi_hier_apply))
})
