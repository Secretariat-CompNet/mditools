library(data.table)

make_disclos_dt <- function() {
  data.table(
    nace   = rep(c("A", "B"), each = 5),
    year   = rep(2020L, 10),
    emp    = c(10, 20, 5, 15, 8, 12, 25, 6, 14, 9),
    firmid = 1:10
  )
}

test_that("domVar='var' returns data.table with domPerc and NumObs", {
  DT <- make_disclos_dt()
  result <- mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
                        bygroups = c("nace", "year"), var_list = "emp")
  expect_s3_class(result, "data.table")
  expect_true(any(grepl("domPerc", names(result))))
  expect_true("NumObs" %in% names(result))
})

test_that("domVar='var' returns one row per group", {
  DT <- make_disclos_dt()
  result <- mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
                        bygroups = c("nace", "year"), var_list = "emp")
  expect_equal(nrow(result), 2L)
})

test_that("domPerc values are between 0 and 1", {
  DT <- make_disclos_dt()
  result <- mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
                        bygroups = "nace", var_list = "emp")
  domcols <- grep("domPerc", names(result), value = TRUE)
  expect_true(all(result[, domcols, with = FALSE] >= 0 &
                    result[, domcols, with = FALSE] <= 1,
                  na.rm = TRUE))
})

test_that("residual formula produces different domPerc than top_share", {
  DT <- make_disclos_dt()
  res_top <- mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
                        bygroups = "nace", var_list = "emp",
                        dom_formula = "top_share")
  res_res <- mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
                        bygroups = "nace", var_list = "emp",
                        dom_formula = "residual")
  expect_false(identical(res_top$domPerc_emp, res_res$domPerc_emp))
})

test_that("count_firms = TRUE adds NumFirms and NumEnt to output", {
  DT <- make_disclos_dt()
  DT[, entid := firmid]
  result <- mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
                        bygroups = "nace", var_list = "emp",
                        count_firms = TRUE)
  expect_true(all(c("NumFirms", "NumEnt") %in% names(result)))
  expect_true(all(result$NumFirms >= 1L))
})

test_that("count_firms = TRUE stops when firm_col absent", {
  DT <- make_disclos_dt()
  expect_error(
    mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
                bygroups = "nace", var_list = "emp",
                count_firms = TRUE),
    "columns not found in"
  )
})

test_that("error on non-data.table input", {
  df <- as.data.frame(make_disclos_dt())
  expect_error(
    mdi_disclose_crit(df, domVar = "var", domNr = 2L,
                bygroups = "nace", var_list = "emp"),
    "'DT' must be a data.table"
  )
})

test_that("error on invalid domVar", {
  DT <- make_disclos_dt()
  expect_error(
    mdi_disclose_crit(DT, domVar = "badvar", domNr = 2L, bygroups = "nace"),
    "'domVar' must be"
  )
})

test_that("error when domVar='emp' not in DT and NSI_MD_conc not supplied", {
  DT <- make_disclos_dt()[, emp := NULL]
  expect_error(
    mdi_disclose_crit(DT, domVar = "emp", domNr = 2L, bygroups = "nace"),
    "'NSI_MD_conc' must be supplied"
  )
})

test_that("error when domVar is not a string", {
  DT <- make_disclos_dt()
  expect_error(
    mdi_disclose_crit(DT, domVar = 123, domNr = 2L, bygroups = "nace"),
    "must be a non-empty character string"
  )
})

test_that("error when bygroups is not a character vector", {
  DT <- make_disclos_dt()
  expect_error(
    mdi_disclose_crit(DT, domVar = "var", domNr = 2L, bygroups = 1L,
                var_list = "emp"),
    "must be a non-empty character vector"
  )
})