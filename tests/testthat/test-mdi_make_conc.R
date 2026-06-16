library(data.table)

make_conc_input <- function() {
  data.table(
    year  = c(2011L, 2011L, 2012L, 2012L),
    left  = c("A",   "B",   "A",   "C"),
    right = c("A",   "B",   "A2",  "C")
  )
}

test_that("returns a data.table in long format", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "pcc")
  expect_s3_class(result, "data.table")
  expect_true(all(c("year", "pcc", "pcc_harmonized") %in% names(result)))
})

test_that("output has one row per year-code combination", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "pcc")
  expect_true(nrow(result) > 0)
  expect_equal(anyDuplicated(result[, c("year", "pcc"), with = FALSE]), 0L)
})

test_that("harmonized codes are non-NA for all rows", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "pcc")
  expect_false(any(is.na(result$pcc_harmonized)))
})

test_that("code_name controls output column names", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "nace")
  expect_true("nace" %in% names(result))
  expect_true("nace_harmonized" %in% names(result))
})

test_that("1:1 codes map to themselves as harmonized code", {
  conc <- data.table(
    year  = c(2011L, 2012L),
    left  = c("A",   "A"),
    right = c("A",   "A")
  )
  result <- mdi_make_conc(conc, 2011:2012, "cd")
  a_rows <- result[result[["cd"]] == "A"]
  expect_true(all(a_rows$cd_harmonized == "A"))
})

test_that("error on non-data.table input", {
  expect_error(
    mdi_make_conc(as.data.frame(make_conc_input()), 2011:2012, "pcc"),
    "'conc_table' must be a data.table"
  )
})

test_that("error when required columns missing", {
  bad <- data.table(x = 1:3, y = 4:6)
  expect_error(
    mdi_make_conc(bad, 2011:2012, "pcc"),
    "columns not found"
  )
})

test_that("error on non-numeric year_list", {
  expect_error(
    mdi_make_conc(make_conc_input(), "2011", "pcc"),
    "'year_list' must be a non-empty numeric vector"
  )
})

test_that("error on non-character code_name", {
  expect_error(
    mdi_make_conc(make_conc_input(), 2011:2012, 123),
    "must be a non-empty character string"
  )
})