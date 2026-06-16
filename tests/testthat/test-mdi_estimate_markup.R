library(data.table)

make_markup_dt <- function() {
  data.table(
    oe_l = c(0.6, 0.7, 0.5),
    nq   = c(100, 200, 150),
    nm   = c(50,  80,  60)
  )
}

test_that("returns a data.table with markup column", {
  result <- mdi_estimate_markup(make_markup_dt())
  expect_s3_class(result, "data.table")
  expect_true("markup" %in% names(result))
  expect_equal(nrow(result), 3L)
})

test_that("calculation is correct", {
  DT <- data.table(oe_l = 0.6, nq = 100, nm = 50)
  result <- mdi_estimate_markup(DT)
  expect_equal(result$markup, 0.6 * 100 / 50)
})

test_that("custom column names work", {
  DT <- data.table(my_oe = 0.5, my_rev = 200, my_cost = 100)
  result <- mdi_estimate_markup(DT, oe = "my_oe", rev_col = "my_rev",
                            input_cost = "my_cost")
  expect_equal(result$markup, 0.5 * 200 / 100)
})

test_that("does not mutate the input DT", {
  DT <- make_markup_dt()
  cols_before <- copy(names(DT))
  mdi_estimate_markup(DT)
  expect_equal(names(DT), cols_before)
})

test_that("error on non-data.table input", {
  expect_error(
    mdi_estimate_markup(list(oe_l = 1, nq = 2, nm = 1)),
    "'DT' must be a data.table"
  )
})

test_that("error when oe is not a string", {
  expect_error(
    mdi_estimate_markup(make_markup_dt(), oe = 1L),
    "must be a non-empty character string"
  )
})

test_that("error when column is missing from DT", {
  DT <- data.table(oe_l = 0.6, nq = 100)
  expect_error(
    mdi_estimate_markup(DT),
    "columns not found"
  )
})