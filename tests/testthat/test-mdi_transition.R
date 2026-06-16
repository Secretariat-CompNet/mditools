library(data.table)

make_transition_dt <- function() {
  data.table(
    firmid = c("A", "A", "A", "B", "B", "B"),
    year   = c(2010L, 2011L, 2012L, 2010L, 2011L, 2012L),
    nace   = c("C10", "C10", "C20", "D30", "D40", "D40")
  )
}

test_that("error on non-data.table input", {
  dt <- as.data.frame(make_transition_dt())
  expect_error(
    mdi_transition(dt, "firmid", "year", "nace"),
    "'DT' must be a data.table"
  )
})

test_that("error on non-character id", {
  dt <- make_transition_dt()
  expect_error(
    mdi_transition(dt, 1L, "year", "nace"),
    "must be a non-empty character string"
  )
})

test_that("error on missing column", {
  dt <- make_transition_dt()
  expect_error(
    mdi_transition(dt, "firmid", "year", "nosuchcol"),
    "columns not found"
  )
})

test_that("returns a data.table with four columns", {
  dt <- make_transition_dt()
  result <- mdi_transition(dt, "firmid", "year", "nace")
  expect_s3_class(result, "data.table")
  expect_named(result, c("old_code", "new_code", "year_shifted", "N"))
})

test_that("counts switches correctly", {
  dt <- make_transition_dt()
  result <- mdi_transition(dt, "firmid", "year", "nace")
  # A: C10->C20 in 2011; B: D30->D40 in 2010
  expect_equal(nrow(result), 2L)
  switch_a <- result[old_code == "C10" & new_code == "C20"]
  expect_equal(nrow(switch_a), 1L)
  expect_equal(switch_a$N, 1L)
})

test_that("no switches returns zero rows", {
  dt <- data.table(
    firmid = c("A", "A", "B", "B"),
    year   = c(2010L, 2011L, 2010L, 2011L),
    nace   = c("C10", "C10", "D30", "D30")
  )
  result <- mdi_transition(dt, "firmid", "year", "nace")
  expect_equal(nrow(result), 0L)
})

test_that("same switch by multiple firms is counted once with N > 1", {
  dt <- data.table(
    firmid = c("A", "A", "B", "B"),
    year   = c(2010L, 2011L, 2010L, 2011L),
    nace   = c("C10", "C20", "C10", "C20")
  )
  result <- mdi_transition(dt, "firmid", "year", "nace")
  expect_equal(nrow(result), 1L)
  expect_equal(result$N, 2L)
})