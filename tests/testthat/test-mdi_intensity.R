library(data.table)

make_intensity_dt <- function() {
  set.seed(42)
  n <- 60L
  data.table(
    firmid = seq_len(n),
    year   = sample(2010:2012, n, replace = TRUE),
    bool1  = sample(0L:1L, n, replace = TRUE),
    bool2  = sample(0L:1L, n, replace = TRUE),
    cont1  = rnorm(n),
    cont2  = rnorm(n)
  )
}

test_that("error on non-data.table input", {
  dt <- as.data.frame(make_intensity_dt())
  expect_error(
    mdi_intensity(dt, "firmid", "bool1", c("cont1", "cont2"), "year"),
    "'DT' must be a data.table"
  )
})

test_that("error on non-character uniqdim", {
  dt <- make_intensity_dt()
  expect_error(
    mdi_intensity(dt, 1L, "bool1", c("cont1", "cont2"), "year"),
    "must be a non-empty character vector"
  )
})

test_that("error on empty boollist", {
  dt <- make_intensity_dt()
  expect_error(
    mdi_intensity(dt, "firmid", character(0), c("cont1", "cont2"), "year"),
    "must be a non-empty character vector"
  )
})

test_that("error on empty contlist", {
  dt <- make_intensity_dt()
  expect_error(
    mdi_intensity(dt, "firmid", "bool1", character(0), "year"),
    "must be a non-empty character vector"
  )
})

test_that("error on non-character fe", {
  dt <- make_intensity_dt()
  expect_error(
    mdi_intensity(dt, "firmid", "bool1", c("cont1", "cont2"), 1L),
    "must be a non-empty character vector"
  )
})

test_that("returns data.table keyed on uniqdim with intens_probit", {
  skip_on_cran()
  dt <- make_intensity_dt()
  result <- mdi_intensity(dt, "firmid", "bool1", c("cont1", "cont2"), "year")
  expect_s3_class(result, "data.table")
  expect_true("intens_probit" %in% names(result))
  expect_true("firmid" %in% names(result))
})

test_that("intens_probit is in (0, 1) for single boolean", {
  skip_on_cran()
  dt <- make_intensity_dt()
  result <- mdi_intensity(dt, "firmid", "bool1", c("cont1", "cont2"), "year")
  vals <- result$intens_probit[!is.na(result$intens_probit)]
  expect_true(all(vals > 0 & vals < 1))
})

test_that("output rows match input unique uniqdim", {
  skip_on_cran()
  dt <- make_intensity_dt()
  result <- mdi_intensity(dt, "firmid", c("bool1", "bool2"),
                      c("cont1", "cont2"), "year")
  expect_equal(nrow(result), data.table::uniqueN(dt$firmid))
})