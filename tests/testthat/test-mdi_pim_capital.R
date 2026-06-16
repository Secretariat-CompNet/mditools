library(data.table)

make_pim_dt <- function() {
  data.table(
    firmid = rep(c("F1", "F2"), each = 5),
    year   = rep(2010:2014, 2),
    K0     = c(100, NA, NA, NA, NA, 200, NA, NA, NA, NA),
    ni_tan = c(10, 12, 15, 11, 13, 20, 22, 25, 21, 23),
    d_GFCF = rep(0.08, 10)
  )
}

test_that("error on non-data.table DT", {
  dt <- as.data.frame(make_pim_dt())
  expect_error(mdi_pim_capital(dt), "'DT' must be a data.table")
})

test_that("error on invalid id argument", {
  dt <- make_pim_dt()
  expect_error(mdi_pim_capital(dt, id = 123), "'id' must be a non-empty character")
  expect_error(mdi_pim_capital(dt, id = ""),  "'id' must be a non-empty character")
})

test_that("error on missing column", {
  dt <- make_pim_dt()
  expect_error(mdi_pim_capital(dt, id = "nosuchcol"), "columns not found")
})

test_that("returns data.table with capital column appended", {
  dt     <- make_pim_dt()
  result <- mdi_pim_capital(dt)
  expect_s3_class(result, "data.table")
  expect_true("k_new" %in% names(result))
  expect_true(ncol(result) == ncol(dt) + 1L)
})

test_that("output_name parameter is respected", {
  dt     <- make_pim_dt()
  result <- mdi_pim_capital(dt, output_name = "capital_stock")
  expect_true("capital_stock" %in% names(result))
  expect_false("k_new" %in% names(result))
})

test_that("capital grows for constant investment above depreciation", {
  dt <- data.table(
    firmid = rep("F1", 5),
    year   = 2010:2014,
    K0     = c(100, NA, NA, NA, NA),
    ni_tan = rep(20, 5),
    d_GFCF = rep(0.1, 5)
  )
  result <- mdi_pim_capital(dt)
  k <- result$k_new[!is.na(result$k_new)]
  expect_true(all(diff(k) > 0))
})

test_that("error on invalid t argument", {
  dt <- make_pim_dt()
  expect_error(mdi_pim_capital(dt, t = 123L), "must be a non-empty character string")
})

test_that("error on invalid K0 argument", {
  dt <- make_pim_dt()
  expect_error(mdi_pim_capital(dt, K0 = 123L), "must be a non-empty character string")
})

test_that("error on invalid I argument", {
  dt <- make_pim_dt()
  expect_error(mdi_pim_capital(dt, I = 123L), "must be a non-empty character string")
})

test_that("error on invalid delta argument", {
  dt <- make_pim_dt()
  expect_error(mdi_pim_capital(dt, delta = 123L), "must be a non-empty character string")
})

test_that("error on invalid output_name argument", {
  dt <- make_pim_dt()
  expect_error(mdi_pim_capital(dt, output_name = 123L), "must be a non-empty character string")
})

test_that("custom id column name works", {
  dt <- data.table(
    plantid = rep(c("P1", "P2"), each = 4),
    year    = rep(2010:2013, 2),
    K0      = c(50, NA, NA, NA, 80, NA, NA, NA),
    ni_tan  = rep(10, 8),
    d_GFCF  = rep(0.05, 8)
  )
  result <- mdi_pim_capital(dt, id = "plantid")
  expect_s3_class(result, "data.table")
  expect_true("k_new" %in% names(result))
})
