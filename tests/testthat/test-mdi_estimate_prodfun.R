library(data.table)

make_prodfun_dt <- function() {
  set.seed(1)
  n <- 200
  data.table(
    firmid = rep(1:50, each = 4),
    year   = rep(2000:2003, times = 50),
    sector = rep(c("A", "B"), each = 100),
    y      = rnorm(n, 5, 1),
    l      = rnorm(n, 3, 0.5),
    k      = rnorm(n, 4, 0.5),
    m      = rnorm(n, 2, 0.5)
  )
}

test_that("ols method returns a data.table with expected columns", {
  skip_on_cran()
  result <- mdi_estimate_prodfun(
    make_prodfun_dt(), methods = "ols",
    y = "y", endog = "l", exog = "k",
    id = "firmid", time = "year", bygroup = "sector",
    verbose = FALSE
  )
  expect_s3_class(result, "data.table")
  expect_true(all(c("sector", "method", "firmid", "year", "tfp",
                    "el_l", "el_k", "NumObs") %in% names(result)))
})

test_that("method column contains the method name", {
  skip_on_cran()
  result <- mdi_estimate_prodfun(
    make_prodfun_dt(), methods = "ols",
    y = "y", endog = "l", exog = "k",
    id = "firmid", time = "year", bygroup = "sector",
    verbose = FALSE
  )
  expect_true(all(result$method == "ols"))
})

test_that("results are returned for each bygroup level", {
  skip_on_cran()
  result <- mdi_estimate_prodfun(
    make_prodfun_dt(), methods = "ols",
    y = "y", endog = "l", exog = "k",
    id = "firmid", time = "year", bygroup = "sector",
    verbose = FALSE
  )
  expect_true(all(c("A", "B") %in% result$sector))
})

test_that("error on non-data.table/data.frame input", {
  expect_error(
    mdi_estimate_prodfun(
      list(), methods = "ols",
      y = "y", endog = "l", exog = "k",
      id = "firmid", time = "year", bygroup = "sector"
    ),
    "'DT' must be a data.table or data.frame"
  )
})

test_that("error on unknown method", {
  expect_error(
    mdi_estimate_prodfun(
      make_prodfun_dt(), methods = "bad_method",
      y = "y", endog = "l", exog = "k",
      id = "firmid", time = "year", bygroup = "sector"
    ),
    "Unknown method"
  )
})

test_that("error when instr missing for acf/lp/wdrg", {
  expect_error(
    mdi_estimate_prodfun(
      make_prodfun_dt(), methods = "acf",
      y = "y", endog = "l", exog = "k",
      id = "firmid", time = "year", bygroup = "sector"
    ),
    "`instr` must be provided"
  )
})

test_that("error when methods is not a character vector", {
  expect_error(
    mdi_estimate_prodfun(
      make_prodfun_dt(), methods = 123L,
      y = "y", endog = "l", exog = "k",
      id = "firmid", time = "year", bygroup = "sector"
    ),
    "must be a non-empty character vector"
  )
})

test_that("error when y is not a string", {
  expect_error(
    mdi_estimate_prodfun(
      make_prodfun_dt(), methods = "ols",
      y = 123L, endog = "l", exog = "k",
      id = "firmid", time = "year", bygroup = "sector"
    ),
    "must be a non-empty character string"
  )
})

test_that("error when endog is not a character vector", {
  expect_error(
    mdi_estimate_prodfun(
      make_prodfun_dt(), methods = "ols",
      y = "y", endog = 123L, exog = "k",
      id = "firmid", time = "year", bygroup = "sector"
    ),
    "must be a non-empty character vector"
  )
})

test_that("error when time is not a string", {
  expect_error(
    mdi_estimate_prodfun(
      make_prodfun_dt(), methods = "ols",
      y = "y", endog = "l", exog = "k",
      id = "firmid", time = 123L, bygroup = "sector"
    ),
    "must be a non-empty character string"
  )
})

test_that("error when bygroup is not a string", {
  expect_error(
    mdi_estimate_prodfun(
      make_prodfun_dt(), methods = "ols",
      y = "y", endog = "l", exog = "k",
      id = "firmid", time = "year", bygroup = 123L
    ),
    "must be a non-empty character string"
  )
})

test_that("returns empty data.table when all estimations fail", {
  skip_on_cran()
  DT <- make_prodfun_dt()
  DT[, y := NA_real_]
  result <- suppressMessages(mdi_estimate_prodfun(
    DT, methods = "ols",
    y = "y", endog = "l", exog = "k",
    id = "firmid", time = "year", bygroup = "sector",
    verbose = FALSE
  ))
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})