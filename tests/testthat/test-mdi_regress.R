library(data.table)

make_reg_dt <- function(n = 100) {
  set.seed(42)
  data.table(
    y      = rnorm(n),
    x1     = rnorm(n),
    x2     = rnorm(n),
    firmid = paste0("F", sample(1:20, n, replace = TRUE)),
    entid  = paste0("E", sample(1:20, n, replace = TRUE))
  )
}

test_that("basic feols returns a data.table with expected columns", {
  DT <- make_reg_dt()
  result <- mdi_regress(DT, formula = "y ~ x1 + x2", minNumObs = 5L)
  expect_s3_class(result, "data.table")
  expect_true(all(c("coef", "Estimate", "Std. Error", "NumObs", "df",
                     "ci.lower", "ci.upper", "R2", "AdjR2") %in% names(result)))
})

test_that("multiple formulas return results for all models", {
  DT <- make_reg_dt()
  result <- mdi_regress(DT, formula = c("y ~ x1", "y ~ x2"), minNumObs = 5L)
  expect_s3_class(result, "data.table")
  expect_equal(length(unique(result$model)), 2L)
})

test_that("disclosure check filters out undersized regressions", {
  DT <- make_reg_dt(n = 3)
  expect_message(
    result <- mdi_regress(DT, formula = "y ~ x1 + x2", minNumObs = 5L),
    "does not satisfy disclosure criteria"
  )
  expect_null(result)
})

test_that("error on non-data.table DT", {
  expect_error(
    mdi_regress(as.data.frame(make_reg_dt()), formula = "y ~ x1"),
    "'DT' must be a data.table"
  )
})

test_that("error when tex = TRUE and dirOUTPUT is NULL", {
  DT <- make_reg_dt()
  expect_error(
    mdi_regress(DT, formula = "y ~ x1", tex = TRUE),
    "'dirOUTPUT' must be supplied when tex = TRUE"
  )
})

test_that("count_firms = TRUE adds NumFirms and NumEnt columns from data", {
  DT <- make_reg_dt()
  result <- mdi_regress(DT, formula = "y ~ x1 + x2",
                        count_firms = TRUE, minNumObs = 5L)
  expect_true(all(c("NumFirms", "NumEnt") %in% names(result)))
  expect_true(result$NumFirms[1] >= 1L)
})

test_that("count_firms = TRUE without firm columns and no fallback stops with error", {
  DT <- data.table(y = rnorm(50), x1 = rnorm(50))
  expect_error(
    mdi_regress(DT, formula = "y ~ x1", count_firms = TRUE, minNumObs = 5L),
    "columns not found in"
  )
})

test_that("num_firms and num_ent fallback works when firm columns absent", {
  DT <- data.table(y = rnorm(50), x1 = rnorm(50))
  result <- mdi_regress(DT, formula = "y ~ x1", count_firms = TRUE,
                        minNumObs = 5L, num_firms = 10L, num_ent = 8L)
  expect_equal(result$NumFirms[1], 10L)
  expect_equal(result$NumEnt[1], 8L)
})

test_that("iv = TRUE adds IV-specific fit statistics", {
  skip_if_not_installed("fixest")
  set.seed(1)
  n <- 100
  z <- rnorm(n)
  DT <- data.table(
    y  = z + rnorm(n),
    x1 = z + rnorm(n),
    z1 = z
  )
  result <- mdi_regress(DT, formula = "y ~ 1 | x1 ~ z1",
                        iv = TRUE, minNumObs = 5L)
  expect_s3_class(result, "data.table")
})

test_that("clustered SE accepts vcov formula string", {
  DT <- make_reg_dt()
  result <- mdi_regress(DT, formula = "y ~ x1 + x2",
                        cluster = TRUE, vcov = "~firmid",
                        minNumObs = 5L)
  expect_s3_class(result, "data.table")
})

test_that("error when formula is not a character vector", {
  DT <- make_reg_dt()
  expect_error(
    mdi_regress(DT, formula = 123L),
    "must be a non-empty character vector"
  )
})