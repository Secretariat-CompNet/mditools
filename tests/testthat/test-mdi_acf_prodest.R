library(data.table)

make_panel <- function(n_firms = 30, n_periods = 4) {
  set.seed(42)
  n <- n_firms * n_periods
  data.table(
    id   = rep(seq_len(n_firms), each = n_periods),
    year = rep(seq(2000L, length.out = n_periods), times = n_firms),
    y    = rnorm(n, 5, 1),
    l    = rnorm(n, 3, 0.5),
    k    = rnorm(n, 4, 0.5),
    m    = rnorm(n, 2, 0.5)
  )
}

test_that("returns a data.table with expected columns", {
  skip_on_cran()
  DT     <- make_panel()
  result <- mdi_acf_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
                        id = "id", time = "year", degree = 2,
                        TFP_demeaned = FALSE, Omega_estimates = FALSE)
  expect_s3_class(result, "data.table")
  expect_true(all(c("id", "year", "tfp", "el_l", "el_k", "NumObs") %in% names(result)))
})

test_that("TFP_demeaned = TRUE adds TFP_demeaned column", {
  skip_on_cran()
  DT     <- make_panel()
  result <- mdi_acf_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
                        id = "id", time = "year", degree = 2,
                        TFP_demeaned = TRUE, Omega_estimates = FALSE)
  expect_true("TFP_demeaned" %in% names(result))
})

test_that("Omega_estimates = TRUE adds g_b columns", {
  skip_on_cran()
  DT     <- make_panel()
  result <- mdi_acf_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
                        id = "id", time = "year", degree = 2,
                        TFP_demeaned = FALSE, Omega_estimates = TRUE)
  expect_true(all(c("g_b_slopes", "g_b_intercept") %in% names(result)))
})

test_that("TFP_minuend = 'y' runs without error", {
  skip_on_cran()
  DT     <- make_panel()
  result <- mdi_acf_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
                        id = "id", time = "year", degree = 2,
                        TFP_minuend = "y", TFP_demeaned = FALSE,
                        Omega_estimates = FALSE)
  expect_s3_class(result, "data.table")
  expect_true("tfp" %in% names(result))
})

test_that("error on non-data.table input", {
  expect_error(
    mdi_acf_prodest(as.data.frame(make_panel()), y = "y", endog = "l",
                exog = "k", instr = "m", id = "id", time = "year"),
    "'DT' must be a data.table"
  )
})

test_that("error on invalid spec", {
  DT <- make_panel()
  expect_error(
    mdi_acf_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
                id = "id", time = "year", spec = "translog"),
    "'spec' must be"
  )
})

test_that("error on invalid TFP_minuend", {
  DT <- make_panel()
  expect_error(
    mdi_acf_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
                id = "id", time = "year", TFP_minuend = "bad"),
    "should be one of"
  )
})