library(data.table)

make_panel <- function(n_firms = 40, n_periods = 5) {
  set.seed(99)
  n <- n_firms * n_periods
  data.table(
    id   = rep(seq_len(n_firms), each = n_periods),
    year = rep(seq(2000L, length.out = n_periods), times = n_firms),
    y    = rnorm(n, 5, 1),
    l    = rnorm(n, 3, 0.5),
    k    = rnorm(n, 4, 0.5)
  )
}

test_that("returns a data.table with expected columns", {
  skip_on_cran()
  result <- mdi_dpgmm_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                          id = "id", time = "year", TFP_demeaned = FALSE)
  expect_s3_class(result, "data.table")
  expect_true(all(c("id", "year", "tfp", "el_l", "el_k", "rho", "NumObs") %in%
                    names(result)))
})

test_that("TFP_demeaned = TRUE adds TFP_demeaned column", {
  skip_on_cran()
  result <- mdi_dpgmm_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                          id = "id", time = "year", TFP_demeaned = TRUE)
  expect_true("TFP_demeaned" %in% names(result))
})

test_that("rho is a finite numeric value", {
  skip_on_cran()
  result <- mdi_dpgmm_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                          id = "id", time = "year", TFP_demeaned = FALSE)
  expect_true(is.numeric(result$rho))
  expect_true(all(is.finite(result$rho)))
})

test_that("max_lag_Z = 1 runs without error", {
  skip_on_cran()
  result <- mdi_dpgmm_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                          id = "id", time = "year", max_lag_Z = 1,
                          TFP_demeaned = FALSE)
  expect_s3_class(result, "data.table")
})

test_that("error on non-data.table input", {
  expect_error(
    mdi_dpgmm_prodest(as.data.frame(make_panel()), y = "y", endog = "l",
                  exog = "k", id = "id", time = "year"),
    "'DT' must be a data.table"
  )
})