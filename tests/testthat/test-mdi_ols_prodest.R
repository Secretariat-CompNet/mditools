library(data.table)

make_panel <- function(n_firms = 50, n_periods = 4) {
  set.seed(11)
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
  result <- mdi_ols_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                        id = "id", time = "year", degree = 2,
                        TFP_demeaned = FALSE)
  expect_s3_class(result, "data.table")
  expect_true(all(c("id", "year", "tfp", "el_l", "el_k", "NumObs") %in%
                    names(result)))
})

test_that("TFP_demeaned = TRUE adds TFP_demeaned column", {
  skip_on_cran()
  result <- mdi_ols_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                        id = "id", time = "year", degree = 2,
                        TFP_demeaned = TRUE)
  expect_true("TFP_demeaned" %in% names(result))
})

test_that("NumObs equals number of complete-case rows", {
  skip_on_cran()
  DT     <- make_panel()
  result <- mdi_ols_prodest(DT, y = "y", endog = "l", exog = "k",
                        id = "id", time = "year", degree = 2,
                        TFP_demeaned = FALSE)
  expect_equal(result$NumObs[1], nrow(DT))
})

test_that("output is sorted by time then id", {
  skip_on_cran()
  result <- mdi_ols_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                        id = "id", time = "year", degree = 2,
                        TFP_demeaned = FALSE)
  expect_equal(result, result[order(year, id)])
})

test_that("error on non-data.table input", {
  expect_error(
    mdi_ols_prodest(as.data.frame(make_panel()), y = "y", endog = "l", exog = "k",
                id = "id", time = "year"),
    "'DT' must be a data.table"
  )
})

test_that("error on invalid spec", {
  expect_error(
    mdi_ols_prodest(make_panel(), y = "y", endog = "l", exog = "k",
                id = "id", time = "year", spec = "translog"),
    "'spec' must be"
  )
})