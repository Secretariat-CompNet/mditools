library(data.table)

make_cs_dt <- function() {
  set.seed(7)
  n <- 120
  data.table(
    id      = rep(1:30, each = 4),
    year    = rep(2000:2003, times = 30),
    nace    = rep(c("A", "B"), each = 60),
    y       = log(runif(n, 5, 50)),
    labour  = log(runif(n, 1, 10)),
    capital = log(runif(n, 2, 20))
  )
}

test_that("returns a data.table with expected columns", {
  result <- mdi_cs_prodest(make_cs_dt(), y = "y", endog = "labour",
                       exog = "capital", id = "id", time = "year",
                       bygroup = "nace")
  expect_s3_class(result, "data.table")
  expect_true(all(c("id", "year", "nace", "el_labour", "el_capital",
                     "tfp", "TFP_demeaned", "NumObs") %in% names(result)))
})

test_that("elasticities are between 0 and 1", {
  result <- mdi_cs_prodest(make_cs_dt(), y = "y", endog = "labour",
                       exog = "capital", id = "id", time = "year",
                       bygroup = "nace")
  expect_true(all(result$el_labour  >= 0 & result$el_labour  <= 1))
  expect_true(all(result$el_capital >= 0 & result$el_capital <= 1))
})

test_that("log_values = FALSE works with level inputs", {
  set.seed(7)
  n <- 120
  DT <- data.table(
    id      = rep(1:30, each = 4),
    year    = rep(2000:2003, times = 30),
    nace    = rep(c("A", "B"), each = 60),
    y       = runif(n, 5, 50),
    labour  = runif(n, 1, 10),
    capital = runif(n, 2, 20)
  )
  result <- mdi_cs_prodest(DT, y = "y", endog = "labour", exog = "capital",
                       id = "id", time = "year", bygroup = "nace",
                       log_values = FALSE)
  expect_s3_class(result, "data.table")
})

test_that("accepts a data.frame and coerces it", {
  result <- mdi_cs_prodest(as.data.frame(make_cs_dt()), y = "y",
                       endog = "labour", exog = "capital",
                       id = "id", time = "year", bygroup = "nace")
  expect_s3_class(result, "data.table")
})

test_that("returns NULL when all rows are filtered out", {
  set.seed(7)
  n <- 120
  DT <- data.table(
    id      = rep(1:30, each = 4),
    year    = rep(2000:2003, times = 30),
    nace    = rep(c("A", "B"), each = 60),
    y       = runif(n, 5, 50),
    labour  = 0,
    capital = 0
  )
  result <- mdi_cs_prodest(DT, y = "y", endog = "labour", exog = "capital",
                       id = "id", time = "year", bygroup = "nace",
                       log_values = FALSE)
  expect_null(result)
})

test_that("error on invalid DT type", {
  expect_error(
    mdi_cs_prodest(list(), y = "y", endog = "labour", exog = "capital",
               id = "id", time = "year", bygroup = "nace"),
    "'DT' must be a data.table or data.frame"
  )
})

test_that("error on missing required column", {
  DT <- make_cs_dt()
  DT[, labour := NULL]
  expect_error(
    mdi_cs_prodest(DT, y = "y", endog = "labour", exog = "capital",
               id = "id", time = "year", bygroup = "nace"),
    "DT is missing required columns"
  )
})

test_that("error when y is not length-1 character", {
  expect_error(
    mdi_cs_prodest(make_cs_dt(), y = c("y", "y"), endog = "labour",
               exog = "capital", id = "id", time = "year", bygroup = "nace"),
    "must be a non-empty character string"
  )
})
