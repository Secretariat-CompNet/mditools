library(data.table)

make_agg_dt <- function() {
  data.table(
    firmid = rep(1:5, each = 2),
    year   = rep(2020:2021, 5),
    nace   = rep(c("A", "B"), 5),
    emp    = c(10, 12, 5, 6, 20, 22, 8, 9, 15, 16),
    rev    = c(100, 110, 50, 55, 200, 210, 80, 85, 150, 155),
    wt     = rep(1, 10)
  )
}

test_that("sum returns data.table with correct row count", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "sum", disclosure = FALSE)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2L)
})

test_that("sum totals are correct", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "sum", disclosure = FALSE)
  expect_equal(sum(result$sum_emp), sum(DT$emp))
})

test_that("multiple agg_types produce prefixed output columns", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", c("sum", "mean"), disclosure = FALSE)
  expect_true(all(c("sum_emp", "mean_emp") %in% names(result)))
})

test_that("mrg=TRUE merges aggregates back into original DT", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "sum", mrg = TRUE, disclosure = FALSE)
  expect_equal(nrow(result), nrow(DT))
  expect_true("sum_emp" %in% names(result))
})

test_that("count_firms adds NumFirms column", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "sum",
    count_firms = TRUE, disclosure = FALSE)
  expect_true("NumFirms" %in% names(result))
  expect_equal(result$NumFirms[1], uniqueN(DT$firmid))
})

test_that("weighted mean uses weight column", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "mean",
    weight_col ="wt", disclosure = FALSE)
  expect_true("mean_emp" %in% names(result))
})

test_that("HHI returns values between 0 and 1", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "HHI", disclosure = FALSE)
  expect_true(all(result$HHI_emp >= 0 & result$HHI_emp <= 1, na.rm = TRUE))
})

test_that("error on non-data.table input", {
  df <- as.data.frame(make_agg_dt())
  expect_error(
    mdi_aggregate(df, "emp", "nace", "sum", disclosure = FALSE),
    "'DT' must be a data.table"
  )
})

test_that("error on unknown agg_type", {
  DT <- make_agg_dt()
  expect_error(
    mdi_aggregate(DT, "emp", "nace", "badtype", disclosure = FALSE),
    "unknown agg_type"
  )
})

test_that("error when bygroups is not a character vector", {
  DT <- make_agg_dt()
  expect_error(
    mdi_aggregate(DT, "emp", 123L, "sum", disclosure = FALSE),
    "must be a non-empty character vector"
  )
})

test_that("error when agg_type is not a character vector", {
  DT <- make_agg_dt()
  expect_error(
    mdi_aggregate(DT, "emp", "nace", 123L, disclosure = FALSE),
    "must be a non-empty character vector"
  )
})

test_that("error when weight is not a string", {
  DT <- make_agg_dt()
  expect_error(
    mdi_aggregate(DT, "emp", "nace", "mean", weight_col =123L, disclosure = FALSE),
    "must be a non-empty character string"
  )
})

test_that("count aggregation counts non-NA values", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "count", disclosure = FALSE)
  expect_true("count_emp" %in% names(result))
  expect_true(all(result$count_emp >= 0L))
})

test_that("median aggregation returns one row per group", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "median", disclosure = FALSE)
  expect_true("median_emp" %in% names(result))
  expect_equal(nrow(result), 2L)
})

test_that("nmiss returns zero when no missing values", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "nmiss", disclosure = FALSE)
  expect_true("nmiss_emp" %in% names(result))
  expect_true(all(result$nmiss_emp == 0L))
})

test_that("n_nonmiss mrg=TRUE adds column to original DT", {
  DT <- make_agg_dt()
  result <- mdi_aggregate(DT, "emp", "nace", "n_nonmiss", mrg = TRUE, disclosure = FALSE)
  expect_equal(nrow(result), nrow(DT))
  expect_true("n_nonmiss_emp" %in% names(result))
})
