library(data.table)

make_jd_dt <- function() {
  data.table(
    nace = rep(c("A", "B"), each = 5),
    year = rep(2020L, 10),
    emp  = c(10, 20, 5, 15, 8, 12, 25, 6, 14, 9)
  )
}

make_hhfile <- function() {
  data.table(h_0 = c("A", "B"), h_1 = c("X", "X"))
}

test_that("returns a data.table with qmoment column", {
  result <- mdi_jointdist(
    make_jd_dt(), make_hhfile(),
    qnames = "emp", var_names = "emp", moment = "quartile",
    bygroups = c("nace", "year"), hier = "h_1",
    agg_type = "sum", disclosure = FALSE
  )
  expect_s3_class(result, "data.table")
  expect_true("qmoment" %in% names(result))
})

test_that("qmoment values are integers in expected range for quartile", {
  result <- mdi_jointdist(
    make_jd_dt(), make_hhfile(),
    qnames = "emp", var_names = "emp", moment = "quartile",
    bygroups = c("nace", "year"), hier = "h_1",
    agg_type = "sum", disclosure = FALSE
  )
  expect_true(all(result$qmoment %in% 1:4))
})

test_that("hier = ALL runs over all hierarchy levels", {
  result <- mdi_jointdist(
    make_jd_dt(), make_hhfile(),
    qnames = "emp", var_names = "emp", moment = "quartile",
    bygroups = c("nace", "year"), hier = "ALL",
    agg_type = "sum", disclosure = FALSE
  )
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0L)
})

test_that("error on invalid moment", {
  expect_error(
    mdi_jointdist(
      make_jd_dt(), make_hhfile(),
      qnames = "emp", var_names = "emp", moment = "tercile",
      bygroups = c("nace", "year"), hier = "h_1",
      agg_type = "sum", disclosure = FALSE
    ),
    "should be one of"
  )
})

test_that("error on non-data.table DT input", {
  expect_error(
    mdi_jointdist(
      as.list(make_jd_dt()), make_hhfile(),
      qnames = "emp", var_names = "emp", moment = "quartile",
      bygroups = c("nace", "year"), hier = "h_1",
      agg_type = "sum", disclosure = FALSE
    ),
    "'DT' must be a data.table"
  )
})

test_that("error on non-data.table hhfile", {
  expect_error(
    mdi_jointdist(
      make_jd_dt(), as.list(make_hhfile()),
      qnames = "emp", var_names = "emp", moment = "quartile",
      bygroups = c("nace", "year"), hier = "h_1",
      agg_type = "sum", disclosure = FALSE
    ),
    "'hhfile' must be a data.table"
  )
})

test_that("error when qnames is not a character vector", {
  expect_error(
    mdi_jointdist(
      make_jd_dt(), make_hhfile(),
      qnames = 123L, var_names = "emp", moment = "quartile",
      bygroups = c("nace", "year"), hier = "h_1",
      agg_type = "sum", disclosure = FALSE
    ),
    "must be a non-empty character vector"
  )
})

test_that("error when var_names is not a character vector", {
  expect_error(
    mdi_jointdist(
      make_jd_dt(), make_hhfile(),
      qnames = "emp", var_names = 123L, moment = "quartile",
      bygroups = c("nace", "year"), hier = "h_1",
      agg_type = "sum", disclosure = FALSE
    ),
    "must be a non-empty character vector"
  )
})

test_that("error when bygroups is not a character vector", {
  expect_error(
    mdi_jointdist(
      make_jd_dt(), make_hhfile(),
      qnames = "emp", var_names = "emp", moment = "quartile",
      bygroups = 123L, hier = "h_1",
      agg_type = "sum", disclosure = FALSE
    ),
    "must be a non-empty character vector"
  )
})

test_that("error when hier is not a string", {
  expect_error(
    mdi_jointdist(
      make_jd_dt(), make_hhfile(),
      qnames = "emp", var_names = "emp", moment = "quartile",
      bygroups = c("nace", "year"), hier = 123L,
      agg_type = "sum", disclosure = FALSE
    ),
    "must be a non-empty character string"
  )
})

test_that("error when agg_type is not a string", {
  expect_error(
    mdi_jointdist(
      make_jd_dt(), make_hhfile(),
      qnames = "emp", var_names = "emp", moment = "quartile",
      bygroups = c("nace", "year"), hier = "h_1",
      agg_type = 123L, disclosure = FALSE
    ),
    "must be a non-empty character string"
  )
})