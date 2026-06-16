library(data.table)

make_hier_apply_data <- function() {
  hhfile <- data.table(
    h_0 = c("A1", "A2", "B1", "B2"),
    h_1 = c("A",  "A",  "B",  "B")
  )
  DT <- data.table(
    nace = c("A1", "A2", "B1", "B2"),
    year = rep(2020L, 4),
    emp  = c(10L, 20L, 15L, 25L)
  )
  list(DT = DT, hhfile = hhfile)
}

test_that("returns a data.table with aggregated values", {
  d <- make_hier_apply_data()
  result <- mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
                       bygroups = c("nace", "year"), hier = "h_1",
                       disclosure = FALSE)
  expect_s3_class(result, "data.table")
  expect_true("sum_emp" %in% names(result))
})

test_that("hier='h_1' returns one row per h_1 group per year", {
  d <- make_hier_apply_data()
  result <- mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
                       bygroups = c("nace", "year"), hier = "h_1",
                       disclosure = FALSE)
  expect_equal(nrow(result), 2L)
})

test_that("sum values are correct", {
  d <- make_hier_apply_data()
  result <- mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
                       bygroups = c("nace", "year"), hier = "h_1",
                       disclosure = FALSE)
  expect_equal(sum(result$sum_emp), sum(d$DT$emp))
})

test_that("hier='ALL' returns rows for every hierarchy level", {
  d <- make_hier_apply_data()
  result <- mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
                       bygroups = c("nace", "year"), hier = "ALL",
                       disclosure = FALSE)
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0L)
})

test_that("node column identifies aggregation level", {
  d <- make_hier_apply_data()
  result <- mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
                       bygroups = c("nace", "year"), hier = "h_1",
                       disclosure = FALSE)
  expect_true("node" %in% names(result))
})

test_that("error on non-data.table DT", {
  d <- make_hier_apply_data()
  expect_error(
    mdi_hier_apply(as.data.frame(d$DT), d$hhfile, var_list = "emp",
               bygroups = c("nace", "year"), hier = "h_1",
               disclosure = FALSE),
    "'DT' must be a data.table"
  )
})

test_that("error on non-data.table hhfile", {
  d <- make_hier_apply_data()
  expect_error(
    mdi_hier_apply(d$DT, as.data.frame(d$hhfile), var_list = "emp",
               bygroups = c("nace", "year"), hier = "h_1",
               disclosure = FALSE),
    "'hhfile' must be a data.table"
  )
})

test_that("error when hier not in hhfile columns", {
  d <- make_hier_apply_data()
  expect_error(
    mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
               bygroups = c("nace", "year"), hier = "h_99",
               disclosure = FALSE),
    "not found in hhfile columns"
  )
})

test_that("error when var_list column missing from DT", {
  d <- make_hier_apply_data()
  expect_error(
    mdi_hier_apply(d$DT, d$hhfile, var_list = "revenue",
               bygroups = c("nace", "year"), hier = "h_1",
               disclosure = FALSE),
    "columns not found"
  )
})

test_that("error when var_list is not a character vector", {
  d <- make_hier_apply_data()
  expect_error(
    mdi_hier_apply(d$DT, d$hhfile, var_list = 1L,
               bygroups = c("nace", "year"), hier = "h_1",
               disclosure = FALSE),
    "must be a non-empty character vector"
  )
})

test_that("error when bygroups is not a character vector", {
  d <- make_hier_apply_data()
  expect_error(
    mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
               bygroups = 1L, hier = "h_1",
               disclosure = FALSE),
    "must be a non-empty character vector"
  )
})

test_that("error when weight is not a string", {
  d <- make_hier_apply_data()
  expect_error(
    mdi_hier_apply(d$DT, d$hhfile, var_list = "emp",
               bygroups = c("nace", "year"), hier = "h_1",
               weight_col =123, disclosure = FALSE),
    "must be a non-empty character string"
  )
})