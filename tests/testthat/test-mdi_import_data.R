library(data.table)

tmp_dir <- function() paste0(tempdir(), "/")

test_that("error on unsupported format", {
  expect_error(
    mdi_import_data("dir/", "file.abc", "abc"),
    "must be one of"
  )
})

test_that("error when dir is not a string", {
  expect_error(
    mdi_import_data(123, "file.csv", "csv"),
    "must be a non-empty character string"
  )
})

test_that("error when file is not a string", {
  expect_error(
    mdi_import_data("dir/", 456, "csv"),
    "must be a non-empty character string"
  )
})

test_that("error when col_list is not a character vector", {
  expect_error(
    mdi_import_data(tmp_dir(), "f.csv", "csv", col_list = 1L),
    "must be a non-empty character vector"
  )
})

test_that("error when char_columns is not a character vector", {
  expect_error(
    mdi_import_data(tmp_dir(), "f.csv", "csv", char_columns = 1L),
    "must be a non-empty character vector"
  )
})

test_that("imports CSV and returns data.table", {
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".csv")
  write.csv(data.frame(id = 1:3, val = c(10, 20, 30)), tmp, row.names = FALSE)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "csv")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
  unlink(tmp)
})

test_that("col_list subsets columns for CSV", {
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".csv")
  write.csv(data.frame(id = 1:3, val = c(10, 20, 30)), tmp, row.names = FALSE)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "csv", col_list = "id")
  expect_equal(names(result), "id")
  unlink(tmp)
})

test_that("char_columns converts column to character for CSV", {
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".csv")
  write.csv(data.frame(id = 1:3, val = c(10, 20, 30)), tmp, row.names = FALSE)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "csv", char_columns = "id")
  expect_type(result$id, "character")
  unlink(tmp)
})

test_that("imports RDS and returns data.table", {
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  saveRDS(data.frame(id = 1:3, val = c(10, 20, 30)), tmp)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "rds")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
  unlink(tmp)
})

test_that("col_list subsets columns for RDS", {
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  saveRDS(data.frame(id = 1:3, val = c(10, 20, 30)), tmp)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "rds", col_list = "id")
  expect_equal(names(result), "id")
  unlink(tmp)
})

test_that("imports parquet and returns data.table", {
  skip_if_not_installed("arrow")
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".parquet")
  arrow::write_parquet(data.frame(id = 1:3, val = c(10, 20, 30)), tmp)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "parquet")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
  unlink(tmp)
})

test_that("col_list subsets columns for parquet", {
  skip_if_not_installed("arrow")
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".parquet")
  arrow::write_parquet(data.frame(id = 1:3, val = c(10, 20, 30)), tmp)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "parquet", col_list = "id")
  expect_equal(names(result), "id")
  unlink(tmp)
})

test_that("integer64 columns are converted to numeric", {
  skip_if_not_installed("arrow")
  tmp <- tempfile(tmpdir = tempdir(), fileext = ".parquet")
  tbl <- arrow::arrow_table(
    id  = 1:3,
    big = arrow::Array$create(c(1e15, 2e15, 3e15), type = arrow::int64())
  )
  arrow::write_parquet(tbl, tmp)
  result <- mdi_import_data(tmp_dir(), basename(tmp), "parquet")
  expect_type(result$big, "double")
  unlink(tmp)
})