library(data.table)

make_outlier_dt <- function() {
  data.table(id = 1:20, grp = rep(c("A", "B"), 10),
             x = c(rep(10, 18), 1000, 2000))
}

test_that("trim returns data.table with same columns", {
  DT <- make_outlier_dt()
  result <- mdi_outlier(DT, "x", "trim", 0.1, both_tails = FALSE)
  expect_s3_class(result, "data.table")
  expect_equal(ncol(result), ncol(DT))
})

test_that("trim sets upper-tail outliers to NA", {
  DT <- make_outlier_dt()
  result <- mdi_outlier(DT, "x", "trim", 0.05, both_tails = FALSE)
  expect_true(any(is.na(result$x)))
  expect_false(any(is.na(DT$x)))  # original unchanged
})

test_that("trim with both_tails also trims lower tail", {
  DT <- data.table(id = 1:20, x = c(0, 0, rep(10, 16), 1000, 2000))
  result <- mdi_outlier(DT, "x", "trim", 0.05, both_tails = TRUE)
  expect_true(any(is.na(result$x)))
})

test_that("winsorize clamps without introducing NAs", {
  DT <- make_outlier_dt()
  result <- mdi_outlier(DT, "x", "winsorize", 0.05, both_tails = FALSE)
  expect_false(any(is.na(result$x)))
  expect_true(max(result$x) < 2000)
})

test_that("flag adds flag_<var> column with 1 for flagged rows", {
  DT <- make_outlier_dt()
  result <- mdi_outlier(DT, "x", "flag", 0.05, both_tails = FALSE)
  expect_true("flag_x" %in% names(result))
  expect_true(any(result$flag_x == 1, na.rm = TRUE))
})

test_that("group argument applies routine within groups", {
  DT <- make_outlier_dt()
  result <- mdi_outlier(DT, "x", "winsorize", 0.1, group = "grp")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), nrow(DT))
})

test_that("error on non-data.table input", {
  df <- as.data.frame(make_outlier_dt())
  expect_error(mdi_outlier(df, "x", "trim", 0.1), "'DT' must be a data.table")
})

test_that("error on bad routine value", {
  DT <- make_outlier_dt()
  expect_error(mdi_outlier(DT, "x", "clip", 0.1), "should be one of")
})

test_that("error on fraction outside [0, 1]", {
  DT <- make_outlier_dt()
  expect_error(mdi_outlier(DT, "x", "trim", 1.5), "'fraction' must be a single numeric value")
})

test_that("error when var_list column not in DT", {
  DT <- make_outlier_dt()
  expect_error(mdi_outlier(DT, "nonexistent", "trim", 0.1), "columns not found")
})

test_that("error when group is not a string", {
  DT <- make_outlier_dt()
  expect_error(mdi_outlier(DT, "x", "trim", 0.1, group = 123L), "must be a non-empty character string")
})
