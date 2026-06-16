library(data.table)

make_reg_dt <- function(df_val = 20L, numobs_val = 25L) {
  data.table(
    coef         = c("(Intercept)", "x1"),
    Estimate     = c(1.2, 0.5),
    `Std. Error` = c(0.1, 0.05),
    df           = c(df_val, df_val),
    NumObs       = c(numobs_val, numobs_val)
  )
}

make_firm_dt <- function(numfirms_val = 10L, nument_val = 10L) {
  data.table(
    coef         = c("(Intercept)", "x1"),
    Estimate     = c(1.2, 0.5),
    `Std. Error` = c(0.1, 0.05),
    NumFirms     = c(numfirms_val, numfirms_val),
    NumEnt       = c(nument_val, nument_val)
  )
}

test_that("returns list with DT, vars, redacted_n", {
  result <- mdi_disclose_reg_tab(make_reg_dt(), min_obs = 3L)
  expect_type(result, "list")
  expect_true(all(c("DT", "vars", "redacted_n") %in% names(result)))
  expect_s3_class(result$DT, "data.table")
  expect_type(result$redacted_n, "integer")
})

test_that("no masking when obs and df are above threshold", {
  result <- mdi_disclose_reg_tab(make_reg_dt(df_val = 20L, numobs_val = 25L),
                             min_obs = 5L)
  expect_false(any(is.na(result$DT$Estimate)))
  expect_equal(result$redacted_n, 0L)
})

test_that("masks Estimate when df below threshold", {
  result <- mdi_disclose_reg_tab(make_reg_dt(df_val = 2L, numobs_val = 25L),
                             min_obs = 5L)
  expect_true(all(is.na(result$DT$Estimate)))
  expect_gt(result$redacted_n, 0L)
})

test_that("masks Estimate when NumObs below threshold", {
  result <- mdi_disclose_reg_tab(make_reg_dt(df_val = 20L, numobs_val = 2L),
                             min_obs = 5L)
  expect_true(all(is.na(result$DT$Estimate)))
})

test_that("show_disclosed=TRUE keeps values visible even when flagged", {
  result <- mdi_disclose_reg_tab(make_reg_dt(df_val = 2L), min_obs = 5L,
                             show_disclosed = TRUE)
  expect_false(any(is.na(result$DT$Estimate)))
})

test_that("vars returns fallback string when all rows masked", {
  result <- mdi_disclose_reg_tab(make_reg_dt(df_val = 2L), min_obs = 5L)
  expect_equal(result$vars, "No coefficients disclosed (all masked)")
})

test_that("disc_method='firm_count' masks when NumFirms below threshold", {
  result <- mdi_disclose_reg_tab(make_firm_dt(numfirms_val = 2L, nument_val = 10L),
                             min_obs = 5L, disc_method = "firm_count")
  expect_true(all(is.na(result$DT$Estimate)))
})

test_that("disc_method='firm_count' masks when NumEnt below threshold", {
  result <- mdi_disclose_reg_tab(make_firm_dt(numfirms_val = 10L, nument_val = 2L),
                             min_obs = 5L, disc_method = "firm_count")
  expect_true(all(is.na(result$DT$Estimate)))
})

test_that("disc_method='firm_count' no masking when both above threshold", {
  result <- mdi_disclose_reg_tab(make_firm_dt(numfirms_val = 10L, nument_val = 10L),
                             min_obs = 5L, disc_method = "firm_count")
  expect_false(any(is.na(result$DT$Estimate)))
  expect_equal(result$redacted_n, 0L)
})

test_that("error on non-data.table input", {
  expect_error(
    mdi_disclose_reg_tab(as.data.frame(make_reg_dt()), min_obs = 3L),
    "'DT' must be a data.table"
  )
})

test_that("error when required columns are missing for obs_df", {
  DT <- data.table(x = 1:3)
  expect_error(
    mdi_disclose_reg_tab(DT, min_obs = 3L),
    "DT must include columns"
  )
})

test_that("error when required columns are missing for firm_count", {
  DT <- data.table(coef = c("x1"), Estimate = c(1.0))
  expect_error(
    mdi_disclose_reg_tab(DT, min_obs = 3L, disc_method = "firm_count"),
    "DT must include columns"
  )
})

test_that("error on invalid disc_method", {
  expect_error(
    mdi_disclose_reg_tab(make_reg_dt(), disc_method = "bad"),
    "should be one of"
  )
})