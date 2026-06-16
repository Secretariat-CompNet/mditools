library(data.table)

make_cluster_dt <- function(n_per_group = 20, seed = 1) {
  set.seed(seed)
  data.table(
    firmid = seq_len(n_per_group * 2),
    x1     = c(rnorm(n_per_group, 0, 0.4), rnorm(n_per_group, 6, 0.4)),
    x2     = c(rnorm(n_per_group, 0, 0.4), rnorm(n_per_group, 6, 0.4))
  )
}

test_that("error on missing id_var column", {
  dt <- make_cluster_dt()
  expect_error(
    mdi_clustering(dt, id_vars = "nosuchcol", cluster_vars = c("x1", "x2"),
               method = "kmeans", k_selection = "fixed", k_fixed = 2),
    "id_vars are not in DT"
  )
})

test_that("error on missing cluster_var column", {
  dt <- make_cluster_dt()
  expect_error(
    mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "nosuch"),
               method = "kmeans", k_selection = "fixed", k_fixed = 2),
    "cluster_vars are not in DT"
  )
})

test_that("error when k_fixed missing for fixed k_selection", {
  dt <- make_cluster_dt()
  expect_error(
    mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
               method = "kmeans", k_selection = "fixed"),
    "k_fixed must be provided"
  )
})

test_that("error when cluster_col already exists and overwrite is FALSE", {
  dt <- make_cluster_dt()
  dt[, cluster := 0L]
  expect_error(
    mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
               method = "kmeans", k_selection = "fixed", k_fixed = 2),
    "already exists"
  )
})

test_that("error on non-numeric cluster_vars", {
  dt <- make_cluster_dt()
  dt[, x1 := as.character(x1)]
  expect_error(
    mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
               method = "kmeans", k_selection = "fixed", k_fixed = 2),
    "must be numeric"
  )
})

test_that("error when automatic_only method used with k_selection = fixed", {
  dt <- make_cluster_dt()
  expect_error(
    mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
               method = "mclust", k_selection = "fixed"),
    "treated as automatic only"
  )
})

test_that("kmeans fixed-k returns list with expected structure", {
  skip_on_cran()
  dt <- make_cluster_dt()
  result <- mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
                       method = "kmeans", k_selection = "fixed", k_fixed = 2,
                       compute_wss = TRUE, compute_silhouette = TRUE,
                       compute_stability = FALSE, verbose = FALSE)
  expect_type(result, "list")
  expect_named(result,
    c("data", "chosen_k", "wss", "silhouette", "stability",
      "selection_plot"))
  expect_s3_class(result$data, "data.table")
  expect_true("cluster" %in% names(result$data))
  expect_equal(result$chosen_k, 2L)
})

test_that("kmeans fixed-k recovers two well-separated groups", {
  skip_on_cran()
  dt <- make_cluster_dt()
  result <- mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
                       method = "kmeans", k_selection = "fixed", k_fixed = 2,
                       compute_wss = FALSE, compute_silhouette = FALSE,
                       compute_stability = FALSE, verbose = FALSE)
  tbl <- table(
    result$data$cluster,
    ifelse(result$data$firmid <= 20, "A", "B")
  )
  expect_equal(sum(diag(tbl)) + sum(diag(tbl[2:1, ])), 40L)
})

test_that("custom cluster_col name is used", {
  skip_on_cran()
  dt <- make_cluster_dt()
  result <- mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
                       method = "kmeans", k_selection = "fixed", k_fixed = 2,
                       cluster_col = "grp", compute_wss = FALSE,
                       compute_silhouette = FALSE, verbose = FALSE)
  expect_true("grp" %in% names(result$data))
  expect_false("cluster" %in% names(result$data))
})

test_that("hc_ward fixed-k returns correct structure", {
  skip_on_cran()
  dt <- make_cluster_dt()
  result <- mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
                       method = "hc_ward", k_selection = "fixed", k_fixed = 2,
                       compute_wss = FALSE, compute_silhouette = FALSE,
                       verbose = FALSE)
  expect_s3_class(result$data, "data.table")
  expect_equal(result$chosen_k, 2L)
})

test_that("na_action = 'stop' errors on missing data", {
  dt <- make_cluster_dt()
  dt[1, x1 := NA_real_]
  expect_error(
    mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
               method = "kmeans", k_selection = "fixed", k_fixed = 2,
               na_action = "stop", verbose = FALSE),
    "Missing values found"
  )
})

test_that("na_action = 'omit' clusters remaining rows", {
  skip_on_cran()
  dt <- make_cluster_dt()
  dt[1, x1 := NA_real_]
  result <- mdi_clustering(dt, id_vars = "firmid", cluster_vars = c("x1", "x2"),
                       method = "kmeans", k_selection = "fixed", k_fixed = 2,
                       na_action = "omit", compute_wss = FALSE,
                       compute_silhouette = FALSE, verbose = FALSE)
  expect_s3_class(result$data, "data.table")
  expect_equal(nrow(result$data), 40L)
  expect_true(is.na(result$data[firmid == 1, cluster]))
})