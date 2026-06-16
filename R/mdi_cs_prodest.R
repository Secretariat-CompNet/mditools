#' @keywords internal
.cs_assert_cols <- function(DT, cols) {
  miss <- setdiff(cols, names(DT))
  if (length(miss) > 0)
    stop("DT is missing required columns: ", paste(miss, collapse = ", "))
}

#' @keywords internal
.cs_to_levels <- function(x, log_values) {
  if (!log_values) return(x)
  exp(x)
}

#' @keywords internal
.cs_compute_shares <- function(DT, inputs_lvl, input_names) {
  DT[, ("total_cost") := rowSums(.SD, na.rm = TRUE), .SDcols = inputs_lvl]
  DT[DT[["total_cost"]] <= 0 | is.na(DT[["total_cost"]]), ("total_cost") := NA_real_]
  DT <- DT[!is.na(DT[["total_cost"]])]
  for (j in seq_along(inputs_lvl)) {
    el_name <- paste0("el_", input_names[j])
    DT[, (el_name) := DT[[inputs_lvl[j]]] / DT[["total_cost"]]]
  }
  DT
}

#' @keywords internal
.cs_average_shares <- function(DT, share_cols, share_group_vars) {
  DT[, lapply(.SD, mean, na.rm = TRUE),
     by = share_group_vars, .SDcols = share_cols]
}

#' @keywords internal
.cs_compute_tfp <- function(DT, y_lvl_col, x_lvl_cols, input_names) {
  .eps <- 1e-12
  tfp_vals <- log(pmax(DT[[y_lvl_col]], .eps))
  for (j in seq_along(input_names)) {
    el_col   <- paste0("el_", input_names[j])
    tfp_vals <- tfp_vals - DT[[el_col]] * log(pmax(DT[[x_lvl_cols[j]]], .eps))
  }
  DT[, ("tfp") := tfp_vals]
  DT
}

#' Cost-Shares Production Function Estimator (Cobb-Douglas)
#'
#' @description
#' Implements a cost-shares approach to production function estimation:
#' - Build firm-time input shares from expenditures in levels.
#' - Average shares by \code{(bygroup, time)}.
#' - Compute TFP as a log-index residual using averaged shares.
#' - Optionally demean TFP by \code{(bygroup, time)}.
#'
#' @param DT A \code{data.table} or coercible \code{data.frame} containing panel data.
#' @param y Character scalar. Name of the output variable column.
#' @param endog Character scalar. Name of the free input (expenditure) column.
#' @param exog Character scalar. Name of the state input (expenditure) column.
#' @param id Character scalar. Name of the entity identifier column.
#' @param time Character scalar. Name of the time period column.
#' @param bygroup Character scalar. Name of the grouping variable column
#'   (e.g., industry code).
#' @param log_values Logical. If \code{TRUE} (default), \code{y}, \code{endog},
#'   and \code{exog} are treated as log values and exponentiated before share
#'   construction.
#' @param TFP_demeaned Logical. If \code{TRUE} (default), returns a
#'   \code{TFP_demeaned} column equal to tfp minus its mean within
#'   \code{(bygroup, time)}. This is mechanically zero by construction of the
#'   cost-shares index.
#'
#' @return A \code{data.table} with one row per observation (after cost-share
#'   filtering), containing:
#' - \code{id}, \code{time}, \code{bygroup} columns
#' - \code{el_<endog>}, \code{el_<exog>}: averaged input elasticities
#' - \code{tfp}: total factor productivity (log-index residual)
#' - \code{TFP_demeaned} (if \code{TFP_demeaned = TRUE})
#' - \code{NumObs}: total number of observations used (matches the convention
#'   of the other estimators in this suite)
#'
#' Returns \code{NULL} if no valid observations remain after filtering.
#'
#' @examples
#' library(data.table)
#' set.seed(1)
#' n <- 120
#' DT <- data.table(
#'   id      = rep(1:30, each = 4),
#'   year    = rep(2000:2003, times = 30),
#'   nace    = rep(c("A", "B"), each = 60),
#'   y       = log(runif(n, 5, 50)),
#'   labour  = log(runif(n, 1, 10)),
#'   capital = log(runif(n, 2, 20))
#' )
#' mdi_cs_prodest(DT, y = "y", endog = "labour", exog = "capital",
#'                id = "id", time = "year", bygroup = "nace")
#'
#' @export

mdi_cs_prodest <- function(DT,
                           y,
                           endog,
                           exog,
                           id,
                           time,
                           bygroup,
                           log_values  = TRUE,
                           TFP_demeaned = TRUE) {

  if (!data.table::is.data.table(DT) && !is.data.frame(DT))
    stop("'DT' must be a data.table or data.frame")
  DT <- data.table::as.data.table(data.table::copy(DT))

  check_string(y,       "y")
  check_string(endog,   "endog")
  check_string(exog,    "exog")
  check_string(id,      "id")
  check_string(time,    "time")
  check_string(bygroup, "bygroup")

  .cs_assert_cols(DT, unique(c(id, time, bygroup, y, endog, exog)))

  keep <- unique(c(id, time, bygroup, y, endog, exog))
  DT   <- DT[, keep, with = FALSE]
  DT   <- DT[complete.cases(DT), ]
  if (nrow(DT) == 0) return(NULL)

  for (v in c(y, endog, exog)) {
    if (!is.numeric(DT[[v]]))
      stop(sprintf("[cs] Variable '%s' must be numeric.", v))
  }

  y_lvl_col <- ".cs_y_lvl"
  l_lvl_col <- ".cs_endog_lvl"
  k_lvl_col <- ".cs_exog_lvl"
  .y_vals <- .cs_to_levels(DT[[y]],     log_values)
  .l_vals <- .cs_to_levels(DT[[endog]], log_values)
  .k_vals <- .cs_to_levels(DT[[exog]],  log_values)
  DT[, (y_lvl_col) := .y_vals]
  DT[, (l_lvl_col) := .l_vals]
  DT[, (k_lvl_col) := .k_vals]

  if (any(DT[[y_lvl_col]] < 0, na.rm = TRUE)) stop("[cs] Output levels contain negative values.")
  if (any(DT[[l_lvl_col]] < 0, na.rm = TRUE)) stop("[cs] Endog input levels contain negative values.")
  if (any(DT[[k_lvl_col]] < 0, na.rm = TRUE)) stop("[cs] Exog input levels contain negative values.")

  input_names   <- c(endog, exog)
  input_lvlcols <- c(l_lvl_col, k_lvl_col)

  DT <- .cs_compute_shares(DT, inputs_lvl = input_lvlcols, input_names = input_names)
  if (nrow(DT) == 0) return(NULL)

  share_group_vars <- unique(c(bygroup, time))
  share_cols       <- paste0("el_", input_names)
  shares           <- .cs_average_shares(DT, share_cols = share_cols,
                                         share_group_vars = share_group_vars)

  DT <- merge(DT, shares, by = share_group_vars, suffixes = c("", "_avg"),
              all.x = TRUE)
  for (nm in share_cols) {
    DT[, (nm) := DT[[paste0(nm, "_avg")]]]
    DT[, (paste0(nm, "_avg")) := NULL]
  }

  DT <- .cs_compute_tfp(DT, y_lvl_col = y_lvl_col,
                        x_lvl_cols = input_lvlcols, input_names = input_names)

  if (TFP_demeaned) {
    DT[, ("TFP_demeaned") := .SD[["tfp"]] - mean(.SD[["tfp"]], na.rm = TRUE),
       by = share_group_vars, .SDcols = "tfp"]
  }

  # NumObs = total observations used (matches the other estimators).
  DT[, ("NumObs") := .N]

  DT[, (y_lvl_col) := NULL]
  DT[, (l_lvl_col) := NULL]
  DT[, (k_lvl_col) := NULL]
  DT[, ("total_cost") := NULL]

  out_cols <- c(id, time, bygroup, paste0("el_", input_names), "tfp")
  if (TFP_demeaned) out_cols <- c(out_cols, "TFP_demeaned")
  out_cols <- c(out_cols, "NumObs")

  DT  <- DT[, out_cols, with = FALSE]
  ord <- do.call(order, DT[, .SD, .SDcols = c(time, id)])
  return(DT[ord])
}
