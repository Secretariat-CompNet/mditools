#' @keywords internal
#' @export
check_choice <- function(x, arg_name, choices) {
  if (!x %in% choices)
    stop(paste0("'", arg_name, "' must be one of: ",
                paste(paste0('"', choices, '"'), collapse = ", "), "."))
}

#' @keywords internal
#' @export
check_string <- function(x, arg_name) {
  if (!is.character(x) || length(x) != 1L || nchar(x) == 0L)
    stop(paste0("'", arg_name, "' must be a non-empty character string."))
}

#' @keywords internal
#' @export
check_char_vec <- function(x, arg_name) {
  if (!is.character(x) || length(x) == 0L)
    stop(paste0("'", arg_name, "' must be a non-empty character vector."))
}

#' @keywords internal
#' @export
check_dt <- function(DT, required_cols = character(0), arg_name = "DT") {
  if (!data.table::is.data.table(DT))
    stop(paste0("'", arg_name, "' must be a data.table"))
  missing <- setdiff(required_cols, names(DT))
  if (length(missing) > 0)
    stop(paste0("columns not found in '", arg_name, "': ",
                paste(missing, collapse = ", ")))
}

#' Gap-aware within-panel lag operators
#'
#' \code{panel_lag_L()} returns the value of \code{x} at the row that is
#' L time-units earlier for the same unit, or NA if no such row exists or
#' the actual time gap differs from L. \code{panel_lag()} is the L = 1
#' specialization.
#'
#' These helpers are gap-aware: they do NOT shift by L rows. Instead they
#' check the time variable and only return a lag when the lagged time
#' equals (current time) - L. Use these in any panel-data estimator where
#' time gaps within a unit's history would otherwise contaminate lagged
#' regressors or instruments.
#'
#' @param x numeric vector to be lagged.
#' @param id_vec vector of unit (firm) identifiers, same length as \code{x}.
#' @param time_vec vector of time identifiers, same length as \code{x}.
#'   Must be integer-coercible and spaced by 1 between consecutive periods.
#' @param L integer >= 1; number of time periods to lag by.
#' @return numeric vector of length(x), with NA where no valid lag exists.
#' @keywords internal
panel_lag_L <- function(x, id_vec, time_vec, L = 1L) {
  DT_tmp <- data.table::data.table(
    id_   = id_vec,
    time_ = as.numeric(time_vec),
    x_    = as.numeric(x)
  )
  data.table::setorderv(DT_tmp, c("id_", "time_"))
  DT_tmp[, ("x_lag") := data.table::shift(.SD[["x_"]], n = L, type = "lag"),
         by = "id_", .SDcols = "x_"]
  DT_tmp[, ("t_lag") := data.table::shift(.SD[["time_"]], n = L, type = "lag"),
         by = "id_", .SDcols = "time_"]
  DT_tmp[, ("x_lag") := data.table::fifelse(
    .SD[["time_"]] - .SD[["t_lag"]] == L, .SD[["x_lag"]], NA_real_),
    .SDcols = c("time_", "t_lag", "x_lag")]
  DT_tmp[["x_lag"]]
}

#' @rdname panel_lag_L
#' @keywords internal
panel_lag <- function(x, id_vec, time_vec) {
  panel_lag_L(x, id_vec, time_vec, L = 1L)
}
