#' @keywords internal
check_choice <- function(x, arg_name, choices) {
  if (!x %in% choices)
    stop(paste0("'", arg_name, "' must be one of: ",
                paste(paste0('"', choices, '"'), collapse = ", "), "."))
}

#' @keywords internal
check_string <- function(x, arg_name) {
  if (!is.character(x) || length(x) != 1L || nchar(x) == 0L)
    stop(paste0("'", arg_name, "' must be a non-empty character string."))
}

#' @keywords internal
check_char_vec <- function(x, arg_name) {
  if (!is.character(x) || length(x) == 0L)
    stop(paste0("'", arg_name, "' must be a non-empty character vector."))
}

#' @keywords internal
check_dt <- function(DT, required_cols = character(0), arg_name = "DT") {
  if (!data.table::is.data.table(DT))
    stop(paste0("'", arg_name, "' must be a data.table"))
  missing <- setdiff(required_cols, names(DT))
  if (length(missing) > 0)
    stop(paste0("columns not found in '", arg_name, "': ",
                paste(missing, collapse = ", ")))
}

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

#' @keywords internal
panel_lag <- function(x, id_vec, time_vec) {
  panel_lag_L(x, id_vec, time_vec, L = 1L)
}
