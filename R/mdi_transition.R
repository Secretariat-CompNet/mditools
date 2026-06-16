#' Count Classification Switches Between Consecutive Periods
#'
#' @description
#' Identifies pairs of classification codes between which a unit switched in a
#' given year, and counts how often each specific switch is observed across
#' units. The output has one row per (old code, new code, year) triple.
#'
#' @param DT A data.table containing the panel data. Modified in place by
#'   sorting on `id` and `time` before processing.
#' @param id Character. Name of the unit identifier column (e.g. `"firmid"`).
#' @param time Character. Name of the time variable column (e.g. `"year"`).
#' @param classvar Character. Name of the classification variable column
#'   (e.g. `"nace"`).
#' @return A data.table with four columns: `old_code`, `new_code`,
#'   `year_shifted`, and `N` (count of units making that switch in that year).
#' @export
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   firmid = c("A", "A", "A", "B", "B", "B"),
#'   year   = c(2010L, 2011L, 2012L, 2010L, 2011L, 2012L),
#'   nace   = c("C10", "C10", "C20", "D30", "D40", "D40")
#' )
#' mdi_transition(DT, id = "firmid", time = "year", classvar = "nace")

mdi_transition <- function(DT, id, time, classvar) {

  check_string(id,    "id")
  check_string(time,     "time")
  check_string(classvar, "classvar")
  check_dt(DT, c(id, time, classvar))

  data.table::setorderv(DT, c(id, time), c(1L, 1L))

  data <- data.table::copy(DT)

  new_cols <- c("old_code", "new_code", "year_shifted")
  data[, (new_cols) := list(
    data.table::shift(.SD[[classvar]]),
    .SD[[classvar]],
    data.table::shift(.SD[[time]])
  ), by = id]

  data <- data[, new_cols, with = FALSE]

  data <- data[data[["new_code"]] != data[["old_code"]]]

  data <- data[, .N, by = c("old_code", "new_code", "year_shifted")]

  return(data)
}