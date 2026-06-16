
#' Estimate Capital Stock Using Perpetual Inventory Method (PIM)
#'
#' @description
#' Estimates capital stock based on the Perpetual Inventory Method (PIM),
#' as outlined in Halle & Mairesse (1995). This function is tailored for data
#' structured as panel data, with indexing based on a firm identifier (`firmid`)
#' and a year variable. It involves the following steps:
#' - Reading the specified variables from the input data.table.
#' - Depending on whether the depreciation rate is provided directly or inferred
#'   from the asset type, the function calculates the capital stock.
#' - The output is the input data.table augmented with the new capital stock variable.
#'
#' The function supports different depreciation formats and asset types, offering
#' flexibility in estimating capital stock across various contexts.
#'
#' @param DT A data.table including the necessary variables.
#' @param id Column name of the firm (unit) identifier. Default is `"firmid"`.
#' @param K0 Column name for the real initial capital stock value. Default is
#'   `"K0"`.
#' @param I Column name for real investment. Default is `"ni_tan"`.
#' @param delta Column name for the depreciation rate. Default is `"d_GFCF"`.
#' @param output_name Name of the output variable for the estimated capital stock.
#'   Default is `"k_new"`.
#' @param time Name of the time variable. Default is `"year"`.
#' @return A modified `DT` with the new capital stock variable appended.
#' @export
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   firmid = rep(c("F1", "F2"), each = 4),
#'   year   = rep(2010:2013, 2),
#'   K0     = c(100, NA, NA, NA, 200, NA, NA, NA),
#'   ni_tan = c(10, 12, 11, 13, 20, 22, 21, 23),
#'   d_GFCF = rep(0.08, 8)
#' )
#' \donttest{
#' mdi_pim_capital(DT)
#' }
#' @references
#' Halle, P., & Mairesse, J. (1995). "Estimation of the Perpetual Inventory Method
#' for Capital Stock".

mdi_pim_capital <- function(DT, id = "firmid", K0 = "K0", I = "ni_tan", delta = "d_GFCF",
                        output_name = "k_new", time = "year") {

  check_string(id,          "id")
  check_string(time,        "time")
  check_string(K0,          "K0")
  check_string(I,           "I")
  check_string(delta,       "delta")
  check_string(output_name, "output_name")
  check_dt(DT, c(id, time, K0, I, delta))

  # Build subset
  cols <- c(id, time, K0, I, delta)
  sub <- unique(DT[, cols, with = FALSE])
  data.table::setorderv(sub, c(id, time))

  # Temporarily rename ID to 'firmid' for grouping
  data.table::setnames(sub, id, "firmid")

  # Alias parameter names so they don't shadow column names inside data.table j
  .cn_K0    <- K0
  .cn_I     <- I
  .cn_delta <- delta
  .cn_t     <- time

  # Filter out rows where K0, delta and I are missing
  sub <- sub[
    , if (any(!is.na(.SD[[.cn_K0]])) & any(!is.na(.SD[[.cn_I]])) & any(!is.na(.SD[[.cn_delta]]))) .SD,
    by = "firmid"
  ]

  # define computation intervals
  intervals <- sub[
    , list(
      start_time = max(
        min(.SD[[.cn_t]][!is.na(.SD[[.cn_K0]])]),
        min(.SD[[.cn_t]][!is.na(.SD[[.cn_I]])]),
        min(.SD[[.cn_t]][!is.na(.SD[[.cn_delta]])])
      ),
      end_time = min(
        max(.SD[[.cn_t]][!is.na(.SD[[.cn_I]])]),
        max(.SD[[.cn_t]][!is.na(.SD[[.cn_delta]])])
      )
    ),
    by = "firmid"
  ]

  sub <- merge(sub, intervals, by = "firmid", all.x = TRUE)
  .t_col   <- sub[[time]]
  .t_start <- sub[["start_time"]]
  .t_end   <- sub[["end_time"]]
  sub <- sub[.t_col >= .t_start & .t_col <= .t_end, ]

  # PIM computation
  sub[, ("capital") := {
    K <- numeric(.N)
    K[1] <- .SD[[.cn_K0]][1]
    if (.N > 1) {
      for (.i in 2:.N) {
        if (is.na(.SD[[.cn_I]][.i - 1])) {
          K[.i] <- K[.i - 1]
        } else {
          K[.i] <- (1 - .SD[[.cn_delta]][.i - 1]) * K[.i - 1] + .SD[[.cn_I]][.i - 1]
        }
      }
    }
    K
  }, by = "firmid"]

  # Rename 'firmid' back to the original column name
  data.table::setnames(sub, "firmid", id)
  data.table::setnames(sub, "capital", output_name)

  # Merge back
  DT <- merge(
    DT,
    sub[, c(id, time, output_name), with = FALSE],
    by = c(id, time),
    all.x = TRUE
  )

  return(DT)
}
