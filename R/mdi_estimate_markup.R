#' Estimate Firm-Level Markup
#'
#' @description
#' Estimates firm-level markup following De Loecker (2012): markup is the
#' output elasticity of an input divided by its expenditure share of revenue.
#'
#' @param DT A `data.table` containing panel data.
#' @param oe Character scalar. Name of the output elasticity column.
#'   Default `"oe_l"`.
#' @param rev_col Character scalar. Name of the total revenue column.
#'   Default `"nq"`.
#' @param input_cost Character scalar. Name of the input cost column.
#'   Default `"nm"`.
#'
#' @return A `data.table` with a single column `markup`.
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   oe_l = c(0.6, 0.7, 0.5),
#'   nq   = c(100, 200, 150),
#'   nm   = c(50,  80,  60)
#' )
#' mdi_estimate_markup(DT)
#'
#' @export

mdi_estimate_markup <- function(DT, oe = "oe_l", rev_col = "nq", input_cost = "nm") {
  check_string(oe,         "oe")
  check_string(rev_col,    "rev_col")
  check_string(input_cost, "input_cost")
  check_dt(DT, c(oe, rev_col, input_cost))

  markup <- DT[[oe]] * DT[[rev_col]] / DT[[input_cost]]
  data.table::data.table(markup = markup)
}