#' Calculate Joint Distributions
#'
#' @description
#' Computes joint distributions for specified variables within a data table.
#' Calculates distributional moments (deciles, quintiles, or quartiles) for
#' one or more variables, aggregated by specified groups and potentially
#' hierarchical structures.
#'
#' @param DT A `data.table` containing variables for distribution calculations.
#' @param hhfile A `data.table` containing hierarchical information for
#'   aggregation. Must have an `h_0` column matching `bygroups[1]`.
#' @param qnames Character vector. Names of variables used for calculating
#'   distributional moments.
#' @param var_names Character vector. Names of the variable(s) whose aggregates
#'   are computed.
#' @param moment Character scalar. Distributional moment to compute. One of
#'   `"decile"`, `"quintile"`, or `"quartile"`. Default `"decile"`.
#' @param bygroups Character vector. Variables used for stratification.
#' @param hier Character scalar. Hierarchical level for aggregation. `"ALL"`
#'   uses all levels in `hhfile`; otherwise specify a column name.
#' @param agg_type Character scalar. Type of aggregation (e.g. `"sum"`,
#'   `"mean"`).
#' @param prefix Character scalar. Prefix for naming aggregated variables.
#'   Default is the value of `agg_type`.
#' @param weight_col Optional character scalar. Name of a weight column in
#'   `DT` for weighted aggregation.
#' @param mrg Logical. Whether to merge results back with the original data
#'   table. Default `FALSE`.
#' @param disclosure Logical. Whether to apply disclosure control. Default
#'   `TRUE`.
#'
#' @return A `data.table` with computed joint distributions including the
#'   distributional moments for specified variables aggregated by the given
#'   criteria.
#'
#' @examples
#' \donttest{
#' library(data.table)
#' DT <- data.table(
#'   nace = rep(c("A", "B"), each = 5),
#'   year = rep(2020L, 10),
#'   emp  = c(10, 20, 5, 15, 8, 12, 25, 6, 14, 9)
#' )
#' hhfile <- data.table(h_0 = c("A", "B"), h_1 = c("X", "X"))
#' mdi_jointdist(DT, hhfile,
#'   qnames = "emp", var_names = "emp", moment = "quartile",
#'   bygroups = c("nace", "year"), hier = "h_1",
#'   agg_type = "sum", disclosure = FALSE)
#' }
#'
#' @export

mdi_jointdist <- function(DT, hhfile, qnames, var_names,
                          moment = c("decile", "quintile", "quartile"),
                          bygroups, hier, agg_type, prefix = agg_type,
                          weight_col = NULL, mrg = FALSE, disclosure = TRUE) {

  check_dt(DT)
  check_dt(hhfile, required_cols = "h_0", arg_name = "hhfile")
  check_char_vec(qnames,    "qnames")
  check_char_vec(var_names, "var_names")
  check_char_vec(bygroups,  "bygroups")
  check_string(hier,        "hier")
  check_string(agg_type,    "agg_type")
  moment <- match.arg(moment)

  m <- switch(moment, decile = 0.1, quintile = 0.2, quartile = 0.25)

  DTout <- merge(hhfile, DT, by.x = "h_0", by.y = bygroups[1])

  if (hier != "ALL") {
    hlist <- c(hier)
  } else {
    hlist <- sort(names(hhfile))
  }

  dims <- lapply(hlist, function(hh) append(hh, bygroups[-1]))

  DTagg <- data.table::rbindlist(lapply(seq_along(dims), function(i) {

    dim_i <- dims[[i]]

    tmp1 <- data.table::rbindlist(lapply(qnames, function(qname) {

      qcol <- paste0(qname, "_", moment)
      DTout[, (qcol) := lapply(.SD, function(x) {
        ranks <- rank(x, ties.method = "random", na.last = "keep")
        cut(ranks,
            breaks = quantile(ranks, probs = seq(0, 1, m), na.rm = TRUE),
            labels = FALSE, include.lowest = TRUE)
      }), by = dim_i, .SDcols = qname]

      tmp2 <- mdi_aggregate(
        DT         = DTout,
        var_list   = var_names,
        bygroups   = c(dim_i, qcol),
        agg_type   = agg_type,
        weight_col     = weight_col,
        mrg        = mrg,
        disclosure = disclosure
      )

      tmp2[, ("qname") := qname]
      data.table::setnames(tmp2, qcol, "qmoment")
    }))

    tmp1[, ("node") := dim_i[1]]
    data.table::setnames(tmp1, dim_i[1], bygroups[1])
  }))

  data.table::setattr(DTagg, "sumvars", var_names)
  data.table::setkeyv(DTagg, c(bygroups, "qname", "qmoment"))

  return(DTagg)
}