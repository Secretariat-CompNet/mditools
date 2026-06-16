#' Hierarchical Aggregation of Data
#'
#' @description
#' Aggregates variables in `var_list` to unique values of the hierarchical
#' dimensions specified in `hhfile` by groups. Aggregation is performed at
#' each level specified by `hier` using [`mdi_aggregate()`].
#'
#' @param DT A `data.table` to be aggregated. Must contain all columns in
#'   `var_list` and `bygroups`.
#' @param hhfile A `data.table` containing the hierarchy; must include a
#'   column `h_0` matching `bygroups[1]` in `DT`, plus one column per
#'   aggregation level (e.g. `h_1`, `h_2`).
#' @param var_list A character vector of numeric variable names in `DT` to
#'   aggregate.
#' @param bygroups A character vector of grouping variables in `DT`. The first
#'   element must match `h_0` in `hhfile`.
#' @param hier Character. Either a single node name (e.g. `"h_2"`) to
#'   aggregate `h_0` up to that level, or `"ALL"` to aggregate to every
#'   available node in `hhfile`.
#' @param agg_type Character vector of aggregation types passed to
#'   [`mdi_aggregate()`]. Default `"sum"`.
#' @param weight_col Optional character string naming a weight column in
#'   `DT`. Passed as `weight_col` to [`mdi_aggregate()`]. Default `NULL`.
#' @param mrg Logical. If `FALSE`, returns the aggregated result. If `TRUE`,
#'   merges result back into `DT`. Default `FALSE`.
#' @param disclosure Logical. If `TRUE`, dominance and observation-count
#'   columns are added for disclosure control (only when `mrg = FALSE`).
#'   Default `TRUE`.
#'
#' @return A `data.table` containing the aggregated variables from `var_list`
#'   at each requested hierarchy level, combined via `rbindlist`. A `node`
#'   column identifies the aggregation level of each row.
#'
#' @examples
#' library(data.table)
#' hhfile <- data.table(
#'   h_0 = c("A1", "A2", "B1", "B2"),
#'   h_1 = c("A",  "A",  "B",  "B")
#' )
#' DT <- data.table(
#'   nace = c("A1", "A2", "B1", "B2"),
#'   year = rep(2020L, 4),
#'   emp  = c(10L, 20L, 15L, 25L)
#' )
#' mdi_hier_apply(DT, hhfile, var_list = "emp",
#'            bygroups = c("nace", "year"), hier = "h_1",
#'            disclosure = FALSE)
#'
#' @export


mdi_hier_apply <- function(DT, hhfile, var_list, bygroups, hier, agg_type = "sum",
                       weight_col = NULL, mrg = FALSE, disclosure = TRUE) {
  check_char_vec(var_list, "var_list")
  check_char_vec(bygroups, "bygroups")
  check_char_vec(agg_type, "agg_type")
  check_string(hier, "hier")
  if (!is.null(weight_col)) check_string(weight_col, "weight_col")
  check_dt(DT, c(var_list, bygroups))
  check_dt(hhfile, required_cols = "h_0", arg_name = "hhfile")
  if (hier != "ALL" && !hier %in% names(hhfile))
    stop(paste0("'hier' \"", hier, "\" not found in hhfile columns"))

  if (hier != "ALL") {
    h0 <- "h_0"
    hlist <- c(hier)
  } else if (hier=="ALL") {
    h0 <- "h_0"
    hlist <- sort(names(hhfile))
  }

  #bring in the information of parental nodes from hhfile, i.e. merge DT with hhfile by bygroups.1
  DTout <- merge(hhfile, DT, by.x = h0, by.y = bygroups[1])


  #aggregation by hierarchies
  dims <- lapply(hlist, function(hh){append(hh, bygroups[-1])})
  DTagg <- lapply(seq_along(dims), function(i){
    data.table::setnames(
      mdi_aggregate(DTout,
                    var_list,
                    bygroups   = dims[[i]],
                    agg_type   = agg_type,
                    weight_col     = weight_col,
                    mrg        = mrg,
                    disclosure = disclosure)
        [, ("node") := dims[[i]][1]],
      dims[[i]][1],
      bygroups[1]
    )
  })

  #merge the aggregation results with hhfile, then combined together
  DTmg <- data.table::rbindlist(DTagg)

  data.table::setkeyv(DTmg, bygroups)
  data.table::setattr(DTmg, "sumvars", var_list)

  return(DTmg)
}

