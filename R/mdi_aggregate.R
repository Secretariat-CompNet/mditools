#' Generic Aggregation Function
#'
#' @description
#' Aggregates variables from a sub-aggregate level to a higher aggregate level
#' within a `data.table`. Supports multiple aggregation types, including sum,
#' standard deviation, mean, quantiles (25%, 50%, 75%), median, count, number of
#' missing values, number of non-missing values, number of empty strings,
#' number of zeros, number of positive values, and the Herfindahl-Hirschman Index (HHI).
#'
#' The function can optionally:
#' - apply a weight column to variables before aggregation,
#' - merge aggregated statistics back into the original dataset,
#' - compute the number of unique firms in the input data,
#' - and apply disclosure control criteria.
#'
#' @param DT A `data.table` containing the data to aggregate.
#' @param var_list A character vector of variable names to aggregate.
#' @param bygroups A character vector of grouping variables defining the aggregation level.
#' @param agg_type A character vector specifying the type(s) of aggregation to perform.
#'   Supported types: `"sum"`, `"sd"`, `"mean"`, `"q10"`,`"q25"`, `"median"`, `"q75"`, `"q90"`,
#'   `"count"`, `"nmiss"`, `"n_nonmiss"`, `"nempty"`, `"nzero"`, `"npos"`, and `"HHI"`.
#'   Default is `"sum"`.
#' @param weight_col Optional character string naming a weight column in `DT` for
#'   weighted aggregates. Default is `NULL`.
#' @param mrg Logical. If `TRUE`, aggregated statistics are merged back into the
#'   original dataset as new variables. If `FALSE`, a new aggregated `data.table`
#'   is returned. Default is `FALSE`.
#' @param dom_formula Character. Dominance formula passed to [`mdi_disclose_crit()`]
#'   when `disclosure = TRUE`. `"top_share"` (default) computes the share of the
#'   top `domNr` firms; `"residual"` computes \eqn{(Total - x_1 - x_2) / x_1}.
#' @param count_firms Logical. If `TRUE`, adds a column `NumFirms` containing
#'   the number of unique firms in the input dataset. A firm identifier column
#'   (`plantid`, `firmid`, `entid`, or `entgrp`) must be present. Default is `FALSE`.
#' @param disclosure Logical. If `TRUE` and `mrg = FALSE`, disclosure criteria are
#'   applied, adding dominance indicators and number of observations for
#'   disclosure control. Default is `TRUE`.
#' @param minNumObs Integer. Minimum number of observations used for the
#'   quantile smoothing window in `"q10"`, `"q25"`, `"median"`, `"q75"`, `"q90"`
#'   aggregation types. Default is `5`.
#'
#' @return
#' - If `mrg = FALSE`: An aggregated `data.table` containing the requested statistics,
#'   optionally with disclosure variables and number of firms.
#' - If `mrg = TRUE`: The input `data.table` with new columns containing the
#'   aggregated statistics.
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   firmid = rep(1:5, each = 2),
#'   year   = rep(2020:2021, 5),
#'   nace   = rep(c("A", "B"), 5),
#'   emp    = c(10, 12, 5, 6, 20, 22, 8, 9, 15, 16),
#'   rev    = c(100, 110, 50, 55, 200, 210, 80, 85, 150, 155)
#' )
#'
#' # Sum by nace
#' mdi_aggregate(DT, "emp", "nace", "sum", disclosure = FALSE)
#'
#' # Multiple agg types
#' mdi_aggregate(DT, "emp", "nace", c("sum", "mean"), disclosure = FALSE)
#'
#' # Merge back into original DT
#' mdi_aggregate(DT, "emp", "nace", "sum", mrg = TRUE, disclosure = FALSE)
#'
#' # Count unique firms
#' mdi_aggregate(DT, "emp", "nace", "sum",
#'   count_firms = TRUE, disclosure = FALSE)
#'
#' @export

mdi_aggregate <-
  function(DT,
           var_list,
           bygroups,
           agg_type = c("sum"),
           weight_col = NULL,
           mrg = FALSE,
           disclosure = TRUE,
           count_firms = FALSE,
           dom_formula = c("top_share", "residual"),
           minNumObs = 5L) {

    dom_formula <- match.arg(dom_formula)
    check_dt(DT)
    check_char_vec(bygroups, "bygroups")
    check_char_vec(agg_type, "agg_type")
    if (!is.null(weight_col)) check_string(weight_col, "weight_col")

    valid_agg_types <- c("sum", "sd", "mean", "q10", "q25", "median", "q75",
                         "q90", "count", "nmiss", "n_nonmiss", "nempty",
                         "nzero", "npos", "HHI")
    bad_types <- setdiff(agg_type, valid_agg_types)
    if (length(bad_types) > 0)
      stop(paste0("unknown agg_type: ", paste(bad_types, collapse = ", ")))

    DTout <- data.table::copy(DT)

    var_list <- eval(substitute(var_list), parent.frame())
    check_char_vec(var_list, "var_list")

    # Error message for nempty
    if ("nempty" %in% agg_type &&
        length(var_list[vapply(DTout[, var_list, with = FALSE], is.character,
                               FUN.VALUE = logical(1))]) == 0) {
      stop("nempty can only be applied to character variables")
    }

    # Flag if users use variables in var_list that aren't present in DTout
    if (any(!var_list %in% names(DTout))) {
      stop(paste0(
        "Variable(s) ",
        ifelse(length(var_list[!var_list %in% names(DTout)]) > 1,
               paste(var_list[!var_list %in% names(DTout)], collapse = ", "),
               var_list[!var_list %in% names(DTout)]),
        ' in "var_list" cannot be found in DTout'
      ))
    }

    # Flag if users use variables in bygroups that aren't present in DTout
    if (any(!bygroups %in% names(DTout))) {
      stop(paste0(
        "Variable(s) ",
        ifelse(length(bygroups[!bygroups %in% names(DTout)]) > 1,
               paste(bygroups[!bygroups %in% names(DTout)], collapse = ", "),
               bygroups[!bygroups %in% names(DTout)]),
        ' in "bygroups" cannot be found in DTout'
      ))
    }

    # Flag if users want to compute the number of firms but there's no firm identifier in DT
    firm_cols <- c("plantid", "firmid", "entid", "entgrp")
    firm_col_present <- intersect(firm_cols, names(DT))
    if (count_firms == TRUE && length(firm_col_present) > 0) {
      count_firms <- TRUE
      firm_col <- firm_col_present[1]
    } else if (count_firms == TRUE && length(firm_col_present) == 0) {
      stop("No firm identifier column found in DT. Please provide 'num_firms' argument indicating the total number of unique firms present in the regression.")
    } else {
      rm(firm_col_present, firm_cols)
    }

    if (mrg == FALSE) {
      # return data.table with aggregate statistics

      DTagg <- Reduce(
        merge,
        lapply(agg_type, function(y) {
          # numerical vars in varlist
          numerics <- var_list[var_list %in% names(DTout)[vapply(DTout, is.numeric, FUN.VALUE = logical(1))]]
          ansvar_list_num <- paste(y, numerics, sep = "_")

          # character vars in varlist
          nonnumerics <- var_list[var_list %in% names(DTout)[vapply(DTout, is.character, FUN.VALUE = logical(1))]]
          ansvar_list_nonnum <- paste(y, nonnumerics, sep = "_")

          ansvar_list <- paste(y, var_list, sep = "_")

          # total unique firms across DTout (only if count_firms)
          NumFirms_val <- if (count_firms) data.table::uniqueN(DTout[[firm_col]]) else NULL

          add_numfirms <- function(dt) {
            if (count_firms) dt[, ("NumFirms") := NumFirms_val]
            return(dt)
          }

          if (y %in% c("mean", "sum", "sd")) {
            if (length(numerics)) {
              if (y == "mean" && !is.null(weight_col)) {
                tmp <- DTout[, lapply(.SD, function(x) weighted.mean(x, w = get(weight_col), na.rm = TRUE)),
                  by = bygroups, .SDcols = numerics
                ]
              } else if (y == "sum" && !is.null(weight_col)) {
                tmp <- DTout[, lapply(.SD, function(x) sum(x * get(weight_col), na.rm = TRUE)),
                  by = bygroups, .SDcols = numerics
                ]
              } else {
                tmp <- DTout[, lapply(.SD, function(x) match.fun(y)(x, na.rm = TRUE)),
                  by = bygroups, .SDcols = numerics
                ]
              }
              data.table::setnames(tmp, numerics, ansvar_list_num)
              return(add_numfirms(tmp))
            }
          } else if (y %in% c("q10", "q25", "median", "q75", "q90")) {
            quant_val <- switch(y,
              q10    = 0.1,
              q25    = 0.25,
              median = 0.50,
              q75    = 0.75,
              q90    = 0.9
            )
            if (length(numerics)) {
              tmp <- DTout[,
                {
                  lapply(.SD, function(x) {
                    x <- sort(x[!is.na(x)])
                    if (length(x) == 0) {
                      return(NA_real_)
                    }
                    q_val <- quantile(x, probs = quant_val, na.rm = TRUE, type = 7)
                    closest_idx <- which.min(abs(x - q_val))
                    n <- minNumObs
                    if (n %% 2 == 1) {
                      n_below <- floor(n / 2)
                      n_above <- floor(n / 2)
                    } else {
                      n_below <- (n / 2) - 1
                      n_above <- (n / 2)
                    }
                    window_idx <- (closest_idx - n_below):(closest_idx + n_above)
                    window_idx <- window_idx[window_idx >= 1 & window_idx <= length(x)]
                    if (length(window_idx) < minNumObs) {
                      message("mdi_aggregate: ", y, " set to NA -- only ", length(window_idx),
                              " value(s) in window, fewer than minNumObs (", minNumObs, ").")
                      return(NA_real_)
                    }
                    mean(x[window_idx], na.rm = TRUE)
                  })
                },
                by = bygroups,
                .SDcols = numerics
              ]
              data.table::setnames(tmp, numerics, ansvar_list_num)
              return(add_numfirms(tmp))
            }
          } else if (y == "count") {
            tmp <- DTout[, lapply(.SD, function(x) sum(!is.na(x))),
              by = bygroups, .SDcols = var_list
            ]
            data.table::setnames(tmp, var_list, ansvar_list)
            return(add_numfirms(tmp))
          } else if (y == "nmiss") {
            if (length(numerics)) {
              tmp <- DTout[, lapply(.SD, function(x) sum(is.na(x))),
                by = bygroups, .SDcols = numerics
              ]
              data.table::setnames(tmp, numerics, ansvar_list_num)
              return(add_numfirms(tmp))
            }
          } else if (y == "n_nonmiss") {
            if (length(numerics)) {
              tmp <- DTout[, lapply(.SD, function(x) sum(!is.na(x) & x != "")),
                by = bygroups, .SDcols = numerics
              ]
              data.table::setnames(tmp, numerics, ansvar_list_num)
              return(add_numfirms(tmp))
            }
          } else if (y == "nempty") {
            if (length(nonnumerics)) {
              tmp <- DTout[, lapply(.SD, function(x) sum(x == "")),
                by = bygroups, .SDcols = nonnumerics
              ]
              data.table::setnames(tmp, nonnumerics, ansvar_list_nonnum)
              return(add_numfirms(tmp))
            }
          } else if (y == "nzero") {
            if (length(numerics)) {
              tmp <- DTout[, lapply(.SD, function(x) sum(x == 0, na.rm = TRUE)),
                by = bygroups, .SDcols = numerics
              ]
              data.table::setnames(tmp, numerics, ansvar_list_num)
              return(add_numfirms(tmp))
            }
          } else if (y == "npos") {
            if (length(numerics)) {
              tmp <- DTout[, lapply(.SD, function(x) sum(x > 0, na.rm = TRUE)),
                by = bygroups, .SDcols = numerics
              ]
              data.table::setnames(tmp, numerics, ansvar_list_num)
              return(add_numfirms(tmp))
            }
          } else if (y == "HHI") {
            if (length(numerics)) {
              tmp <- DTout[, lapply(.SD, function(x) sum((x / sum(x, na.rm = TRUE))^2, na.rm = TRUE)),
                by = bygroups, .SDcols = numerics
              ]
              data.table::setnames(tmp, numerics, ansvar_list_num)
              return(add_numfirms(tmp))
            }
          }
        })
      )

      # Apply disclosure checks
      if (disclosure == TRUE) {
        DTdisc <- mdi_disclose_crit(DTout, bygroups = bygroups, var_list = var_list,
                                    dom_formula = dom_formula, count_firms = count_firms)
        DTagg <- merge(DTagg, DTdisc, by = bygroups)
      }

      data.table::setkeyv(DTagg, bygroups)
      data.table::setattr(DTagg, "sumvars", var_list)

      return(DTagg)
    }


    # return original data.table and added aggregate statistics

    if (mrg == TRUE) {
      for (y in agg_type) {
        # numerical vars in varlist
        numerics <- var_list[var_list %in% colnames(DTout)[colnames(DTout) %in% colnames(DTout[, which(vapply(DTout, is.numeric, FUN.VALUE = logical(1))), with = FALSE])]]
        ansvar_list_num <- paste(y, numerics, sep = "_")

        # character vars in varlist
        nonnumerics <- var_list[var_list %in% colnames(DTout)[colnames(DTout) %in% colnames(DTout[, which(vapply(DTout, is.character, FUN.VALUE = logical(1))), with = FALSE])]]
        ansvar_list_nonnum <- paste(y, nonnumerics, sep = "_")

        # all vars in varlist
        ansvar_list <- paste(y, var_list, sep = "_")

        if (y == "mean" | y == "sum" | y == "sd") {
          if (!length(numerics) == 0) {
            if (y == "mean" && !is.null(weight_col)) {
              DTout[, (ansvar_list_num) := lapply(.SD, function(x) {
                weighted.mean(x, w = get(weight_col), na.rm = TRUE)
              }), by = bygroups, .SDcols = numerics]
            } else if (y == "sum" && !is.null(weight_col)) {
              DTout[, (ansvar_list_num) := lapply(.SD, function(x) {
                sum(x * get(weight_col), na.rm = TRUE)
              }), by = bygroups, .SDcols = numerics]
            } else {
              DTout[, (ansvar_list_num) := lapply(.SD, function(x) {
                match.fun(y)(x, na.rm = TRUE)
              }), by = bygroups, .SDcols = numerics]
            }
          }
        } else if (y %in% c("q10", "q25", "median", "q75", "q90")) {
          if (y == "q10") {
            quant_val <- 0.1
          } else if (y == "q25") {
            quant_val <- 0.25
          } else if (y == "median") {
            quant_val <- 0.50
          } else if (y == "q75") {
            quant_val <- 0.75
          } else if (y == "q90") {
            quant_val <- 0.9
          }
          if (!length(numerics) == 0) {
            DTout[, (ansvar_list) := lapply(.SD, function(x) {
              x <- sort(x[!is.na(x)])
              if (length(x) == 0) {
                return(NA_real_)
              }

              q_val <- quantile(x, probs = quant_val, na.rm = TRUE, type = 7)
              closest_idx <- which.min(abs(x - q_val))

              # Determine number of values below and above
              n <- minNumObs
              if (n %% 2 == 1) {
                n_below <- floor(n / 2)
                n_above <- floor(n / 2)
                window_idx <- (closest_idx - n_below):(closest_idx + n_above)
              } else {
                n_below <- (n / 2) - 1
                n_above <- (n / 2)
                window_idx <- (closest_idx - n_below):(closest_idx + n_above)
              }

              # Clip to valid bounds
              window_idx <- window_idx[window_idx >= 1 & window_idx <= length(x)]
              if (length(window_idx) < minNumObs) {
                message("mdi_aggregate: ", y, " set to NA -- only ", length(window_idx),
                        " value(s) in window, fewer than minNumObs (", minNumObs, ").")
                return(NA_real_)
              }
              mean(x[window_idx], na.rm = TRUE)
            }), by = bygroups, .SDcols = numerics]
          }
        } else if (y == "count") {
          DTout[, (ansvar_list) := lapply(.SD, function(x) {
            sum(!is.na(x))
          }), by = bygroups, .SDcols = var_list]
        } else if (y == "nmiss") {
          if (!length(numerics) == 0) {
            DTout[, (ansvar_list) := lapply(.SD, function(x) {
              sum(is.na(x))
            }), by = bygroups, .SDcols = numerics]
          }
        } else if (y == "n_nonmiss") {
          if (length(numerics)) {
            DTout[, (ansvar_list_num) := lapply(.SD, function(x) {
              sum(!is.na(x) & x != "")
            }), by = bygroups, .SDcols = numerics]
          }
        } else if (y == "nempty") {
          if (!length(nonnumerics) == 0) {
            DTout[, (ansvar_list) := lapply(.SD, function(x) {
              sum(x == "")
            }), by = bygroups, .SDcols = nonnumerics]
          }
        } else if (y == "nzero") {
          if (!length(numerics) == 0) {
            DTout[, (ansvar_list_num) := lapply(.SD, function(x) {
              sum(x == 0, na.rm = TRUE)
            }), by = bygroups, .SDcols = numerics]
          }
        } else if (y == "npos") {
          if (!length(numerics) == 0) {
            DTout[, (ansvar_list_num) := lapply(.SD, function(x) {
              sum(x > 0, na.rm = TRUE)
            }), by = bygroups, .SDcols = numerics]
          }
        } else if (y == "HHI") {
          if (!length(numerics) == 0) {
            DTout[, (ansvar_list) := lapply(.SD, function(x) {
              sum((x / sum(x, na.rm = TRUE) * 100)^2, na.rm = TRUE)
            }), by = bygroups, .SDcols = numerics]
          }
        }
      }
      return(DTout)
    }
  }