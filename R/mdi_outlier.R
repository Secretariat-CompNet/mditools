#' Outlier Routine for Data Cleaning
#'
#' @description
#' Executes a specified outlier handling routine (trimming, winsorizing, or flagging) on a dataset for selected continuous variables. This function can trim or winsorize the data at specified quantiles or flag observations as outliers based on the fraction provided. Trimming replaces outliers with NA, winsorizing replaces outliers with the closest value within the non-outlier range, and flagging marks outliers with a flag variable.
#'
#' @param DT A `data.table` containing the variables to process.
#' @param var_list A character vector of continuous variable names in `DT` to be processed by the outlier routine.
#' @param routine A string specifying the outlier routine to apply: `"trim"`, `"winsorize"`, or `"flag"`. Default `"trim"`.
#' @param fraction The fraction of data to be trimmed or winsorized; must be a numeric value between 0 and 1.
#' @param both_tails Logical indicating whether to apply the routine to both tails of the distribution. If `TRUE`, the operation affects both the upper and lower tails; otherwise, it affects only the upper tail. Default `FALSE`.
#' @param group An optional character string naming a grouping variable in `DT`. When supplied, the outlier routine is applied within each group. Default `NULL`.
#' @return A modified copy of `DT` with the outlier routine applied to the specified variables. If `routine = "flag"`, new columns named `flag_<var>` are added (value `1` for flagged observations, `NA` otherwise).
#' @examples
#' library(data.table)
#' DT <- data.table(id = 1:10, income = c(50, 55, 45, 60, 200, 40, 45, 55, 65, 1000))
#' mdi_outlier(DT, "income", "trim", 0.1, both_tails = TRUE)
#' mdi_outlier(DT, "income", "winsorize", 0.1)
#' mdi_outlier(DT, "income", "flag", 0.1)
#' @export


mdi_outlier <- function(DT, var_list, routine = c("trim", "winsorize", "flag"),
                        fraction, both_tails = FALSE, group = NULL) {

  check_char_vec(var_list, "var_list")
  check_dt(DT, var_list)
  routine <- match.arg(routine)
  if (length(fraction) != 1 || !is.numeric(fraction) || fraction < 0 || fraction > 1)
    stop("'fraction' must be a single numeric value between 0 and 1")
  if (!is.null(group)) check_string(group, "group")

  dbout = data.table::copy(DT)

  if(routine=="winsorize"){

    dbout = dbout[, (var_list) := lapply(.SD, function(x){

      xq <- quantile(x, probs=c(fraction, 1-fraction), na.rm = TRUE)

      maxval <- xq[2L]
      x[x>maxval] <- maxval

      if((isTRUE(both_tails))){
        minval <- xq[1L]
        x[x<minval] <- minval
      }

      return(x)

    }), .SDcols = var_list, by=group]

  }


  if(routine=="trim"){

    dbout = dbout[, (var_list) := lapply(.SD, function(x){

      xq <- quantile(x, probs=c(fraction, 1-fraction), na.rm = TRUE)

      maxval <- xq[2L]
      x[x>maxval] <- NA

      if(isTRUE(both_tails)){
        minval <- xq[1L]
        x[x<minval] <- NA
      }

      return(x)

    }), .SDcols = var_list, by=group]

  }

  if(routine=="flag"){

    for(var in var_list){
      flag_name <- paste0("flag_",var)
      .col <- dbout[[var]]
      dbout[.col > quantile(.col, probs=1-fraction, na.rm = TRUE), (flag_name) := 1, by=group]
      if(isTRUE(both_tails)){
        dbout[.col < quantile(.col, probs=fraction, na.rm = TRUE), (flag_name) := 1, by=group]
      }
    }
  }

  return(dbout)
}
