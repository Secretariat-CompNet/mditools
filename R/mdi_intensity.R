#' Compute Technology Adoption Intensity via Probit Propensities
#'
#' @description
#' Reduces a set of binary adoption indicators to a single continuous intensity
#' score using probit regressions. For each boolean indicator a probit model is
#' fitted with continuous firm-level predictors and optional fixed effects. The
#' predicted propensities are then combined into a single intensity score via
#' the geometric mean.
#'
#' @param DT A data.table containing the input data.
#' @param uniqdim Character vector of column names that uniquely identify each
#'   observation (e.g. `c("firmid", "year")`).
#' @param boollist Character vector of column names for binary adoption
#'   indicators (0/1). A probit propensity is estimated for each.
#' @param contlist Character vector of column names for continuous firm-level
#'   predictors used in each probit model.
#' @param fe Character vector of column names to include as factor fixed
#'   effects in each probit model (e.g. `c("nace2", "year")`).
#' @return A data.table keyed on `uniqdim` with one additional column
#'   `intens_probit`: the geometric mean of all predicted propensities.
#' @export
#' @examples
#' \donttest{
#' library(data.table)
#' set.seed(1)
#' n <- 100
#' DT <- data.table(
#'   firmid = seq_len(n),
#'   year   = sample(2010:2012, n, replace = TRUE),
#'   bool1  = sample(0L:1L, n, replace = TRUE),
#'   cont1  = rnorm(n),
#'   cont2  = rnorm(n)
#' )
#' mdi_intensity(DT, uniqdim = "firmid", boollist = "bool1",
#'           contlist = c("cont1", "cont2"), fe = "year")
#' }

mdi_intensity <- function(DT, uniqdim, boollist, contlist, fe) {

  check_char_vec(uniqdim,  "uniqdim")
  check_char_vec(boollist, "boollist")
  check_char_vec(contlist, "contlist")
  check_char_vec(fe,       "fe")
  check_dt(DT, c(uniqdim, boollist, contlist, fe), arg_name = "DT")

  dbout <- DT[, c(uniqdim, boollist, contlist, fe), with = FALSE]

  for (bool in boollist) {
    formula <- as.formula(paste(
      paste(bool, "~"),
      paste(
        paste(contlist, collapse = "+"),
        paste("as.factor(", fe, ")", collapse = "+"),
        sep = "+"
      )
    ))

    dbout[, paste0(bool, "prob") := predict(
      glm(formula, family = binomial(link = "probit"), data = .SD,
          na.action = na.exclude),
      type = "response"
    )]
  }

  dbout[, ("intens_probit") := exp(rowMeans(log(.SD), na.rm = TRUE)),
        .SDcols = paste0(boollist, "prob")]

  dbout <- dbout[, c(uniqdim, "intens_probit"), with = FALSE]

  data.table::setkeyv(dbout, uniqdim)

  return(dbout)
}