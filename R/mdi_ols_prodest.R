#' OLS Production Function Estimator
#'
#' @description
#' Fits a Cobb-Douglas production function by OLS on the free and state inputs
#' together with a degree-G polynomial in the same inputs, following the
#' prodest-package convention. Returns TFP as the residual between output and
#' the fitted input index, with optional period demeaning.
#'
#' Intended as a naive baseline for the simultaneity-bias comparison against
#' the proxy-variable estimators. NOTE: at \code{degree > 1} the reported
#' linear-input coefficients are partial regression coefficients conditional
#' on the higher-order polynomial terms, not structural elasticities. Use
#' \code{degree = 1} for the clean naive-OLS baseline.
#'
#' tfp = y - X*beta (same convention as ACF/LP/WDRG/dpGMM).
#'
#' @param DT A \code{data.table} (or coercible object) containing panel data.
#' @param y Character scalar. Name of the output variable column.
#' @param endog Character vector. Names of the free (endogenous) input columns
#'   (e.g. log labour).
#' @param exog Character vector. Names of the state input columns (e.g. log
#'   capital).
#' @param id Character scalar. Name of the firm/unit identifier column.
#' @param time Character scalar. Name of the time period column.
#' @param spec Character. Functional form. Only \code{"cd"} (Cobb-Douglas) is
#'   implemented. Default \code{"cd"}.
#' @param degree Integer. Polynomial degree for the input polynomial. Use 1 for
#'   the true naive baseline. Default \code{3}.
#' @param TFP_demeaned Logical. If \code{TRUE}, TFP is demeaned by subtracting
#'   the period mean (via \code{mdi_aggregate}). Default \code{TRUE}.
#'
#' @return A \code{data.table} with one row per observation in the estimation
#'   sample, containing:
#' - \code{id}, \code{time} columns (using the names supplied)
#' - \code{tfp}: total factor productivity (OLS residual)
#' - \code{el_<endog>}, \code{el_<exog>}: estimated input elasticities
#' - \code{TFP_demeaned} (if \code{TFP_demeaned = TRUE}): period-demeaned TFP
#' - \code{NumObs}: number of observations used
#'
#' @examples
#' \donttest{
#' library(data.table)
#' set.seed(3)
#' n <- 200
#' DT <- data.table(
#'   id   = rep(1:50, each = 4),
#'   year = rep(2000:2003, times = 50),
#'   y    = rnorm(n, 5, 1),
#'   l    = rnorm(n, 3, 0.5),
#'   k    = rnorm(n, 4, 0.5)
#' )
#' result <- mdi_ols_prodest(DT, y = "y", endog = "l", exog = "k",
#'                           id = "id", time = "year", degree = 2,
#'                           TFP_demeaned = FALSE)
#' }
#'
#' @export

mdi_ols_prodest <- function(DT,
                            y,
                            endog,
                            exog,
                            id,
                            time,
                            spec         = "cd",
                            degree       = 3,
                            TFP_demeaned = TRUE) {

  check_string(y,    "y")
  check_string(id,   "id")
  check_string(time, "time")
  check_char_vec(endog, "endog")
  check_char_vec(exog,  "exog")
  check_dt(DT, c(y, endog, exog, id, time))
  check_choice(spec, "spec", "cd")

  DT <- data.table::copy(DT)
  data.table::setkeyv(DT, c(id, time))

  Y  <- as.matrix(DT[[y]])
  fX <- as.matrix(DT[, endog, with = FALSE])
  sX <- as.matrix(DT[, exog,  with = FALSE])

  polyframe <- poly(fX, sX, degree = degree, raw = TRUE)
  regvars   <- cbind(fX, sX, polyframe)

  full_reg_dt <- data.table::data.table(Y = as.numeric(Y), regvars)
  keep_rows   <- complete.cases(full_reg_dt)
  full_reg_dt <- full_reg_dt[keep_rows]

  first_stage <- lm(Y ~ ., data = as.data.frame(full_reg_dt),
                    na.action = na.exclude)

  numobs <- nrow(full_reg_dt)

  phi_inputs   <- c(endog, exog)
  elasticities <- coef(first_stage)[2:(1 + length(phi_inputs))]
  theta_names  <- phi_inputs

  # Restrict to the first-stage sample BEFORE building X so dimensions align.
  DT_keep <- DT[keep_rows]
  X       <- as.matrix(DT_keep[, theta_names, with = FALSE])
  y_hat   <- X %*% elasticities

  DT_out <- setNames(
    data.table::data.table(DT_keep[[id]], DT_keep[[time]], DT_keep[[y]]),
    c(id, time, y)
  )

  DT_out[, ("tfp") := .SD - y_hat, .SDcols = y]
  DT_out[, (y) := NULL]

  if (TFP_demeaned) {
    DT_out <- mdi_aggregate(
      DT_out,
      var_list   = "tfp",
      bygroups   = time,
      agg_type   = "mean",
      disclosure = FALSE,
      mrg        = TRUE
    )
    DT_out[, ("TFP_demeaned") := .SD[["tfp"]] - .SD[["mean_tfp"]],
           .SDcols = c("tfp", "mean_tfp")]
    DT_out[, ("mean_tfp") := NULL]
  }

  for (j in seq_along(phi_inputs)) {
    DT_out[, (paste0("el_", phi_inputs[j])) := elasticities[j]]
  }

  DT_out[, ("NumObs") := numobs]

  ord <- do.call(order, DT_out[, .SD, .SDcols = c(time, id)])
  return(DT_out[ord])
}
