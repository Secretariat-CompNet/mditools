#' ACF Production Function Estimation
#'
#' @description
#' Estimates a production function using the Ackerberg-Caves-Frazer (ACF)
#' method (Ackerberg, Caves & Frazer 2015, Econometrica). Runs a two-stage
#' GMM procedure: a first-stage OLS (with optional polynomial and time fixed
#' effects) to recover the productivity proxy Phi, followed by GMM
#' minimization to identify input elasticities.
#'
#' tfp = (Phi or y) - X*beta depending on \code{TFP_minuend}; with
#' \code{TFP_minuend = "y"} this matches the y - X*beta convention of the
#' other estimators.
#'
#' @param DT A \code{data.table} (or coercible object) containing the panel data.
#' @param y Character. Name of the output variable column.
#' @param endog Character vector. Names of endogenous input columns (e.g. labour).
#' @param exog Character vector. Names of exogenous input columns (e.g. capital).
#' @param instr Character vector. Names of instrument columns for the first
#'   stage polynomial.
#' @param id Character. Name of the firm/unit identifier column.
#' @param time Character. Name of the time period column.
#' @param spec Character. Functional form. Only \code{"cd"} (Cobb-Douglas) is
#'   implemented. Default \code{"cd"}.
#' @param degree Integer. Degree of the polynomial used in the first stage and
#'   the Omega law of motion. Default \code{3}.
#' @param lower_bound_theta Numeric. Lower bound for elasticity estimates in
#'   the GMM optimisation. Default \code{0}.
#' @param upper_bound_theta Numeric. Upper bound for elasticity estimates in
#'   the GMM optimisation. Default \code{1}.
#' @param TFP_demeaned Logical. If \code{TRUE}, TFP is demeaned by subtracting
#'   the period mean (using \code{mdi_aggregate}). Default \code{TRUE}.
#' @param TFP_minuend Character. Whether TFP is computed as residual from
#'   \code{"Phi"} (first-stage fitted values) or \code{"y"} (raw output).
#'   Default \code{"Phi"}.
#' @param Omega_estimates Logical. If \code{TRUE}, attaches the estimated
#'   law-of-motion parameters (\code{g_b_slopes}, \code{g_b_intercept}) to the
#'   output. Default \code{TRUE}.
#' @param time_FE Logical. If \code{TRUE}, period dummies are included in the
#'   first stage regression. Default \code{FALSE}.
#' @param extended_instr Logical. If \code{TRUE}, augments the second-stage
#'   instrument set from \code{{k_t, l_{t-1}}} to
#'   \code{{k_t, l_{t-1}, Phi_hat_{t-1}}} as in ACF (2015) eq (28). Produces
#'   overidentification by one moment and enables a Hansen J test. Useful as a
#'   robustness check when the default exactly-identified system hits the
#'   optimiser bounds. Default \code{FALSE}.
#'
#' @return A \code{data.table} with one row per observation in the GMM sample,
#'   containing:
#' - \code{id}, \code{time} columns (using the names supplied)
#' - \code{tfp}: total factor productivity
#' - \code{el_<input>}: estimated input elasticity for each input
#' - \code{TFP_demeaned} (if \code{TFP_demeaned = TRUE}): period-demeaned TFP
#' - \code{g_b_slopes}, \code{g_b_intercept} (if \code{Omega_estimates = TRUE})
#' - \code{NumObs}: number of observations used in the GMM stage
#' - \code{convergence}: optimiser convergence code (0 = converged)
#'
#' @examples
#' \donttest{
#' library(data.table)
#' set.seed(1)
#' n <- 200
#' DT <- data.table(
#'   id   = rep(1:50, each = 4),
#'   year = rep(2000:2003, times = 50),
#'   y    = rnorm(n, 5, 1),
#'   l    = rnorm(n, 3, 0.5),
#'   k    = rnorm(n, 4, 0.5),
#'   m    = rnorm(n, 2, 0.5)
#' )
#' result <- mdi_acf_prodest(
#'   DT, y = "y", endog = "l", exog = "k", instr = "m",
#'   id = "id", time = "year", degree = 2, TFP_demeaned = FALSE
#' )
#' }
#'
#' @export

mdi_acf_prodest <- function(DT,
                            y,
                            endog,
                            exog,
                            instr,
                            id,
                            time,
                            spec = "cd",
                            degree = 3,
                            lower_bound_theta = 0,
                            upper_bound_theta = 1,
                            TFP_demeaned = TRUE,
                            TFP_minuend = c("Phi", "y"),
                            Omega_estimates = TRUE,
                            time_FE = FALSE,
                            extended_instr = FALSE) {

  check_string(y,    "y")
  check_string(id,   "id")
  check_string(time, "time")
  check_char_vec(endog, "endog")
  check_char_vec(exog,  "exog")
  check_char_vec(instr, "instr")
  check_dt(DT, c(y, endog, exog, instr, id, time))
  check_choice(spec, "spec", "cd")
  TFP_minuend <- match.arg(TFP_minuend)

  # --------- Data prep ---------
  DT <- data.table::copy(DT)
  data.table::setkeyv(DT, c(id, time))

  Y  <- as.matrix(DT[[y]])
  fX <- as.matrix(DT[, endog, with = FALSE])
  sX <- as.matrix(DT[, exog,  with = FALSE])
  pX <- as.matrix(DT[, instr, with = FALSE])

  polyframe <- poly(fX, sX, pX, degree = degree, raw = TRUE)
  regvars   <- cbind(fX, sX, pX, polyframe)

  # --------- FIRST STAGE with optional time FE ---------
  if (time_FE) {
    full_reg_dt <- data.table::data.table(
      Y       = as.numeric(Y),
      regvars,
      time_FE = factor(DT[[time]])
    )
  } else {
    full_reg_dt <- data.table::data.table(
      Y = as.numeric(Y),
      regvars
    )
  }

  keep_rows   <- complete.cases(full_reg_dt)
  full_reg_dt <- full_reg_dt[keep_rows]

  first_stage <- lm(Y ~ ., data = as.data.frame(full_reg_dt),
                    na.action = na.exclude)

  DT <- DT[keep_rows]
  DT[, ("Phi") := fitted(first_stage)]

  id_vec   <- DT[[id]]
  time_vec <- DT[[time]]
  phi_col  <- DT[["Phi"]]

  DT[, ("Phi_lag") := panel_lag(phi_col, id_vec, time_vec)]

  # --------- Build lags and matrices for GMM ---------
  theta_names <- c(endog, exog)

  lag_mat <- vapply(DT[, theta_names, with = FALSE],
                    panel_lag,
                    FUN.VALUE = numeric(nrow(DT)),
                    id_vec   = id_vec,
                    time_vec = time_vec)
  DT[, (paste0(theta_names, "_lag")) := data.table::as.data.table(lag_mat)]

  X       <- as.matrix(DT[, theta_names, with = FALSE])
  lX      <- as.matrix(DT[, paste0(theta_names, "_lag"), with = FALSE])
  phi     <- DT[["Phi"]]
  phi_lag <- DT[["Phi_lag"]]

  # Default Z = {k_t, l_{t-1}} (exactly identified). With extended_instr,
  # add Phi_hat_{t-1} for one overidentifying moment (ACF eq 28).
  instr_vars <- c(exog, paste0(endog, "_lag"))
  if (extended_instr) instr_vars <- c(instr_vars, "Phi_lag")
  Z <- as.matrix(DT[, instr_vars, with = FALSE])

  tmp_df <- data.frame(
    Z      = I(Z),
    X      = I(X),
    lX     = I(lX),
    phi    = phi,
    lagphi = phi_lag
  )

  mf     <- model.frame(Z ~ X + lX + phi + lagphi, data = tmp_df,
                        na.action = na.omit)
  numobs <- nrow(mf)

  Z       <- mf$Z
  X       <- mf$X
  lX      <- mf$lX
  phi     <- mf$phi
  phi_lag <- mf$lagphi

  # --------- Initial theta from first stage (Cobb-Douglas) ---------
  phi_inputs <- c(endog, exog)
  theta0     <- coef(first_stage)[2:(1 + length(phi_inputs))]

  W <- solve(crossprod(Z)) / nrow(Z)

  # --------- GMM objective ---------
  gACF <- function(theta, Z, X, lX, phi, phi_lag, A) {
    Omega     <- phi     - X  %*% theta
    Omega_lag <- phi_lag - lX %*% theta

    Omega_lag_poly <- poly(Omega_lag, degree = A, raw = TRUE)
    Omega_lag_poly <- cbind(1, Omega_lag_poly)

    g_b <- solve(t(Omega_lag_poly) %*% Omega_lag_poly) %*%
      t(Omega_lag_poly) %*% Omega

    XI   <- Omega - Omega_lag_poly %*% g_b
    crit <- t(crossprod(Z, XI)) %*% W %*% (crossprod(Z, XI))
    as.numeric(crit)
  }

  lower_bounds <- rep(lower_bound_theta, length(phi_inputs))
  upper_bounds <- rep(upper_bound_theta, length(phi_inputs))

  gmm_out <- optim(
    par     = theta0,
    fn      = gACF,
    Z       = Z,
    X       = X,
    lX      = lX,
    phi     = phi,
    phi_lag = phi_lag,
    A       = degree,
    method  = "L-BFGS-B",
    lower   = lower_bounds,
    upper   = upper_bounds,
    control = list(maxit = 2000)
  )

  if (gmm_out$convergence != 0)
    warning("ACF second stage: optimizer did not report convergence (code ",
            gmm_out$convergence, ").")

  elasticities <- gmm_out$par

  # --------- Recover Omega and g_b ---------
  Omega_hat     <- phi     - X  %*% elasticities
  Omega_lag_hat <- phi_lag - lX %*% elasticities

  Omega_lag_poly <- poly(Omega_lag_hat, degree = degree, raw = TRUE)
  Omega_lag_poly <- cbind(1, Omega_lag_poly)

  g_b_hat       <- solve(t(Omega_lag_poly) %*% Omega_lag_poly) %*%
    t(Omega_lag_poly) %*% Omega_hat
  g_b_intercept <- g_b_hat[1]
  g_b_slopes    <- g_b_hat[-1]

  # --------- Build output DT aligned with mf rows ---------
  y_hat    <- X %*% elasticities
  keep_idx <- as.integer(row.names(mf))
  DT_keep  <- DT[keep_idx]

  DT_out <- setNames(
    data.table::data.table(DT_keep[[id]], DT_keep[[time]], DT_keep[[y]], DT_keep[["Phi"]]),
    c(id, time, y, "Phi")
  )

  if (TFP_minuend == "y") {
    minuend <- y
  } else {
    minuend <- "Phi"
  }

  DT_out[, ("tfp") := .SD - y_hat, .SDcols = minuend]
  DT_out[, (y) := NULL]
  DT_out[, ("Phi") := NULL]

  for (j in seq_along(phi_inputs)) {
    DT_out[, (paste0("el_", phi_inputs[j])) := elasticities[j]]
  }

  ord    <- do.call(order, DT_out[, .SD, .SDcols = c(time, id)])
  DT_out <- DT_out[ord]

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

  if (Omega_estimates) {
    DT_out[, ("g_b_slopes")    := paste(g_b_slopes, collapse = " ; ")]
    DT_out[, ("g_b_intercept") := g_b_intercept]
  }

  DT_out[, ("NumObs")      := numobs]
  DT_out[, ("convergence") := as.integer(gmm_out$convergence)]

  return(DT_out)
}
