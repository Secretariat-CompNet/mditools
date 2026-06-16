#' Levinsohn-Petrin (2003) Production Function Estimator
#'
#' @description
#' Two-stage proxy-variable estimator using an intermediate input (typically
#' materials) to control for unobserved productivity. First stage: regress y
#' on a flexible polynomial in (state, proxy) plus the free input, recovering
#' Phi. Second stage: GMM on the law-of-motion residual identifying the state
#' elasticities. Reference: Levinsohn & Petrin (2003, ReStud).
#'
#' tfp = (y or Phi) - X*beta, depending on \code{TFP_minuend}. Defaults to
#' \code{"y"} to match the convention shared by the other estimators.
#'
#' @param DT A \code{data.table} (or coercible object) containing panel data.
#' @param y Character. Name of the output variable column.
#' @param endog Character vector. Names of endogenous input columns (e.g. labour).
#' @param exog Character vector. Names of exogenous input columns (e.g. capital).
#' @param instr Character vector. Names of proxy/instrument columns (e.g. materials).
#' @param id Character. Name of the firm/unit identifier column.
#' @param time Character. Name of the time period column.
#' @param spec Character. Functional form. Only \code{"cd"} implemented. Default \code{"cd"}.
#' @param degree Integer. Polynomial degree in proxy function and law of motion. Default \code{3}.
#' @param lower_bound_theta Numeric. Lower bound for state elasticities. Default \code{0}.
#' @param upper_bound_theta Numeric. Upper bound for state elasticities. Default \code{1}.
#' @param TFP_demeaned Logical. If \code{TRUE}, TFP is demeaned by subtracting
#'   the period mean (via \code{mdi_aggregate}). Default \code{TRUE}.
#' @param TFP_minuend Character. \code{"y"} (default) or \code{"Phi"}.
#' @param Omega_estimates Logical. If \code{TRUE}, attaches law-of-motion
#'   parameters (\code{g_b_slopes}, \code{g_b_intercept}). Default \code{TRUE}.
#' @param time_FE Logical. If \code{TRUE}, period dummies enter the first stage. Default \code{FALSE}.
#'
#' @return A \code{data.table} with one row per observation in the GMM sample,
#'   containing:
#' - \code{id}, \code{time} columns (using the names supplied)
#' - \code{tfp}: total factor productivity
#' - \code{el_<input>}: estimated input elasticities
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
#' result <- mdi_lp_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
#'                          id = "id", time = "year", degree = 2,
#'                          TFP_demeaned = FALSE)
#' }
#'
#' @export

mdi_lp_prodest <- function(DT,
                           y,
                           endog,
                           exog,
                           instr,
                           id,
                           time,
                           spec              = "cd",
                           degree            = 3,
                           lower_bound_theta = 0,
                           upper_bound_theta = 1,
                           TFP_demeaned      = TRUE,
                           TFP_minuend       = c("y", "Phi"),
                           Omega_estimates   = TRUE,
                           time_FE           = FALSE) {

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

  base_vars <- c(y, id, time, endog, exog, instr)
  DT <- DT[complete.cases(DT[, base_vars, with = FALSE])]

  Y  <- as.numeric(DT[[y]])
  fX <- as.matrix(DT[, endog, with = FALSE])
  sX <- as.matrix(DT[, exog,  with = FALSE])
  pX <- as.matrix(DT[, instr, with = FALSE])

  # --------- First stage: polynomial in (state, proxy) ---------
  polyframe <- poly(
    as.matrix(DT[, c(exog, instr), with = FALSE]),
    degree = degree,
    raw    = TRUE
  )
  regvars <- cbind(fX, sX, pX, polyframe)

  if (time_FE) {
    full_reg_dt <- data.table::data.table(
      Y       = Y,
      regvars,
      time_FE = factor(DT[[time]])
    )
  } else {
    full_reg_dt <- data.table::data.table(Y = Y, regvars)
  }

  keep_rows   <- complete.cases(full_reg_dt)
  full_reg_dt <- full_reg_dt[keep_rows]

  first_stage <- lm(Y ~ ., data = as.data.frame(full_reg_dt),
                    na.action = na.exclude)

  # Restrict DT and matrices to the first-stage sample
  DT <- DT[keep_rows]
  Y  <- as.numeric(DT[[y]])
  fX <- as.matrix(DT[, endog, with = FALSE])
  sX <- as.matrix(DT[, exog,  with = FALSE])
  pX <- as.matrix(DT[, instr, with = FALSE])

  phi     <- as.numeric(fitted(first_stage))
  fs_coef <- coef(first_stage)

  beta_free_init  <- unname(fs_coef[match(colnames(fX), names(fs_coef))])
  beta_state_init <- unname(fs_coef[match(colnames(sX), names(fs_coef))])

  if (any(is.na(beta_free_init)))
    stop("Could not find free-input coefficients in 1st-stage.")
  if (any(is.na(beta_state_init))) {
    warning("Some state-input starts are NA; setting them to 0.01.")
    beta_state_init[is.na(beta_state_init)] <- 0.01
  }

  beta_free_init  <- as.numeric(beta_free_init)
  beta_state_init <- as.numeric(beta_state_init)

  # --------- Residual objects for the OP/LP second stage ---------
  phi_clean <- as.numeric(phi - fX %*% beta_free_init)
  res       <- as.numeric(Y   - fX %*% beta_free_init)

  DT[, ("phi_clean") := phi_clean]

  id_vec   <- DT[[id]]
  time_vec <- DT[[time]]

  DT[, ("phi_lag") := panel_lag(phi_clean, id_vec, time_vec)]

  lag_sX_mat <- vapply(DT[, exog, with = FALSE], panel_lag,
                       FUN.VALUE = numeric(nrow(DT)),
                       id_vec = id_vec, time_vec = time_vec)
  DT[, (paste0(exog, "_lag")) := data.table::as.data.table(lag_sX_mat)]

  mX      <- as.matrix(DT[, exog, with = FALSE])
  mlX     <- as.matrix(DT[, paste0(exog, "_lag"), with = FALSE])
  vphi    <- as.numeric(DT[["phi_clean"]])
  vlagphi <- as.numeric(DT[["phi_lag"]])
  vres    <- as.numeric(res)

  tmp_df <- data.frame(
    mX      = I(mX),
    mlX     = I(mlX),
    vphi    = vphi,
    vlagphi = vlagphi,
    vres    = vres
  )

  mf <- model.frame(vphi ~ mX + mlX + vlagphi + vres,
                    data = tmp_df, na.action = na.omit)
  numobs <- nrow(mf)

  mX      <- mf$mX
  mlX     <- mf$mlX
  vphi    <- mf$vphi
  vlagphi <- mf$vlagphi
  vres    <- mf$vres

  keep_idx <- as.integer(row.names(mf))
  DT_sub <- DT[keep_idx]
  Y_sub  <- as.numeric(DT_sub[[y]])
  fX_sub <- as.matrix(DT_sub[, endog, with = FALSE])
  sX_sub <- as.matrix(DT_sub[, exog,  with = FALSE])

  # --------- Second-stage objective ---------
  gLP <- function(vtheta, mX, mlX, vphi, vlag.phi, vres, degree) {
    Omega     <- vphi     - mX  %*% vtheta
    Omega_lag <- vlag.phi - mlX %*% vtheta

    Omega_lag_scaled <- scale(Omega_lag)
    Omega_lag_pol <- poly(Omega_lag_scaled, degree = degree, raw = TRUE)
    Omega_lag_pol <- cbind(1, Omega_lag_pol)

    XtX <- crossprod(Omega_lag_pol)
    XtY <- crossprod(Omega_lag_pol, Omega)
    g_b <- solve(XtX, XtY)

    XI <- vres - (mX %*% vtheta) - (Omega_lag_pol %*% g_b)
    as.numeric(crossprod(XI))
  }

  theta0 <- beta_state_init
  if (length(theta0) != ncol(mX)) theta0 <- rep_len(theta0, ncol(mX))
  theta0 <- as.numeric(theta0)

  lower_bounds <- rep(lower_bound_theta, length(theta0))
  upper_bounds <- rep(upper_bound_theta, length(theta0))

  opt <- optim(
    par      = theta0,
    fn       = gLP,
    mX       = mX,
    mlX      = mlX,
    vphi     = vphi,
    vlag.phi = vlagphi,
    vres     = vres,
    degree   = degree,
    method   = "L-BFGS-B",
    lower    = lower_bounds,
    upper    = upper_bounds,
    control  = list(maxit = 1000)
  )

  if (opt$convergence != 0)
    warning("LP second stage: optimizer did not report convergence.")

  beta_state <- as.numeric(opt$par)
  beta_free  <- beta_free_init

  # --------- Recover Omega law-of-motion parameters ---------
  Omega_hat     <- as.numeric(vphi    - mX  %*% beta_state)
  Omega_lag_hat <- as.numeric(vlagphi - mlX %*% beta_state)

  Omega_lag_poly_hat <- poly(Omega_lag_hat, degree = degree, raw = TRUE)
  Omega_lag_poly_hat <- cbind(1, Omega_lag_poly_hat)

  g_b_hat       <- solve(t(Omega_lag_poly_hat) %*% Omega_lag_poly_hat) %*%
    t(Omega_lag_poly_hat) %*% Omega_hat
  g_b_intercept <- as.numeric(g_b_hat[1])
  g_b_slopes    <- as.numeric(g_b_hat[-1])

  # --------- TFP and output ---------
  y_hat <- as.numeric(fX_sub %*% beta_free + sX_sub %*% beta_state)

  if (TFP_minuend == "y") {
    minuend_vec <- Y_sub
  } else {
    minuend_vec <- DT_sub[["phi_clean"]] + as.numeric(fX_sub %*% beta_free)
  }

  tfp <- as.numeric(minuend_vec - y_hat)

  DT_out <- setNames(
    data.table::data.table(DT_sub[[id]], DT_sub[[time]], DT_sub[[y]]),
    c(id, time, y)
  )

  DT_out[, ("tfp") := tfp]
  DT_out[, (y) := NULL]

  for (j in seq_along(endog)) {
    DT_out[, (paste0("el_", endog[j])) := beta_free[j]]
  }
  for (j in seq_along(exog)) {
    DT_out[, (paste0("el_", exog[j])) := beta_state[j]]
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
  DT_out[, ("convergence") := as.integer(opt$convergence)]

  return(DT_out)
}
