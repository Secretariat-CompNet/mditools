#' Wooldridge (2009) System-GMM Production Function Estimator
#'
#' @description
#' Implements the stacked two-equation GMM from Wooldridge (2009, Economics
#' Letters 104, 112-114). The system is estimated linearly in the parameters:
#' the polynomial coefficients on \eqn{(x_t, m_t)} (Wooldridge eq 3.3) and on
#' \eqn{(x_{t-1}, m_{t-1})} (eq 3.4) are free, separate vectors. This is more
#' general than the random-walk-with-drift restriction (eq 3.10) which forces
#' them equal, but it does NOT impose the structural rho_g * lambda^g form
#' that eq (3.4) implies for a general degree-G polynomial law of motion
#' (G > 1, which would require nonlinear GMM and is not implemented here).
#' The implementation matches the standard empirical Wooldridge GMM used in
#' Petrin, Poi and Levinsohn (2004).
#'
#' Returns elasticities common to all firms in the group, firm-year
#' \code{tfp = y - X*beta} (the intercept \code{alpha_hat} is reported as a
#' separate column for diagnostics, not subtracted), and analytical SEs.
#'
#' @param DT A \code{data.table} (or coercible object) containing panel data.
#' @param y Character. Output variable name (in logs).
#' @param endog Character. Free input name (e.g. labour; one variable for now).
#' @param exog Character. State input name (e.g. capital; one variable for now).
#' @param instr Character. Proxy variable name (e.g. materials).
#' @param id Character. Firm identifier column.
#' @param time Character. Time identifier column.
#' @param degree Integer. Polynomial degree for c(x, m). Default \code{2}.
#' @param tol Numeric. Linear-solver tolerance for the GMM normal equations.
#'   Default \code{1e-10}.
#' @param TFP_demeaned Logical. If \code{TRUE}, returns TFP_demeaned
#'   (tfp - period mean). Default \code{TRUE}.
#' @param TFP_minuend Character. Currently only \code{"y"} is supported.
#'
#' @return A \code{data.table} with one row per observation in the GMM sample,
#'   containing \code{id}, \code{time}, \code{tfp}, \code{alpha_hat},
#'   \code{el_<input>}, \code{se_el_<input>}, \code{NumObs}, and optionally
#'   \code{TFP_demeaned}.
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
#' result <- mdi_wdrg_prodest(DT, y = "y", endog = "l", exog = "k", instr = "m",
#'                            id = "id", time = "year", TFP_demeaned = FALSE)
#' }
#'
#' @export

mdi_wdrg_prodest <- function(DT,
                             y,
                             endog,
                             exog,
                             instr,
                             id,
                             time,
                             degree       = 2,
                             tol          = 1e-10,
                             TFP_demeaned = TRUE,
                             TFP_minuend  = "y") {

  check_string(y,    "y")
  check_string(id,   "id")
  check_string(time, "time")
  check_char_vec(endog, "endog")
  check_char_vec(exog,  "exog")
  check_char_vec(instr, "instr")
  check_dt(DT, c(y, endog, exog, instr, id, time))
  check_choice(TFP_minuend, "TFP_minuend", "y")

  # --- helper: drop polynomial columns identical to a base column ---
  drop_dup_cols <- function(polyM, baseM, tol = 1e-12) {
    keep <- rep(TRUE, ncol(polyM))
    for (j in seq_len(ncol(polyM))) {
      colj <- polyM[, j]
      for (b in seq_len(ncol(baseM))) {
        if (max(abs(colj - baseM[, b])) < tol) {
          keep[j] <- FALSE
          break
        }
      }
    }
    polyM[, keep, drop = FALSE]
  }

  # --- local copy of prodest::weightM ---
  weightM_local <- function(Y, X1, X2, Z1, Z2, betas, numR, SE = FALSE) {
    k1 <- ncol(X1)
    N  <- nrow(X1)

    R1t <- Y - X1 %*% betas[1:k1, drop = FALSE]
    R2t <- Y - X2 %*% c(betas[1:numR],
                        betas[(k1 + 1):length(betas), drop = FALSE])
    u   <- c(R1t, R2t)
    Z   <- as.matrix(Matrix::bdiag(Z1, Z2))

    sigma_rs <- t(u) %*% u
    S <- sigma_rs[1] * (t(Z) %*% Z)

    if (SE) {
      dX <- rbind(
        cbind(X1, matrix(0, N, ncol(X2) - numR)),
        cbind(X2[, 1:numR, drop = FALSE],
              matrix(0, N, ncol(X1) - numR),
              X2[, (numR + 1):ncol(X2), drop = FALSE])
      )
      var_beta <- (1 / N) * solve((t(dX) %*% Z) %*% solve(S) %*% (t(Z) %*% dX))
      sqrt(diag(var_beta))
    } else {
      solve(S)
    }
  }

  # --- 1. Data prep ---
  DT <- data.table::as.data.table(data.table::copy(DT))
  data.table::setkeyv(DT, c(id, time))

  Y_vec <- DT[[y]]
  fX    <- as.matrix(DT[, endog, with = FALSE])
  sX    <- as.matrix(DT[, exog,  with = FALSE])
  pX    <- as.matrix(DT[, instr, with = FALSE])

  id_vec   <- DT[[id]]
  time_vec <- DT[[time]]

  fnum <- ncol(fX)

  # Lags of free inputs
  lag.fX <- fX
  for (j in seq_len(fnum)) {
    lag.fX[, j] <- panel_lag(fX[, j], id_vec, time_vec)
  }

  # Polynomial in (sX, pX), drop the degree-1 monomials (= base columns)
  polyframe_all <- poly(sX, pX, degree = degree, raw = TRUE)
  baseM         <- cbind(sX, pX)
  polyframe     <- drop_dup_cols(polyframe_all, baseM)
  regvars       <- cbind(sX, pX, polyframe)

  # Lags of regvars
  lagregvars <- regvars
  for (j in seq_len(ncol(regvars))) {
    lagregvars[, j] <- panel_lag(regvars[, j], id_vec, time_vec)
  }

  # Stack all pieces to drop NAs consistently
  mf_df <- data.frame(
    Y       = as.numeric(Y_vec),
    idvar   = as.numeric(id_vec),
    timevar = as.numeric(time_vec),
    fX,
    sX,
    lag.fX,
    regvars,
    lagregvars
  )

  keep     <- complete.cases(mf_df)
  keep_idx <- which(keep)
  mf_df    <- mf_df[keep, , drop = FALSE]

  N     <- nrow(mf_df)
  Y_use <- mf_df$Y
  dY    <- c(Y_use, Y_use)
  if (length(dY) != 2L * N)
    stop("WDRG: stacked response vector has wrong length (", length(dY),
         " != ", 2L * N, ").")

  # Pull cleaned blocks back out by position (regvars have no stable names)
  fX_use      <- as.matrix(mf_df[, 3 + seq_len(ncol(fX)), drop = FALSE])
  off         <- 3 + ncol(fX)
  sX_use      <- as.matrix(mf_df[, off + seq_len(ncol(sX)), drop = FALSE])
  off         <- off + ncol(sX)
  lagfX_use   <- as.matrix(mf_df[, off + seq_len(ncol(lag.fX)), drop = FALSE])
  off         <- off + ncol(lag.fX)
  regvars_use <- as.matrix(mf_df[, off + seq_len(ncol(regvars)), drop = FALSE])
  off         <- off + ncol(regvars)
  lagreg_use  <- as.matrix(mf_df[, off + seq_len(ncol(lagregvars)), drop = FALSE])

  # Build X1/X2, Z1/Z2 as in prodest
  X1 <- cbind(1, fX_use, regvars_use)
  X2 <- cbind(1, fX_use, sX_use, lagreg_use)
  Z1 <- cbind(1, fX_use, regvars_use)
  Z2 <- cbind(1, lagfX_use, sX_use, lagreg_use)

  fnum <- ncol(fX_use)
  snum <- ncol(sX_use)
  cnum <- 0
  numR <- 1 + fnum + snum + cnum

  if (ncol(X1) < numR) {
    stop(sprintf("X1 has %d cols but numR=%d. State inputs likely missing from regvars/X1.",
                 ncol(X1), numR))
  }

  numU1 <- ncol(X1) - numR
  numU2 <- ncol(X2) - numR
  N     <- nrow(X1)

  dX <- rbind(
    cbind(X1, matrix(0, N, numU2)),
    cbind(X2[, 1:numR, drop = FALSE],
          matrix(0, N, numU1),
          X2[, (numR + 1):ncol(X2), drop = FALSE])
  )

  Z_big <- as.matrix(Matrix::bdiag(Z1, Z2))

  # First-step diagonal weighting based on (Z'Z)^(-1)
  ZZ_inv <- solve(t(Z_big) %*% Z_big)
  W1     <- ZZ_inv * diag(ncol(Z_big))

  # --- 3. First-step GMM ---
  A1 <- t(dX) %*% Z_big %*% W1 %*% t(Z_big) %*% dX
  if (nrow(dX) != 2L * N || nrow(Z_big) != 2L * N || length(dY) != 2L * N)
    stop("WDRG: stacked matrix dimensions are inconsistent (N=", N, ").")
  B1 <- t(dX) %*% Z_big %*% W1 %*% t(Z_big) %*% dY

  betas_1st <- qr.solve(A1, B1, tol = tol)

  Y_use <- as.numeric(mf_df$Y)
  if (length(Y_use) != nrow(X1) || nrow(X1) != nrow(X2))
    stop("WDRG: Y, X1, X2 have inconsistent row counts.")

  W_star <- weightM_local(Y = Y_use, X1 = X1, X2 = X2, Z1 = Z1, Z2 = Z2,
                          betas = betas_1st, numR = numR, SE = FALSE)

  # --- 4. Second-step GMM ---
  A2 <- t(dX) %*% Z_big %*% W_star %*% t(Z_big) %*% dX
  B2 <- t(dX) %*% Z_big %*% W_star %*% t(Z_big) %*% dY
  betas_2nd <- solve(A2, B2, tol = tol)

  numobs <- nrow(X1)

  se_all <- weightM_local(Y = Y_use, X1 = X1, X2 = X2, Z1 = Z1, Z2 = Z2,
                          betas = as.numeric(betas_2nd), numR = numR, SE = TRUE)

  # --- 5. Extract coefficients & SEs ---
  betapar <- betas_2nd[2:(1 + fnum + snum + cnum)]
  betase  <- se_all[  2:(1 + fnum + snum + cnum)]

  res_names <- c(endog, exog)
  names(betapar) <- res_names
  names(betase)  <- res_names

  alpha_hat  <- betas_2nd[1]
  beta_free  <- betapar[endog]
  beta_state <- betapar[exog]

  # --- 6. TFP (excludes intercept; alpha_hat reported separately) ---
  DT_keep <- DT[keep_idx]
  fX_used <- as.matrix(DT_keep[, endog, with = FALSE])
  sX_used <- as.matrix(DT_keep[, exog,  with = FALSE])
  Y_used  <- DT_keep[[y]]

  y_hat <- as.numeric(
    fX_used %*% as.numeric(beta_free) +
      sX_used %*% as.numeric(beta_state)
  )

  if (TFP_minuend == "y") {
    tfp <- Y_used - y_hat
  } else {
    stop("For WRDG, TFP_minuend should be 'y' (no Phi available).")
  }

  # --- 7. Build output ---
  DT_out <- setNames(
    data.table::data.table(DT_keep[[id]], DT_keep[[time]]),
    c(id, time)
  )

  DT_out[, ("tfp")       := tfp]
  DT_out[, ("alpha_hat") := as.numeric(alpha_hat)]

  for (v in endog) {
    DT_out[, (paste0("el_",    v)) := as.numeric(beta_free[v])]
    DT_out[, (paste0("se_el_", v)) := as.numeric(betase[v])]
  }
  for (v in exog) {
    DT_out[, (paste0("el_",    v)) := as.numeric(beta_state[v])]
    DT_out[, (paste0("se_el_", v)) := as.numeric(betase[v])]
  }

  if (TFP_demeaned) {
    DT_out[, ("mean_tfp")     := mean(.SD[["tfp"]], na.rm = TRUE),
           by = c(time), .SDcols = "tfp"]
    DT_out[, ("TFP_demeaned") := .SD[["tfp"]] - .SD[["mean_tfp"]],
           .SDcols = c("tfp", "mean_tfp")]
    DT_out[, ("mean_tfp")     := NULL]
  }

  DT_out[, ("NumObs") := numobs]

  ord    <- do.call(order, DT_out[, .SD, .SDcols = c(time, id)])
  DT_out <- DT_out[ord]

  return(DT_out)
}
