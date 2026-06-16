#' Arellano-Bond Difference-GMM Production Function Estimator
#'
#' @description
#' Estimates the dynamic Cobb-Douglas production function
#' \deqn{y_{it} = \rho\,y_{i,t-1} + \beta_l\,l_{it} + \beta_k\,k_{it} + \alpha_i + \varepsilon_{it}}
#' by first-differencing to eliminate the firm fixed effect \eqn{\alpha_i},
#' then applying one-step GMM with lagged levels of (y, l, k) as instruments
#' for the differenced regressors. Reference: Arellano & Bond (1991, ReStud).
#'
#' Capital is treated as predetermined (chosen at t-1), so \eqn{k_{i,t-s}} for
#' s >= 1 are valid instruments for \eqn{\Delta k_{it}}. Labour is treated as
#' endogenous (chosen at t with knowledge of \eqn{\varepsilon_{it}}), so
#' \eqn{l_{i,t-s}} for s >= 2 are valid. Lagged output \eqn{y_{i,t-s}} for
#' s >= 2 instruments \eqn{\Delta y_{i,t-1}}. The lag depth is capped by
#' \code{max_lag_Z} to avoid the "too many instruments" problem (Roodman 2009).
#'
#' Estimation is one-step GMM with weight matrix \eqn{W = (Z'HZ)^{-1}}, where H
#' is block-diagonal by firm with tridiagonal blocks (2 on the diagonal, -1 on
#' the first off-diagonals) reflecting the MA(1) structure of
#' \eqn{\Delta\varepsilon_{it}} under iid \eqn{\varepsilon}. Standard errors use
#' the cluster-robust sandwich formula (clusters = firms).
#'
#' tfp is reported in levels as \code{tfp = y - beta_l*l - beta_k*k}, matching
#' the convention of the other estimators (ACF/LP/OLS/WDRG). It absorbs the
#' firm fixed effect and the lagged-y persistence; it is NOT the productivity
#' innovation \eqn{\varepsilon_{it}}.
#'
#' @param DT A \code{data.table} (or coercible object) containing panel data.
#' @param y Character scalar. Output variable name (in logs).
#' @param endog Character scalar. Free input name (e.g. "ln_labor_cost").
#' @param exog Character scalar. State input name (e.g. "ln_capital").
#' @param id Character scalar. Firm identifier column.
#' @param time Character scalar. Time identifier column.
#' @param max_lag_Z Integer >= 1. Maximum lag depth for instruments. Default \code{2}.
#' @param TFP_demeaned Logical. If \code{TRUE}, returns TFP_demeaned = tfp -
#'   period mean. Default \code{TRUE}.
#'
#' @return A \code{data.table} with one row per firm-year (subset where y,
#'   endog, exog are non-missing). Columns: \code{id}, \code{time},
#'   \code{el_<endog>}, \code{el_<exog>}, \code{se_el_<endog>},
#'   \code{se_el_<exog>}, \code{rho}, \code{se_rho}, \code{tfp}, \code{NumObs},
#'   the diagnostics \code{sargan_J}/\code{sargan_df}/\code{sargan_pval} and
#'   \code{ar1_z}/\code{ar1_pval}/\code{ar2_z}/\code{ar2_pval}, and optionally
#'   \code{TFP_demeaned}. Under iid \eqn{\varepsilon}: AR(1) should reject,
#'   AR(2) should not; Sargan tests the overidentifying restrictions.
#'
#' @examples
#' \donttest{
#' library(data.table)
#' set.seed(1)
#' n_firms <- 40; n_periods <- 6
#' n <- n_firms * n_periods
#' DT <- data.table(
#'   id   = rep(seq_len(n_firms), each = n_periods),
#'   year = rep(seq(2000L, length.out = n_periods), times = n_firms),
#'   y    = rnorm(n, 5, 1),
#'   l    = rnorm(n, 3, 0.5),
#'   k    = rnorm(n, 4, 0.5)
#' )
#' result <- mdi_dpgmm_prodest(DT, y = "y", endog = "l", exog = "k",
#'                             id = "id", time = "year", TFP_demeaned = FALSE)
#' }
#'
#' @export

mdi_dpgmm_prodest <- function(DT,
                              y,
                              endog,
                              exog,
                              id,
                              time,
                              max_lag_Z    = 2,
                              TFP_demeaned = TRUE) {

  check_string(y,     "y")
  check_string(id,    "id")
  check_string(time,  "time")
  check_string(endog, "endog")
  check_string(exog,  "exog")
  check_dt(DT, c(y, endog, exog, id, time))
  if (max_lag_Z < 1L) stop("'max_lag_Z' must be >= 1.")

  DT <- data.table::as.data.table(data.table::copy(DT))
  data.table::setorderv(DT, c(id, time))

  id_vec    <- DT[[id]]
  time_vec  <- DT[[time]]
  y_col     <- DT[[y]]
  endog_col <- DT[[endog]]
  exog_col  <- DT[[exog]]

  # ----- build lags 1..(max_lag_Z + 1) for y, endog, exog -----
  for (L in seq_len(max_lag_Z + 1L)) {
    DT[, (paste0(y,     "_lag", L)) := panel_lag_L(y_col,     id_vec, time_vec, L = L)]
    DT[, (paste0(endog, "_lag", L)) := panel_lag_L(endog_col, id_vec, time_vec, L = L)]
    DT[, (paste0(exog,  "_lag", L)) := panel_lag_L(exog_col,  id_vec, time_vec, L = L)]
  }

  # ----- differenced regressors and LHS -----
  y_lag1_col     <- DT[[paste0(y,     "_lag1")]]
  y_lag2_col     <- DT[[paste0(y,     "_lag2")]]
  endog_lag1_col <- DT[[paste0(endog, "_lag1")]]
  exog_lag1_col  <- DT[[paste0(exog,  "_lag1")]]
  DT[, ("dy_")     := y_col     - y_lag1_col]
  DT[, ("dy_lag_") := y_lag1_col - y_lag2_col]
  DT[, ("dl_")     := endog_col - endog_lag1_col]
  DT[, ("dk_")     := exog_col  - exog_lag1_col]

  # ----- instrument columns -----
  # y_{t-2..t-1-L}, l_{t-2..t-1-L} (endogenous), k_{t-1..t-L} (predetermined)
  iv_y_cols <- paste0(y,     "_lag", 2:(max_lag_Z + 1L))
  iv_l_cols <- paste0(endog, "_lag", 2:(max_lag_Z + 1L))
  iv_k_cols <- paste0(exog,  "_lag", 1:max_lag_Z)
  iv_cols   <- c(iv_y_cols, iv_l_cols, iv_k_cols)

  # ----- complete-case filter on the GMM sample -----
  needed_cols <- c("dy_", "dy_lag_", "dl_", "dk_", iv_cols)
  DT_d <- DT[complete.cases(DT[, needed_cols, with = FALSE])]
  data.table::setorderv(DT_d, c(id, time))

  if (nrow(DT_d) < 10L)
    stop("mdi_dpgmm_prodest: too few observations after differencing/lagging.")

  # ----- build matrices -----
  X <- as.matrix(DT_d[, c("dy_lag_", "dl_", "dk_"), with = FALSE])
  colnames(X) <- c("d_y_lag", paste0("d_", endog), paste0("d_", exog))
  y_diff <- as.numeric(DT_d[["dy_"]])
  Z      <- as.matrix(DT_d[, iv_cols, with = FALSE])

  N_obs <- nrow(X)
  K     <- ncol(Z)
  k_par <- ncol(X)

  # ----- Z'HZ for the tridiagonal H, computed without materializing H -----
  #   Z'HZ = 2*(Z'Z) - sum over adjacent (t-1,t) pairs of (Z_t'Z_{t-1} + Z_{t-1}'Z_t)
  # prev_idx[i] = row index of the t-1 observation for row i (gap-aware), else NA.
  prev_idx <- panel_lag_L(seq_len(N_obs), DT_d[[id]], DT_d[[time]], L = 1L)
  has_prev <- which(!is.na(prev_idx))
  adj_cur  <- has_prev
  adj_prv  <- as.integer(prev_idx[has_prev])

  ZZ <- crossprod(Z)
  if (length(adj_cur) > 0L) {
    ZcZp <- crossprod(Z[adj_cur, , drop = FALSE], Z[adj_prv, , drop = FALSE])
  } else {
    ZcZp <- matrix(0, K, K)
  }
  ZHZ <- 2 * ZZ - ZcZp - t(ZcZp)

  # ----- one-step GMM -----
  W <- solve(ZHZ)

  ZX      <- crossprod(Z, X)
  Zy      <- crossprod(Z, y_diff)
  XZ_W    <- crossprod(ZX, W)
  XZ_W_ZX <- XZ_W %*% ZX
  XZ_W_Zy <- XZ_W %*% Zy

  XZ_W_ZX_inv <- solve(XZ_W_ZX)
  beta_hat    <- as.numeric(XZ_W_ZX_inv %*% XZ_W_Zy)
  names(beta_hat) <- colnames(X)

  u_hat <- as.numeric(y_diff - X %*% beta_hat)

  # ----- cluster-robust SE (clusters = firms) -----
  # Omega = sum_i (Z_i' u_i)(Z_i' u_i)'
  Zu          <- Z * u_hat
  Zu_dt       <- data.table::as.data.table(Zu)
  Zu_dt[, ("firm_cluster_") := DT_d[[id]]]
  sum_cols    <- setdiff(names(Zu_dt), "firm_cluster_")
  Zu_firm     <- Zu_dt[, lapply(.SD, sum), by = "firm_cluster_", .SDcols = sum_cols]
  Zu_firm[, ("firm_cluster_") := NULL]
  Zu_firm_mat <- as.matrix(Zu_firm)
  Omega       <- crossprod(Zu_firm_mat)

  V_beta  <- XZ_W_ZX_inv %*% XZ_W %*% Omega %*% t(XZ_W) %*% XZ_W_ZX_inv
  se_beta <- sqrt(pmax(diag(V_beta), 0))
  names(se_beta) <- colnames(X)

  rho_hat <- beta_hat["d_y_lag"]
  beta_l  <- beta_hat[paste0("d_", endog)]
  beta_k  <- beta_hat[paste0("d_", exog)]
  se_rho  <- se_beta["d_y_lag"]
  se_l    <- se_beta[paste0("d_", endog)]
  se_k    <- se_beta[paste0("d_", exog)]

  # ----- Sargan/Hansen J -----
  Zu_total <- crossprod(Z, u_hat)
  J_stat   <- as.numeric(t(Zu_total) %*% W %*% Zu_total)
  J_df     <- K - k_par
  J_pval   <- if (J_df > 0L) pchisq(J_stat, df = J_df, lower.tail = FALSE) else NA_real_

  # ----- Arellano-Bond AR(p) serial-correlation test on Delta-eps_hat -----
  # m_p ~ N(0,1) under H0 of no order-p serial correlation. Variance uses
  # within-firm sums of cross-products (a simplified cluster-style version;
  # not the full first-stage correction of AB 1991, but a directional check).
  ab_serial_test <- function(u, firm_v, time_v, p) {
    u_lag <- panel_lag_L(u, firm_v, time_v, L = p)
    ok    <- !is.na(u_lag)
    if (sum(ok) < 2L) return(c(stat = NA_real_, pval = NA_real_))
    prod_   <- u[ok] * u_lag[ok]
    num     <- sum(prod_)
    var_frm <- tapply(prod_, firm_v[ok], sum)
    denom   <- sqrt(sum(var_frm^2))
    if (!is.finite(denom) || denom == 0) return(c(stat = NA_real_, pval = NA_real_))
    z <- num / denom
    c(stat = z, pval = 2 * pnorm(-abs(z)))
  }

  ar1 <- ab_serial_test(u_hat, DT_d[[id]], DT_d[[time]], p = 1L)
  ar2 <- ab_serial_test(u_hat, DT_d[[id]], DT_d[[time]], p = 2L)

  # ----- build output on the LEVEL data (not the differenced sample) -----
  lvl_cols <- c(id, time, y, endog, exog)
  DT_out   <- DT[complete.cases(DT[, lvl_cols, with = FALSE]), lvl_cols, with = FALSE]

  y_out_col     <- DT_out[[y]]
  endog_out_col <- DT_out[[endog]]
  exog_out_col  <- DT_out[[exog]]
  DT_out[, ("tfp") := y_out_col - beta_l * endog_out_col - beta_k * exog_out_col]

  if (TFP_demeaned) {
    DT_out[, ("mean_tfp_")    := mean(.SD[["tfp"]], na.rm = TRUE),
           by = c(time), .SDcols = "tfp"]
    DT_out[, ("TFP_demeaned") := .SD[["tfp"]] - .SD[["mean_tfp_"]],
           .SDcols = c("tfp", "mean_tfp_")]
    DT_out[, ("mean_tfp_")    := NULL]
  }

  DT_out[, (paste0("el_",    endog)) := as.numeric(beta_l)]
  DT_out[, (paste0("el_",    exog))  := as.numeric(beta_k)]
  DT_out[, (paste0("se_el_", endog)) := as.numeric(se_l)]
  DT_out[, (paste0("se_el_", exog))  := as.numeric(se_k)]
  DT_out[, ("rho")        := as.numeric(rho_hat)]
  DT_out[, ("se_rho")     := as.numeric(se_rho)]
  DT_out[, ("NumObs")     := N_obs]
  DT_out[, ("sargan_J")   := J_stat]
  DT_out[, ("sargan_df")  := J_df]
  DT_out[, ("sargan_pval"):= J_pval]
  DT_out[, ("ar1_z")      := unname(ar1["stat"])]
  DT_out[, ("ar1_pval")   := unname(ar1["pval"])]
  DT_out[, ("ar2_z")      := unname(ar2["stat"])]
  DT_out[, ("ar2_pval")   := unname(ar2["pval"])]

  # drop the raw level columns; keep id/time + derived
  DT_out[, (y)     := NULL]
  DT_out[, (endog) := NULL]
  DT_out[, (exog)  := NULL]

  ord <- do.call(order, DT_out[, .SD, .SDcols = c(time, id)])
  DT_out[ord]
}
