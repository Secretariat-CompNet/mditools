#' Estimate production functions with multiple estimators (panel, by-group)
#'
#' @description
#' \code{mdi_estimate_prodfun()} is a wrapper that estimates Cobb-Douglas production functions
#' on firm-level panel data using a selectable set of estimators (methods).
#' It is designed for *grid runs* across within-method specifications (e.g., degree choices,
#' time fixed effects, alternative demeaning) and returns standardized outputs that can be
#' pooled across methods and industries.
#'
#' The wrapper is typically run "by industry" (or another grouping variable) using \code{bygroup}.
#' Within each group, the function calls one estimator at a time and binds results into a common
#' panel-style output format including:
#' \itemize{
#'   \item firm id and year (or generic \code{id}, \code{time})
#'   \item elasticities with standardized names \code{el_<var>}
#'   \item total factor productivity proxy (\code{tfp}) and optional de-meaned version (\code{TFP_demeaned})
#'   \item book-keeping: \code{NumObs}, plus method/spec identifiers if requested
#' }
#'
#' @details
#' ## Input convention
#' The wrapper assumes all production-function variables are in logs unless a method explicitly
#' requires levels (e.g., cost shares with \code{log_values=TRUE} will exponentiate internally).
#'
#' Minimal required columns in \code{DT} are: output \code{y}; free input(s) \code{endog};
#' state input(s) \code{exog}; proxy/instruments \code{instr} (ACF/LP/WRDG); panel identifiers
#' \code{id}, \code{time}; and the grouping variable \code{bygroup}.
#'
#' Each estimator performs its own NA filtering on the variables it needs; hence the effective
#' sample can differ by method/spec.
#'
#' ## Methods implemented
#' Methods are selected via \code{methods}. The wrapper recognizes:
#' \describe{
#'   \item{\code{"acf"}}{Ackerberg-Caves-Frazer (2015) control-function estimator (Cobb-Douglas).
#'     First-stage polynomial in inputs and proxy to construct \eqn{\Phi}, then a GMM stage with
#'     lagged inputs as instruments. Returns firm-level elasticities and a residual-based \code{tfp}.}
#'
#'   \item{\code{"lp"}}{Levinsohn-Petrin (2003) proxy estimator (Cobb-Douglas).}
#'
#'   \item{\code{"wdrg"}}{Wooldridge (2009) system-GMM estimator. Stacked two-equation GMM,
#'     estimated linearly in the parameters (the polynomial coefficients on \eqn{c(x_t,m_t)} and
#'     \eqn{c(x_{t-1},m_{t-1})} are free, separate vectors; this is more general than the
#'     random-walk-with-drift case but does not impose the structural AR(G) restriction of a
#'     degree-G nonlinear law of motion). Returns common elasticities, \code{se_el_<var>},
#'     \code{tfp = y - X*beta}, and \code{alpha_hat} as a diagnostic column.}
#'
#'   \item{\code{"dpgmm"}}{Arellano-Bond (1991) difference-GMM for the dynamic PF
#'     \eqn{y_{it} = \rho y_{i,t-1} + \beta_l l_{it} + \beta_k k_{it} + \alpha_i + \varepsilon_{it}}.
#'     First-differences to remove the firm fixed effect; lagged levels of (y, l, k) instrument the
#'     differenced regressors (K predetermined, L endogenous). One-step GMM, cluster-robust SEs,
#'     Sargan/Hansen and AR(1)/AR(2) diagnostics. Returns \code{el_<var>}, \code{rho},
#'     \code{tfp = y - beta_l l - beta_k k}.}
#'
#'   \item{\code{"ols"}}{Pooled OLS baseline with flexible polynomial controls (Cobb-Douglas).
#'     \code{tfp = y - X*beta}. Use \code{degree = 1} for the clean naive baseline.}
#'
#'   \item{\code{"cs"}}{Cost-shares (index-number) approach for Cobb-Douglas.}
#' }
#'
#' ## Method-specific optional arguments (\code{xxxx_args})
#' Each method accepts an optional list of tuning arguments passed via a dedicated parameter name:
#' \code{acf_args}, \code{lp_args}, \code{wdrg_args}, \code{dpgmm_args}, \code{ols_args}, \code{cs_args}.
#' Unspecified fields fall back to method defaults.
#'
#' \strong{ACF arguments (\code{acf_args})}: \code{spec}, \code{degree},
#' \code{lower_bound_theta}, \code{upper_bound_theta}, \code{TFP_demeaned}, \code{TFP_minuend}
#' ("Phi"/"y"), \code{Omega_estimates}, \code{time_FE}, and \code{extended_instr} (logical,
#' default FALSE; if TRUE adds \eqn{\Phi_{t-1}} to the instrument set per ACF eq 28 for
#' overidentification).
#'
#' \strong{LP arguments (\code{lp_args})}: \code{spec}, \code{degree}, \code{lower_bound_theta},
#' \code{upper_bound_theta}, \code{TFP_demeaned}, \code{TFP_minuend} ("y"/"Phi"),
#' \code{Omega_estimates}, \code{time_FE}.
#'
#' \strong{Wooldridge arguments (\code{wdrg_args})}: \code{degree}, \code{tol},
#' \code{TFP_demeaned}, \code{TFP_minuend} ("y" only).
#'
#' \strong{Dynamic panel GMM arguments (\code{dpgmm_args})}: \code{max_lag_Z} (integer >= 1,
#' default 2; instrument lag depth), \code{TFP_demeaned}.
#'
#' \strong{OLS arguments (\code{ols_args})}: \code{spec}, \code{degree}, \code{TFP_demeaned}.
#'
#' \strong{Cost shares arguments (\code{cs_args})}: \code{log_values}, \code{TFP_demeaned}.
#'
#' @param DT A \code{data.table} or \code{data.frame} with firm-level panel data.
#' @param methods Character vector of methods to run. Supported: \code{"acf"}, \code{"lp"},
#'   \code{"wdrg"}, \code{"dpgmm"}, \code{"ols"}, \code{"cs"}.
#' @param y Character scalar. Output variable name (typically log output/value added).
#' @param endog Character vector. Free/endogenous input(s) (e.g., log labor cost).
#' @param exog Character vector. State/exogenous input(s) (e.g., log capital).
#' @param instr Character vector. Proxy/instrument variable(s) (e.g., log materials).
#'   Required by \code{"acf"}, \code{"lp"}, \code{"wdrg"}; ignored otherwise.
#' @param id Character scalar. Firm identifier column.
#' @param time Character scalar. Time identifier column.
#' @param bygroup Character scalar. Column name of the grouping variable (e.g. industry code);
#'   estimation is performed separately for each unique value.
#' @param acf_args,lp_args,wdrg_args,dpgmm_args,ols_args,cs_args Optional lists of tuning
#'   arguments per method. See Details.
#' @param verbose Logical. If \code{TRUE}, progress messages and estimation warnings are printed.
#'   Default \code{TRUE}.
#' @param drop_empty Logical. If \code{TRUE}, groups with no successful estimations are excluded
#'   from the output. Default \code{TRUE}.
#' @param allowed_ids Optional character vector. If non-NULL, restricts the accepted values of
#'   \code{id} to this whitelist (e.g., for MDI use:
#'   \code{c("plantid","firmid","entid","entgrp")}). Default NULL means any column name is accepted.
#'
#' @return A \code{data.table} with one row per firm-year (or per used observation), containing
#'   \code{bygroup}, \code{method}, \code{id}, \code{time}, \code{el_<var>}, \code{tfp}, and
#'   method-specific extras (\code{TFP_demeaned}, \code{rho}, \code{se_el_<var>}, diagnostics),
#'   plus \code{NumObs}.
#'
#' @examples
#' \donttest{
#' library(data.table)
#' set.seed(42)
#' n <- 200
#' DT <- data.table(
#'   firmid = rep(1:50, each = 4),
#'   year   = rep(2000:2003, 50),
#'   sector = rep(c("A", "B"), each = 100),
#'   y      = rnorm(n, 5, 1),
#'   l      = rnorm(n, 3, 0.5),
#'   k      = rnorm(n, 4, 0.5),
#'   m      = rnorm(n, 2, 0.5)
#' )
#' mdi_estimate_prodfun(
#'   DT, methods = c("ols", "acf"),
#'   y = "y", endog = "l", exog = "k", instr = "m",
#'   id = "firmid", time = "year", bygroup = "sector"
#' )
#' }
#'
#' @export

mdi_estimate_prodfun <- function(DT,
                                 methods,
                                 # core variable names
                                 y,
                                 endog,    # only one entry allowed (for the moment)
                                 exog,     # only one entry allowed (for the moment)
                                 instr = NULL,     # only needed for ACF/LP/WRDG
                                 id,
                                 time,
                                 bygroup,  # don't mention any time dimension here
                                 # per-method option lists
                                 acf_args   = list(),
                                 lp_args    = list(),
                                 wdrg_args  = list(),
                                 dpgmm_args = list(),
                                 ols_args   = list(),
                                 cs_args    = list(),
                                 # behaviour options
                                 verbose    = TRUE,
                                 drop_empty = TRUE,
                                 # id-column whitelist; NULL = any column name accepted.
                                 # MDI use: c("plantid","firmid","entid","entgrp").
                                 allowed_ids = NULL) {

  if (!data.table::is.data.table(DT) && !is.data.frame(DT))
    stop("'DT' must be a data.table or data.frame.")
  DT <- data.table::as.data.table(DT)
  check_char_vec(methods, "methods")
  check_string(y,       "y")
  check_char_vec(endog, "endog")
  check_char_vec(exog,  "exog")
  if (!is.null(instr)) check_char_vec(instr, "instr")
  check_string(id,      "id")
  check_string(time,    "time")
  check_string(bygroup, "bygroup")

  # ---- sanity checks on mandatory columns ----
  needed_cols  <- unique(c(y, endog, exog, instr, id, time, bygroup))
  needed_cols  <- needed_cols[!is.null(needed_cols)]
  missing_cols <- setdiff(needed_cols, names(DT))
  if (length(missing_cols) > 0)
    stop("DT is missing required columns: ", paste(missing_cols, collapse = ", "))

  # ---- optional id whitelist ----
  if (!is.null(allowed_ids) && !id %in% allowed_ids) {
    stop(sprintf("`id` must be one of: %s. You provided: '%s'",
                 paste(allowed_ids, collapse = ", "), id))
  }

  # normalize inputs
  methods <- tolower(methods)

  # allowed methods
  allowed <- c("acf", "lp", "wdrg", "dpgmm", "ols", "cs")
  bad <- setdiff(methods, allowed)
  if (length(bad) > 0)
    stop("Unknown method(s): ", paste(bad, collapse = ", "),
         ". Allowed: ", paste(allowed, collapse = ", "))

  # check which methods require instr
  needs_instr <- methods %in% c("acf", "lp", "wdrg")
  if (any(needs_instr) && (is.null(instr) || length(instr) == 0))
    stop("`instr` must be provided for methods: acf, lp, wdrg.")

  # group codes
  codes    <- unique(DT[[bygroup]])
  res_list <- list()

  # method dispatch: each returns a DT_out or NULL
  call_method <- function(method, DT_copy, code) {

    warn_handler <- function(w) {
      if (verbose)
        message(paste0("---> Warning [", method, "] for ", bygroup, "=", code, ": ",
                       conditionMessage(w)))
      invokeRestart("muffleWarning")
    }

    err_handler <- function(e) {
      if (verbose)
        message(paste0("---> Estimation failed [", method, "] for ", bygroup, "=", code, ": ",
                       conditionMessage(e)))
      NULL
    }

    out <- tryCatch(
      withCallingHandlers(
        {
          if (method == "acf") {
            do.call(mdi_acf_prodest,
                    c(list(DT = DT_copy, y = y, endog = endog, exog = exog,
                           instr = instr, id = id, time = time), acf_args))
          } else if (method == "lp") {
            do.call(mdi_lp_prodest,
                    c(list(DT = DT_copy, y = y, endog = endog, exog = exog,
                           instr = instr, id = id, time = time), lp_args))
          } else if (method == "wdrg") {
            do.call(mdi_wdrg_prodest,
                    c(list(DT = DT_copy, y = y, endog = endog, exog = exog,
                           instr = instr, id = id, time = time), wdrg_args))
          } else if (method == "dpgmm") {
            do.call(mdi_dpgmm_prodest,
                    c(list(DT = DT_copy, y = y, endog = endog, exog = exog,
                           id = id, time = time), dpgmm_args))
          } else if (method == "ols") {
            do.call(mdi_ols_prodest,
                    c(list(DT = DT_copy, y = y, endog = endog, exog = exog,
                           id = id, time = time), ols_args))
          } else if (method == "cs") {
            do.call(mdi_cs_prodest,
                    c(list(DT = DT_copy, y = y, endog = endog, exog = exog,
                           id = id, time = time, bygroup = bygroup), cs_args))
          } else {
            stop("Unknown method: ", method)
          }
        },
        warning = warn_handler
      ),
      error = err_handler
    )

    if (!is.null(out) && nrow(out) > 0) {
      out[, (bygroup)  := code]
      out[, ("method") := method]
      data.table::setcolorder(out, c(bygroup, "method",
                                     setdiff(names(out), c(bygroup, "method"))))
    }

    out
  }

  # loop
  for (code in codes) {
    DT_copy <- data.table::copy(DT[DT[[bygroup]] == code])

    code_results <- list()
    for (m in methods) {
      if (verbose) message(paste0("Running [", m, "] for ", bygroup, "=", code, " ..."))
      code_results[[m]] <- call_method(m, DT_copy, code)
    }

    if (drop_empty)
      code_results <- code_results[!vapply(code_results, is.null, logical(1))]

    if (length(code_results) > 0) {
      res_list[[as.character(code)]] <- data.table::rbindlist(code_results, fill = TRUE)
    } else {
      res_list[[as.character(code)]] <- NULL
    }
  }

  # final bind
  res_list <- res_list[!vapply(res_list, is.null, logical(1))]
  if (length(res_list) == 0) {
    if (verbose) message("No successful estimations.")
    return(data.table::data.table())
  }

  data.table::rbindlist(res_list, fill = TRUE)
}
