#' Weighted Regression with Automatic Disclosure Check
#'
#' @description
#' Runs regression analysis on a `data.table` using models from the **fixest** package
#' (`feols`, `feglm`, etc.), with automatic disclosure control applied.
#'
#' Disclosure rules ensure that regressions are only reported if they meet
#' minimum thresholds for sample size: both the number of observations and the
#' residual degrees of freedom must be at least `minNumObs`.
#'
#' When `count_firms = TRUE`, the number of unique firms and enterprises in the
#' regression sample are also computed and added to the output (`NumFirms`,
#' `NumEnt`). This is controlled by the caller -- typically set based on the
#' country-specific disclosure requirements of the NSI running the code.
#'
#' The function supports clustered standard errors, regression weights,
#' instrumental variables, and optional LaTeX export of results.
#'
#' @param DT A `data.table` containing the dataset for regression analysis.
#' @param formula A character vector of regression formulas.
#' @param model Character. The regression model to use (e.g. `"feols"`, `"feglm"`).
#'   Default is `"feols"`.
#' @param family Family specification for GLM models (only relevant if
#'   `model = "feglm"`). Example: `binomial`.
#' @param vcov Variance-covariance specification for standard errors.
#'   Can be `"iid"` (default) or a formula for clustered SE (e.g. `~clustervar`).
#' @param cluster Logical. If `TRUE`, standard errors are clustered. In this case
#'   `vcov` must specify the clustering variable. Default is `FALSE`.
#' @param tex Logical. If `TRUE`, regression results are exported as a LaTeX
#'   table using `etable()`. Default is `FALSE`.
#' @param output_name Character. Name of the LaTeX output file (without extension),
#'   if `tex = TRUE`. Default is `NULL`.
#' @param desc_file Character. Name of a description file to which an entry for
#'   the regression output is appended (if `tex = TRUE`). Default is `NULL`.
#' @param weights Optional formula specifying the weights variable to apply
#'   in the regression (e.g. `~myweights`). Default is `NULL`.
#' @param iv Logical. If `TRUE`, the regression is treated as an instrumental
#'   variable model and IV-specific fit statistics are reported. Default is `FALSE`.
#' @param num_firms Optional integer. Number of unique firms to use when
#'   `count_firms = TRUE` but `firm_col` is not found in the data. Default `NULL`.
#' @param num_ent Optional integer. Number of unique enterprises to use when
#'   `count_firms = TRUE` but `ent_col` is not found in the data. Default `NULL`.
#' @param count_firms Logical. If `TRUE`, the number of unique firms and
#'   enterprises in the regression sample are computed and added to the output
#'   as `NumFirms` and `NumEnt`. The columns used are controlled by `firm_col`
#'   and `ent_col`. Set to `TRUE` when NSI disclosure rules require firm-level
#'   counts. Default `FALSE`.
#' @param firm_col Character. Name of the column in `DT` that identifies firms,
#'   used to count unique firms when `count_firms = TRUE`. Falls back to
#'   `num_firms` if the column is absent. Default `"firmid"`.
#' @param ent_col Character. Name of the column in `DT` that identifies
#'   enterprises, used to count unique enterprises when `count_firms = TRUE`.
#'   Falls back to `num_ent` if the column is absent. Default `"entid"`.
#' @param minNumObs Integer. Minimum observations and degrees of freedom
#'   required for a regression to pass disclosure. Default `5L`.
#' @param dirOUTPUT Character. Path to the output directory (must end with
#'   `"/"`), used when `tex = TRUE`. Default `NULL`.
#'
#' @return A `data.table` of regression results, including:
#' - Coefficients and standard errors
#' - Confidence intervals (`ci.lower`, `ci.upper`)
#' - Sample size (`NumObs`), residual df (`df`)
#' - `NumFirms` and `NumEnt` (only when `count_firms = TRUE`)
#' - Fit statistics (R2, Adj. R2, AIC, BIC, LogLik, and IV/F tests if applicable)
#'
#' If disclosure criteria are not satisfied, the regression is skipped and a
#' message is printed.
#'
#' @details
#' - Regression output is only returned if both `nobs >= minNumObs` and
#'   residual degrees of freedom `>= minNumObs`.
#' - When `count_firms = TRUE`: unique firm and enterprise counts are computed
#'   from the regression sample using `firm_col` and `ent_col`. If a column is
#'   absent from the data, the corresponding `num_firms` or `num_ent` argument
#'   is used instead. If neither the column nor the manual count is provided,
#'   the function stops with an error before any regression is run.
#' - If `tex = TRUE`, results are saved to `<dirOUTPUT>/<output_name>.tex` and an
#'   entry is appended to `<dirOUTPUT>/<desc_file>.txt`.
#'
#' @examples
#' library(data.table)
#' set.seed(1)
#' DT <- data.table(
#'   y      = rnorm(100),
#'   x1     = rnorm(100),
#'   x2     = rnorm(100),
#'   firmid = paste0("F", sample(1:20, 100, replace = TRUE)),
#'   entid  = paste0("E", sample(1:15, 100, replace = TRUE))
#' )
#' # Basic regression
#' mdi_regress(DT, formula = "y ~ x1 + x2", minNumObs = 5L)
#'
#' # With firm/enterprise counts (e.g. when NSI rules require it)
#' mdi_regress(DT, formula = "y ~ x1 + x2", minNumObs = 5L,
#'             count_firms = TRUE)
#'
#' \dontrun{
#' # Weighted GLM with clustered SEs
#' mdi_regress(DT, formula = "y ~ x1 + x2", model = "feglm",
#'             family = binomial, weights = ~w,
#'             cluster = TRUE, vcov = "~firmid")
#'
#' # With LaTeX export
#' mdi_regress(DT, formula = "y ~ x1", tex = TRUE,
#'             output_name = "reg1", desc_file = "log",
#'             dirOUTPUT = "/output/NL/")
#' }
#'
#' @export

mdi_regress <-
  function(DT,
           formula,
           model = "feols",
           family = NULL,
           vcov = "iid",
           cluster = FALSE,
           tex = FALSE,
           output_name = NULL,
           desc_file = NULL,
           weights = NULL,
           iv = FALSE,
           num_firms = NULL,
           num_ent = NULL,
           count_firms = FALSE,
           firm_col = "firmid",
           ent_col = "entid",
           minNumObs = 5L,
           dirOUTPUT = NULL) {

    check_dt(DT)
    check_char_vec(formula, "formula")
    if (tex && is.null(dirOUTPUT))
      stop("'dirOUTPUT' must be supplied when tex = TRUE")
    if (count_firms) {
      if (is.null(num_firms)) check_dt(DT, firm_col)
      if (is.null(num_ent))   check_dt(DT, ent_col)
    }

    if (cluster == TRUE) {
      vcov <- as.formula(vcov)
    }

    regs <- lapply(formula, function(f) {
      # Run regression
      model_fn <- get(model, envir = asNamespace("fixest"))
      if (!is.null(family)) {
        if (!is.null(weights)) {
          reg <- model_fn(as.formula(f),
                          family = family,
                          data = DT,
                          weights = weights,
                          data.save = TRUE)
        } else {
          reg <- model_fn(as.formula(f),
                          family = family,
                          data = DT,
                          data.save = TRUE)
        }
      } else {
        if (!is.null(weights)) {
          reg <- model_fn(as.formula(f),
                          data = DT,
                          weights = weights,
                          data.save = TRUE)
        } else {
          reg <- model_fn(as.formula(f), data = DT,
                          data.save = TRUE)
        }
      }

      df_resid <- fixest::degrees_freedom(reg, type = "resid")

      if (reg$nobs >= as.integer(minNumObs) &&
          df_resid >= as.integer(minNumObs)) {
        return(reg)
      } else {
        message("Regression formula ", f, " model ", model,
                " does not satisfy disclosure criteria (obs/df).")
        NULL
      }
    })

    names(regs) <- formula
    regs <- regs[!vapply(regs, is.null, logical(1))]

    if (length(regs) != 0) {
      regDT <- data.table::rbindlist(lapply(regs, function(r) {
        coef_table <- summary(r, vcov = vcov)$coeftable
        ci <- tryCatch(confint(r), error = function(e) NULL)
        dt <- data.table::as.data.table(coef_table, keep.rownames = "coef")

        .coef_names <- dt[["coef"]]
        if (!is.null(ci) && all(.coef_names %in% rownames(ci))) {
          dt[, ("ci.lower") := ci[.coef_names, 1]]
          dt[, ("ci.upper") := ci[.coef_names, 2]]
        } else {
          .est <- dt[["Estimate"]]
          .se  <- dt[["Std. Error"]]
          dt[, ("ci.lower") := .est - 1.96 * .se]
          dt[, ("ci.upper") := .est + 1.96 * .se]
        }

        dt[, ("NumObs") := r$nobs]
        dt[, ("df") := fixest::degrees_freedom(r, type = "resid")]

        if (count_firms) {
          removed <- r$obs_selection$obsRemoved
          kept    <- if (is.null(removed) || length(removed) == 0L) r$data else r$data[removed, ]

          NumFirms_val <- if (firm_col %in% names(kept)) {
            data.table::uniqueN(kept[[firm_col]])
          } else {
            as.integer(num_firms)
          }

          NumEnt_val <- if (ent_col %in% names(kept)) {
            data.table::uniqueN(kept[[ent_col]])
          } else {
            as.integer(num_ent)
          }

          dt[, c("NumFirms", "NumEnt") := list(NumFirms_val, NumEnt_val)]
        }

        if (inherits(r, "fixest")) {
          if (iv) {
            fit_types <- c("r2", "ar2", "aic", "bic", "ll", "sargan", "wh", "kpr", "cd",
                           "ivwald", "ivwald1", "ivwald2", "ivwaldall", "ivf", "ivf1", "ivf2", "ivfall")
          } else {
            fit_types <- c("r2", "ar2", "aic", "bic", "ll", "f", "wf", "wr2", "awr2", "my", "rmse")
          }
          fs <- fixest::fitstat(r, type = fit_types)

          dt[, ("R2")     := fs[["r2"]]]
          dt[, ("AdjR2")  := fs[["ar2"]]]
          dt[, ("AIC")    := fs[["aic"]]]
          dt[, ("BIC")    := fs[["bic"]]]
          dt[, ("LogLik") := fs[["ll"]]]

          extract_stat <- function(obj) {
            if (is.list(obj) && "stat" %in% names(obj)) return(obj[["stat"]])
            if (is.numeric(obj) && length(obj) > 1)     return(obj[1])
            if (is.numeric(obj) && length(obj) == 1)    return(obj)
            NA_real_
          }

          if (iv) {
            iv_only <- c("sargan", "wh", "kpr", "cd",
                         "ivwald", "ivwald1", "ivwald2", "ivwaldall", "ivf", "ivf1", "ivf2", "ivfall")
            for (stat_name in iv_only) {
              value <- if (stat_name %in% names(fs)) extract_stat(fs[[stat_name]]) else NA_real_
              dt[, (stat_name) := value]
            }
          } else {
            noniv_only <- c("f", "wf", "wr2", "awr2", "my", "rmse")
            for (stat_name in noniv_only) {
              value <- if (stat_name %in% names(fs)) extract_stat(fs[[stat_name]]) else NA_real_
              dt[, (stat_name) := value]
            }
          }
        } else {
          dt[, c("R2", "AdjR2", "AIC", "BIC", "LogLik") := NA_real_]
        }
        return(dt)
      }), idcol = "model")

      if (tex == TRUE) {
        fixest::etable(regs,
               vcov = vcov,
               tex = TRUE,
               file = paste0(dirOUTPUT, output_name, ".tex"))
        newentry <- paste0(output_name, ": Regression table", "\n")
        write(newentry,
              append = TRUE,
              file = paste0(dirOUTPUT, desc_file, ".txt"))
      }
      return(regDT)
    } else {
      message("Regression models do not satisfy disclosure criteria.")
    }
  }