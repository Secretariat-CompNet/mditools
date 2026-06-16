#' Disclosure Check and Masking for Regression Tables
#'
#' @description
#' Applies disclosure control rules to regression output tables.
#' Rows that do not meet minimum thresholds for degrees of freedom, number of
#' observations, or (for Germany) number of firms are flagged and sensitive
#' regression statistics are masked.
#'
#' @param DT A `data.table` containing regression output. Must include the
#'   columns `"coef"` and—depending on `disc_method`—either `"df"` and
#'   `"NumObs"` (for `"obs_df"`) or `"NumFirms"` and `"NumEnt"` (for
#'   `"firm_count"`).
#' @param min_obs Numeric. Minimum threshold used for disclosure checks.
#'   Default `5`.
#' @param show_disclosed Logical. If `TRUE`, disclosed values are shown even
#'   when flagged. If `FALSE` (default), disclosed values are masked with `NA`.
#' @param disc_method Character. Disclosure rule to apply. `"obs_df"` (default)
#'   flags rows where `df < min_obs` or `NumObs < min_obs`. `"firm_count"` flags
#'   rows where `NumFirms < min_obs` or `NumEnt < min_obs` (used for Germany).
#'
#' @return A list with three elements:
#'   \item{DT}{A `data.table` of the regression output with disclosure rules applied.
#'   Masked cells are set to `NA` (unless `show_disclosed = TRUE`).}
#'   \item{vars}{A character string listing the disclosed coefficient names
#'   (for use in output description). If all rows are masked, returns
#'   `"No coefficients disclosed (all masked)"`.}
#'   \item{redacted_n}{An integer count of the number of rows flagged and masked.}
#'
#' @details
#' - **`disc_method = "obs_df"`:** disclosure is triggered when either
#'   `df < min_obs` or `NumObs < min_obs`.
#' - **`disc_method = "firm_count"`:** disclosure is based on
#'   `NumFirms < min_obs` or `NumEnt < min_obs`. Used for Germany.
#' - The following regression statistics may be masked if disclosure applies:
#'   `"Estimate"`, `"Std. Error"`, `"z value"`, `"Pr(>|z|)"`,
#'   `"ci.lower"`, `"ci.upper"`, `"R2"`, `"AdjR2"`, `"AIC"`, `"BIC"`, `"LogLik"`.
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   coef         = c("(Intercept)", "x1"),
#'   Estimate     = c(1.2, 0.5),
#'   `Std. Error` = c(0.1, 0.05),
#'   df           = c(20L, 20L),
#'   NumObs       = c(25L, 25L)
#' )
#' result <- mdi_disclose_reg_tab(DT, min_obs = 3L)
#' result$DT
#' result$redacted_n
#'
#' @export

mdi_disclose_reg_tab <- function(DT, min_obs = 5, show_disclosed = FALSE,
                             disc_method = c("obs_df", "firm_count")) {

  disc_method <- match.arg(disc_method)
  check_dt(DT)

  required_cols <- "coef"
  if (disc_method == "obs_df") {
    required_cols <- c(required_cols, "df", "NumObs")
  } else {
    required_cols <- c(required_cols, "NumFirms", "NumEnt")
  }

  if (!all(required_cols %in% colnames(DT))) {
    stop(paste0("DT must include columns: ",
                paste(required_cols, collapse = ", ")))
  }

  DT <- data.table::copy(DT)

  .flag <- "disclosure_flag"
  if (disc_method == "firm_count") {
    DT[, (.flag) := DT[["NumFirms"]] < as.integer(min_obs) | DT[["NumEnt"]] < as.integer(min_obs)]
  } else {
    DT[, (.flag) := DT[["df"]] < as.integer(min_obs) | DT[["NumObs"]] < as.integer(min_obs)]
  }

  mask_cols <- intersect(
    c(
      "Estimate", "Std. Error", "z value", "Pr(>|z|)",
      "ci.lower", "ci.upper", "R2", "AdjR2", "AIC", "BIC", "LogLik"
    ),
    colnames(DT)
  )

  if (length(mask_cols) > 0 && !show_disclosed) {
    DT[DT[["disclosure_flag"]] == TRUE, (mask_cols) := lapply(.SD, function(x) NA), .SDcols = mask_cols]
  }

  redacted_n <- sum(DT[["disclosure_flag"]], na.rm = TRUE)

  vars <- paste(unique(DT[["coef"]][!is.na(DT[["Estimate"]])]), collapse = "; ")
  if (vars == "") vars <- "No coefficients disclosed (all masked)"

  return(list(DT = DT, vars = vars, redacted_n = redacted_n))
}