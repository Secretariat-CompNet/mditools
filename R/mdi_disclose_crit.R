#' Add Disclosure Criteria to Aggregated Data
#'
#' @description
#' Computes and attaches disclosure-control variables to an aggregated dataset.
#' The function calculates a dominance measure and the number of non-missing
#' observations per group.
#'
#' Two dominance formulas are available via `dom_formula`:
#' - `"top_share"` (default): share of the top `domNr` firms in the group total.
#' - `"residual"`: \eqn{(Total - x_1 - x_2) / x_1}, where \eqn{x_1} and
#'   \eqn{x_2} are the two largest values. Used when the dominance criterion
#'   is defined as the residual relative to the largest firm.
#'
#' This function is normally called internally by [`mdi_aggregate()`] when
#' `disclosure = TRUE`, but can also be used standalone.
#'
#' @param DT A `data.table` containing the aggregated dataset.
#' @param domVar Character. Variable used for the dominance criterion.
#'   Use `"var"` (default) to compute dominance for all variables in
#'   `var_list`, or supply the name of a single numeric column already
#'   present in `DT` (e.g. `"emp"`, `"nq"`).
#' @param domNr Numeric. Number of top firms to consider in the dominance
#'   criterion (e.g. top 1, 2, or 3). Default `2`.
#' @param bygroups Character vector of grouping variables, as in
#'   [`mdi_aggregate()`].
#' @param var_list Character vector of variables to include when
#'   `domVar = "var"`. Usually the same as in [`mdi_aggregate()`].
#'   Default `NULL`.
#' @param dom_formula Character. Formula used to compute the dominance share.
#'   `"top_share"` (default) computes the share of the top `domNr` firms in
#'   the group total. `"residual"` computes \eqn{(Total - x_1 - x_2) / x_1}.
#'   Only applies when `domVar = "var"`.
#' @param count_firms Logical. If `TRUE`, the number of unique firms and
#'   enterprises per group are computed and added to the output as `NumFirms`
#'   and `NumEnt`, using `firm_col` and `ent_col`. Default `FALSE`.
#' @param firm_col Character. Column name used to count unique firms when
#'   `count_firms = TRUE`. Default `"firmid"`.
#' @param ent_col Character. Column name used to count unique enterprises when
#'   `count_firms = TRUE`. Default `"entid"`.
#'
#' @return A `data.table` with the same grouping structure as the input, plus:
#'   - One or more `domPerc_*` columns: dominance share per group.
#'   - A column `NumObs`: number of non-missing observations per group.
#'   - `NumFirms` and `NumEnt` (only when `count_firms = TRUE`).
#'
#' @details
#' - For `domVar != "var"`, the named column must already be present in `DT`.
#'   One dominance column (`domPerc`) is returned and `dom_formula` is ignored.
#' - For `domVar = "var"`, separate dominance columns are created for each
#'   variable in `var_list` (`domPerc_<var>`), using the formula specified
#'   by `dom_formula`.
#' - When `count_firms = TRUE`, `firm_col` and `ent_col` must be present in
#'   `DT`; the function stops with an error if either is missing.
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   nace   = rep(c("A", "B"), each = 5),
#'   year   = rep(2020L, 10),
#'   emp    = c(10, 20, 5, 15, 8, 12, 25, 6, 14, 9),
#'   firmid = 1:10,
#'   entid  = c(1,1,2,2,3,4,4,5,5,6)
#' )
#'
#' # Standard top-share formula
#' mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
#'             bygroups = c("nace", "year"), var_list = "emp")
#'
#' # Residual formula with firm counts
#' mdi_disclose_crit(DT, domVar = "var", domNr = 2L,
#'             bygroups = c("nace", "year"), var_list = "emp",
#'             dom_formula = "residual", count_firms = TRUE)
#'
#' # Single-column dominance (column must be present in DT)
#' mdi_disclose_crit(DT, domVar = "emp", domNr = 2L,
#'             bygroups = c("nace", "year"))
#'
#' @export

mdi_disclose_crit <-
  function(DT,
           domVar = "var",
           domNr = 2,
           bygroups,
           var_list = NULL,
           dom_formula  = c("top_share", "residual"),
           count_firms  = FALSE,
           firm_col     = "firmid",
           ent_col      = "entid") {

    dom_formula <- match.arg(dom_formula)
    check_string(domVar, "domVar")
    check_char_vec(bygroups, "bygroups")
    check_dt(DT)
    if (domVar != "var" && !domVar %in% names(DT))
      stop("'domVar' must be \"var\" or a column present in DT. Column '",
           domVar, "' not found in DT.")
    if (count_firms) {
      check_dt(DT, firm_col)
      check_dt(DT, ent_col)
    }
    
    # Loop the calculation of the dominance criterion if domVar == 'var'
    if (domVar == 'var') {
      .use_residual <- dom_formula == "residual"
      # Dominance shares are only meaningful for numeric variables
      numeric_var_list <- var_list[var_list %in% names(DT)[vapply(DT, is.numeric, FUN.VALUE = logical(1))]]
      DTDC <- DT[, {
        out <- lapply(.SD, function(x) {
          v <- sum(x, na.rm = TRUE)
          if (v == 0) return(NA_real_)
          
          if (.use_residual) {
            # DE formula: (X - x1 - x2) / x1
            ord <- order(x, decreasing = TRUE, na.last = NA)
            if (length(ord) < 1L) return(NA_real_)
            x1 <- x[ord[1L]]
            x2 <- if (length(ord) >= 2L) x[ord[2L]] else 0
            if (is.na(x1) || x1 == 0) return(NA_real_)
            (v - x1 - x2) / x1
          } else {
            # Original: share of top domNr values
            idx <- head(order(x, decreasing = TRUE, na.last = NA), domNr)
            sum(x[idx], na.rm = TRUE) / v
          }
        })
        
        setNames(out, paste0("domPerc_", names(.SD)))
      }, by = bygroups, .SDcols = numeric_var_list]
      
      # Calculate number of observations per subgroup

      if (count_firms) {
        DTde <- DT[, list(NumFirms = data.table::uniqueN(.SD[[firm_col]], na.rm = TRUE),
                          NumEnt   = data.table::uniqueN(.SD[[ent_col]],  na.rm = TRUE)),
                   by = bygroups]
      }
      .id_col <- intersect(c("firmid", "entid", "plantid", "entgrp"), names(DT))[1]
      DTC <- if (!is.na(.id_col)) {
        DT[, list(NumObs = sum(!is.na(.SD[[.id_col]]))), by = bygroups]
      } else {
        DT[, list(NumObs = .N), by = bygroups]
      }
      rm(.id_col)
        
    } else {
      # For dominance variables employment and sales
      DTDC <- DT[, list(domPerc = vapply(.SD, function(x) {
        y <- sum(sort(x, decreasing = TRUE)[seq_len(domNr)])
        v <- sum(x, na.rm = TRUE)
        return(y / v)
      }, FUN.VALUE = numeric(1))), by = bygroups, .SDcols = domVar]

      DTC <-
        DT[, list(NumObs = vapply(.SD, function(x)
          sum(!is.na(x)), FUN.VALUE = numeric(1))), by = bygroups, .SDcols = domVar]
    }
    
    # Merge
    DTout <- merge(DTDC, DTC, by = bygroups)
    if (count_firms) {
      DTout <- merge(DTout, DTde, by = bygroups)
    }
    
    return(DTout)
  }

