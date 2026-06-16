#' Harmonize a Classification Over Time
#'
#' @description
#' Creates a harmonized concordance for a classification of interest over a
#' time period, starting from year-by-year concordance tables. Each row in
#' `conc_table` maps a code in year `t-1` (column `left`) to a code in year
#' `t` (second column), along with a `year` column indicating which transition
#' the row belongs to.
#'
#' The function handles 1:1, m:1, 1:m, and m:m code changes, and appends
#' `"D"` to harmonized codes that disappear before the last year.
#'
#' @param conc_table A `data.table` of concordance mappings with at least
#'   three columns: `year` (integer transition year), `left` (code at `t-1`),
#'   and a second code column (code at `t`). Rows must cover all transitions
#'   within `year_list`.
#' @param year_list Integer or numeric vector of years of interest, starting
#'   from the first year `t` in the first concordance table (not `t-1`).
#' @param code_name Character. Name of the classification variable, used to
#'   label output columns (e.g. `"pcc8"` produces columns `pcc8` and
#'   `pcc8_harmonized`).
#'
#' @return A `data.table` in long format with columns:
#'   \itemize{
#'     \item `year` — the year of the observation.
#'     \item `<code_name>` — the original code for that year.
#'     \item `<code_name>_harmonized` — the harmonized code mapped to the
#'       most recent non-missing code in the group.
#'   }
#'
#' @examples
#' library(data.table)
#' conc <- data.table(
#'   year  = c(2011L, 2011L, 2012L, 2012L),
#'   left  = c("A",   "B",   "A",   "C"),
#'   right = c("A",   "B",   "A2",  "C")
#' )
#' mdi_make_conc(conc, 2011:2012, "pcc")
#'
#' @export

mdi_make_conc <- function(conc_table, year_list, code_name) {
  check_dt(conc_table, c("year", "left"), arg_name = "conc_table")
  if (!is.numeric(year_list) || length(year_list) < 1)
    stop("'year_list' must be a non-empty numeric vector")
  check_string(code_name, "code_name")

  conc_table <- data.table::setorderv(conc_table, c("year", "left"))
  conc_list <- split(conc_table, by = "year")
  conc_list <- lapply(conc_list, function(dt) dt[, year := NULL])

  start_year <- year_list[1]
  end_year <- year_list[length(year_list)]
  conc_list <- conc_list[as.integer(names(conc_list)) >= start_year &
                         as.integer(names(conc_list)) <= end_year]

  conc_list <- lapply(conc_list, function(conc) {
    data.table::setnames(conc, old = names(conc)[1:2], new = c("code_t_1", "code_t"))
    conc[, c("code_t_1", "code_t"), with = FALSE]
  })

  conc_list <- lapply(conc_list, function(conc) {
    for (col in c("code_t_1", "code_t")) {
      conc[[col]][conc[[col]] == ""] <- NA
    }
    return(conc)
  })

  for (d in year_list) {
    conc_list[[as.character(d)]][, c("code_t_1", "code_t") := lapply(.SD, function(x) {
      gsub(" ", "", x)
    }), .SDcols = c("code_t_1", "code_t")]
    conc_list[[as.character(d)]][, c("code_t_1", "code_t") := lapply(.SD, function(x) {
      gsub("\\.", "", x)
    }), .SDcols = c("code_t_1", "code_t")]
  }

  for (year in year_list) {
    .dt <- conc_list[[as.character(year)]]
    conc_list[[as.character(year)]] <- .dt[!(is.na(.dt[["code_t_1"]]) & is.na(.dt[["code_t"]]))]
  }

  conc_list <- lapply(conc_list, function(dt) {
    dt <- dt[, if (any(!is.na(.SD[["code_t_1"]]))) .SD[!is.na(.SD[["code_t_1"]])] else .SD, by = "code_t"]
    dt <- dt[, if (any(!is.na(.SD[["code_t"]])))   .SD[!is.na(.SD[["code_t"]])]   else .SD, by = "code_t_1"]
  })

  for (year in year_list) {
    conc_list[[as.character(year)]] <- unique(conc_list[[as.character(year)]])
  }

  for (year in year_list) {
    data.table::setorderv(conc_list[[as.character(year)]], c("code_t_1", "code_t"))
  }

  evol_group <- function(conc_table) {
    conc_table[, ("obs_t_1") := .N, by = "code_t_1"][, ("obs_t") := .N, by = "code_t"]
    conc_table[is.na(conc_table[["code_t_1"]]), ("obs_t_1") := 1]
    conc_table[is.na(conc_table[["code_t"]]),   ("obs_t")   := 1]

    .i <- !is.na(conc_table[["code_t_1"]]) & !is.na(conc_table[["code_t"]])
    conc_table[.i, ("evol") := data.table::fcase(
      .SD[["obs_t_1"]] == 1 & .SD[["obs_t"]] == 1, 1,
      .SD[["obs_t_1"]] < .SD[["obs_t"]] & .SD[["obs_t_1"]] == 1, 2,
      .SD[["obs_t_1"]] > .SD[["obs_t"]] & .SD[["obs_t"]] == 1, 3,
      default = 4
    ), .SDcols = c("obs_t_1", "obs_t")]

    conc_table[.i, ("diff_cat_t_1") := ifelse(length(unique(.SD[["evol"]])) > 1, 1, 0),
               by = "code_t_1", .SDcols = "evol"]
    conc_table[.i, ("diff_cat_t")   := ifelse(length(unique(.SD[["evol"]])) > 1, 1, 0),
               by = "code_t", .SDcols = "evol"]
    conc_table[.i, ("evol") := ifelse(
      .SD[["diff_cat_t_1"]] == 1 | .SD[["diff_cat_t"]] == 1, 4, .SD[["evol"]]
    ), .SDcols = c("diff_cat_t_1", "diff_cat_t", "evol")]
    conc_table[, c("obs_t_1", "obs_t", "diff_cat_t_1", "diff_cat_t") := NULL]

    data.table::setorderv(conc_table, c("evol", "code_t_1", "code_t"))

    conc_table[, ("linked") := .I]
    conc_table[.i, ("linked") := .SD[["linked"]][1], by = "code_t",   .SDcols = "linked"]
    conc_table[.i, ("linked") := .SD[["linked"]][1], by = "code_t_1", .SDcols = "linked"]

    n_start <- 2
    cols    <- c("code_t", "code_t_1")

    pick_by <- function(by_col, pick_second) {
      conc_table[conc_table[["evol"]] == 4,
        ("linked") := {
          link <- unique(.SD[["linked"]])
          if (length(link) > 1 && pick_second) link[2] else link[1]
        },
        by = by_col, .SDcols = "linked"]
    }

    run_all_for_n <- function(n) {
      k <- 2 * n
      combos <- do.call(
        expand.grid,
        c(rep(list(c(FALSE, TRUE)), k), list(KEEP.OUT.ATTRS = FALSE))
      )
      combos <- as.matrix(combos)
      for (i in seq_len(nrow(combos))) {
        picks <- as.logical(combos[i, ])
        for (round in seq_len(n)) {
          pick_by(cols[1], picks[2*round - 1])
          pick_by(cols[2], picks[2*round])
        }
      }
      invisible(NULL)
    }

    prev_after <- NULL
    n <- as.integer(n_start)
    repeat {
      run_all_for_n(n)
      curr_after <- data.table::copy(conc_table[["linked"]])
      if (!is.null(prev_after) && identical(curr_after, prev_after)) break
      prev_after <- curr_after
      n <- n + 1
    }

    conc_table[, ("evol") := NULL]
    invisible(conc_table)
  }

  for (year in year_list) {
    conc_list[[as.character(year)]] <- evol_group(conc_list[[as.character(year)]])
  }

  for (year in year_list) {
    conc_list[[as.character(year)]] <- data.table::setnames(
      conc_list[[as.character(year)]],
      old = c("code_t_1", "code_t"),
      new = c(paste0("code_", as.character(year - 1)), paste0("code_", as.character(year)))
    )
  }

  for (year in year_list) {
    if (year == start_year) {
      history <- data.table::copy(conc_list[[as.character(year)]])
      data.table::setnames(history, "linked", "code_t_1")
      current_code <- paste0("code_", as.character(year))
      history[is.na(history[[current_code]]), (current_code) := "Disappeared code"]
    } else {
      history <- merge(history, conc_list[[as.character(year)]], by = paste0("code_", year - 1), all = TRUE)
      data.table::setnames(history, "linked", "code_t")
      current_code <- paste0("code_", as.character(year))
      history[is.na(history[[current_code]]), (current_code) := "Disappeared code"]
      history[, ("code_t") := {
        .ct <- .SD[["code_t"]]
        fill_link <- .ct[!is.na(.ct)][1]
        if (is.na(fill_link)) {
          data.table::fifelse(is.na(.ct), .BY$code_t_1 * 10000, .ct)
        } else {
          data.table::fifelse(is.na(.ct), fill_link, .ct)
        }
      }, by = "code_t_1", .SDcols = "code_t"]
      history[, ("code_t_1") := {
        .ct1 <- .SD[["code_t_1"]]
        fill_link <- .ct1[!is.na(.ct1)][1]
        if (is.na(fill_link)) {
          data.table::fifelse(is.na(.ct1), .BY$code_t * 10000, .ct1)
        } else {
          data.table::fifelse(is.na(.ct1), fill_link, .ct1)
        }
      }, by = "code_t", .SDcols = "code_t_1"]
      history_unique <- unique(history[, c("code_t_1", "code_t"), with = FALSE])
      history_unique <- evol_group(history_unique)
      history <- merge(history, history_unique, by = c("code_t_1", "code_t"), all.x = TRUE)
      history[, c("code_t_1", "code_t") := NULL]
      data.table::setnames(history, "linked", "code_t_1")
    }
  }

  data.table::setnames(history, "code_t_1", "group")
  code_years <- grep("^code_", names(history), value = TRUE)
  code_years <- c(sort(code_years), "group")
  history <- history[, code_years, with = FALSE]
  history[, (names(history)) := lapply(.SD, function(col) {
    ifelse(col == "Disappeared code", NA, col)
  })]

  code_col_reverse <- paste0("code_", end_year:(start_year - 1))
  history[, ("harmonized_code") := do.call(data.table::fcoalesce, .SD), .SDcols = code_col_reverse]
  history[, ("harmonized_code") := .SD[["harmonized_code"]][1], by = "group", .SDcols = "harmonized_code"]

  lastyear_code <- paste0("code_", as.character(end_year))
  history[, ("harmonized_code") := {
    col_lastyear_code <- .SD[[lastyear_code]]
    .hc <- .SD[["harmonized_code"]]
    if (all(is.na(col_lastyear_code))) paste0(.hc, "D") else .hc
  }, by = "group", .SDcols = c("harmonized_code", lastyear_code)]
  history[, ("group") := NULL]

  code_col <- paste0("code_", (start_year - 1):end_year)
  harmonized_codes_long <- data.table::melt(history,
    id.vars = "harmonized_code",
    measure.vars = code_col, variable.name = "year",
    value.name = "code"
  )
  harmonized_codes_long[, ("year") := as.numeric(substr(as.character(.SD[["year"]]), 6, 9)), .SDcols = "year"]
  data.table::setcolorder(harmonized_codes_long, c("year", "code", "harmonized_code"))
  harmonized_codes_long <- unique(harmonized_codes_long)
  harmonized_codes_long <- harmonized_codes_long[!is.na(harmonized_codes_long[["code"]])]

  data.table::setnames(harmonized_codes_long, c("code", "harmonized_code"),
           c(code_name, paste0(code_name, "_harmonized")))

  return(harmonized_codes_long)
}
