#' Harmonize a Classification Over Time
#'
#' @description
#' Creates a harmonized concordance for a classification of interest over a
#' time period, starting from year-by-year concordance tables. Each row in
#' `conc_table` maps a code in year `t-1` (column `left`) to a code in year
#' `t` (second column), along with a `year` column indicating which transition
#' the row belongs to.
#'
#' Codes linked by the concordance, directly or through other codes and
#' years, form one group, so 1:1, m:1, 1:m and m:m changes are all handled
#' alike. Every code in a group gets the same harmonized code.
#'
#' @param conc_table A `data.table` of concordance mappings with at least
#'   three columns: `year` (integer transition year), `left` (code at `t-1`),
#'   and a second code column (code at `t`). Rows must cover all transitions
#'   within `year_list`.
#' @param year_list Integer or numeric vector of years of interest, starting
#'   from the first year `t` in the first concordance table (not `t-1`).
#'   `NULL` (the default) uses every year in `conc_table`. Narrow it to the
#'   years your data covers: extra years before the data join codes that
#'   changed before it starts, and extra years after it decide the naming and
#'   the `"D"`.
#' @param code_name Character. Name of the classification variable, used to
#'   label output columns (e.g. `"pcc8"` produces columns `pcc8` and
#'   `pcc8_harmonized`).
#'
#' @return A `data.table` in long format with columns:
#'   \itemize{
#'     \item `year` — the year of the observation.
#'     \item `<code_name>` — the original code for that year.
#'     \item `<code_name>_harmonized` — the group's code: its smallest code
#'       in the latest year the group still has one, with `"D"` appended if
#'       that year is before the last year.
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

mdi_make_conc <- function(conc_table, year_list = NULL, code_name) {
  check_dt(conc_table, c("year", "left"), arg_name = "conc_table")
  if (is.null(year_list)) {
    year_list <- seq(min(conc_table[["year"]], na.rm = TRUE), max(conc_table[["year"]], na.rm = TRUE))
  }
  if (!is.numeric(year_list) || length(year_list) < 1)
    stop("'year_list' must be a non-empty numeric vector")
  check_string(code_name, "code_name")

  start_year <- year_list[1]
  end_year <- year_list[length(year_list)]
  in_range <- conc_table[["year"]] >= start_year & conc_table[["year"]] <= end_year
  conc_list <- split(conc_table[in_range], by = "year", keep.by = FALSE)

  clean_year <- function(conc) {
    conc <- conc[, names(conc)[1:2], with = FALSE]
    data.table::setnames(conc, c("code_t_1", "code_t"))
    # blanks become NA before spaces and dots are stripped, so " " stays ""
    for (col in c("code_t_1", "code_t")) {
      conc[[col]][conc[[col]] == ""] <- NA
    }
    conc[, c("code_t_1", "code_t") := lapply(.SD, gsub, pattern = "[ .]", replacement = ""),
         .SDcols = c("code_t_1", "code_t")]
    conc <- conc[!(is.na(conc[["code_t_1"]]) & is.na(conc[["code_t"]]))]
    # a code with a real partner drops its "new code" / "disappeared" rows
    old <- conc[["code_t_1"]]
    new <- conc[["code_t"]]
    conc <- conc[!is.na(old) | !(new %in% new[!is.na(old)])]
    old <- conc[["code_t_1"]]
    new <- conc[["code_t"]]
    conc <- conc[!is.na(new) | !(old %in% old[!is.na(new)])]
    unique(conc)
  }

  # every year needs its transition rows: without them, every code would look
  # discontinued
  missing_years <- setdiff(as.character(year_list), names(conc_list))
  if (length(missing_years)) {
    stop("mdi_make_conc(): no concordance rows for year(s) ", paste(missing_years, collapse = ", "),
         " (each year from ", start_year, " to ", end_year, " needs its t-1 -> t rows).")
  }

  links <- data.table::rbindlist(lapply(year_list, function(year) {
    conc <- clean_year(conc_list[[as.character(year)]])
    list(year = rep(year, nrow(conc)), code_t_1 = conc[["code_t_1"]], code_t = conc[["code_t"]])
  }))

  # every (year, code) is a node; each concordance row links (t-1, left) to (t, right)
  nodes <- unique(data.table::data.table(
    year = as.numeric(c(links[["year"]] - 1, links[["year"]])),
    code = c(links[["code_t_1"]], links[["code_t"]])
  ))
  nodes <- nodes[!is.na(nodes[["code"]])]
  node_id <- function(year, code) {
    id <- match(paste(year, code), paste(nodes[["year"]], nodes[["code"]]))
    id[is.na(code)] <- NA
    id
  }
  from <- node_id(links[["year"]] - 1, links[["code_t_1"]])
  to <- node_id(links[["year"]], links[["code_t"]])
  linked <- !is.na(from) & !is.na(to)
  group <- connected_groups(nrow(nodes), from[linked], to[linked])

  # a group is named after its smallest code in the latest year it still has one,
  # with "D" if that year is before the last year
  o <- order(group, -nodes[["year"]], nodes[["code"]], method = "radix")
  lead <- o[!duplicated(group[o])]
  lead <- lead[match(group, group[lead])]
  harmonized <- nodes[["code"]][lead]
  gone <- nodes[["year"]][lead] < end_year
  harmonized[gone] <- paste0(harmonized[gone], "D")

  harmonized_codes_long <- data.table::data.table(
    year = nodes[["year"]], code = nodes[["code"]], harmonized_code = harmonized
  )
  data.table::setorderv(harmonized_codes_long, c("year", "code"))
  data.table::setnames(harmonized_codes_long, c("code", "harmonized_code"),
           c(code_name, paste0(code_name, "_harmonized")))

  return(harmonized_codes_long)
}

# Label each of `n` nodes with the smallest node in its connected component,
# given undirected edges a[i]--b[i]. Every node's label is always a node of its
# own component, so following labels to their labels (`group[group]`) is safe
# and saves rounds on long chains.
connected_groups <- function(n, a, b) {
  group <- seq_len(n)
  repeat {
    m <- pmin(group[a], group[b])
    node <- c(a, b)
    m <- c(m, m)
    o <- order(m)
    first <- !duplicated(node[o])
    new <- group
    new[node[o][first]] <- pmin(new[node[o][first]], m[o][first])
    new <- new[new]
    if (identical(new, group)) return(group)
    group <- new
  }
}
