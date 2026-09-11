#' Ordered levels of a hierarchy file
#'
#' @description
#' Hierarchy tables name their columns `h_0`, `h_1`, ... — the number is the
#' level index, finest first. `hier_levels()` returns those columns in level
#' order, read from the index in each name rather than from column position
#' or alphabetical sorting. Both have been wrong before: column order breaks
#' silently if a file is ever re-saved with its columns moved, and `sort()`
#' puts `h_10` before `h_2`.
#'
#' @param hhfile A hierarchy `data.table` whose columns are named `h_0`,
#'   `h_1`, ... .
#'
#' @return A character vector of column names, finest level first.
#'
#' @examples
#' hhfile <- data.table::data.table(h_0 = character(), h_2 = character(),
#'                                   h_1 = character())
#' hier_levels(hhfile)  # "h_0" "h_1" "h_2"
#'
#' @keywords internal
#' @export
hier_levels <- function(hhfile) {
  n <- names(hhfile)
  i <- suppressWarnings(as.integer(sub("^h_", "", n)))
  if (anyNA(i)) {
    stop("hier_levels(): a hierarchy file's columns must all be named h_<level>, ",
         "the number being the level index with 0 the finest. Offending column(s): ",
         paste(n[is.na(i)], collapse = ", "), ".")
  }
  n[order(i)]
}
