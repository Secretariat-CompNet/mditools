# Shared toy data used across all test files.
# Keep this small (10-20 rows) so tests run fast and examples are self-contained.

make_toy_dt <- function() {
  data.table::data.table(
    firmid  = rep(1:5, each = 2),
    year    = rep(2020:2021, 5),
    nace    = rep(c("A", "B"), 5),
    emp     = c(10, 12, 5, 6, 20, 22, 8, 9, 15, 16),
    rev     = c(100, 110, 50, 55, 200, 210, 80, 85, 150, 155),
    sizeclass = rep(c("small", "large"), 5)
  )
}

make_toy_conc <- function() {
  data.table::data.table(
    left  = as.character(c(1, 2, 2, 3, 4)),
    right = c("A", "B", "C", "D", "D")
  )
}
