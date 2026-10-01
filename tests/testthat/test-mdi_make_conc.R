library(data.table)

make_conc_input <- function() {
  data.table(
    year  = c(2011L, 2011L, 2012L, 2012L),
    left  = c("A",   "B",   "A",   "C"),
    right = c("A",   "B",   "A2",  "C")
  )
}

test_that("returns a data.table in long format", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "pcc")
  expect_s3_class(result, "data.table")
  expect_true(all(c("year", "pcc", "pcc_harmonized") %in% names(result)))
})

test_that("output has one row per year-code combination", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "pcc")
  expect_true(nrow(result) > 0)
  expect_equal(anyDuplicated(result[, c("year", "pcc"), with = FALSE]), 0L)
})

test_that("harmonized codes are non-NA for all rows", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "pcc")
  expect_false(any(is.na(result$pcc_harmonized)))
})

test_that("code_name controls output column names", {
  result <- mdi_make_conc(make_conc_input(), 2011:2012, "nace")
  expect_true("nace" %in% names(result))
  expect_true("nace_harmonized" %in% names(result))
})

test_that("1:1 codes map to themselves as harmonized code", {
  conc <- data.table(
    year  = c(2011L, 2012L),
    left  = c("A",   "A"),
    right = c("A",   "A")
  )
  result <- mdi_make_conc(conc, 2011:2012, "cd")
  a_rows <- result[result[["cd"]] == "A"]
  expect_true(all(a_rows$cd_harmonized == "A"))
})

test_that("error on non-data.table input", {
  expect_error(
    mdi_make_conc(as.data.frame(make_conc_input()), 2011:2012, "pcc"),
    "'conc_table' must be a data.table"
  )
})

test_that("error when required columns missing", {
  bad <- data.table(x = 1:3, y = 4:6)
  expect_error(
    mdi_make_conc(bad, 2011:2012, "pcc"),
    "columns not found"
  )
})

test_that("error on non-numeric year_list", {
  expect_error(
    mdi_make_conc(make_conc_input(), "2011", "pcc"),
    "'year_list' must be a non-empty numeric vector"
  )
})

test_that("error on non-character code_name", {
  expect_error(
    mdi_make_conc(make_conc_input(), 2011:2012, 123),
    "must be a non-empty character string"
  )
})
# Cases from the M5 rewrite (connected components, naming rule, year_list default)
conc_of <- function(year, left, right) {
  data.table::data.table(year = year, left = left, right = right)
}
label_of <- function(out, in_year, in_code) {
  out[["c_harmonized"]][out[["year"]] == in_year & out[["c"]] == in_code]
}

test_that("a group is named after a code that still exists", {
  # as SI pcc8 25112330/25112360 -> 25112355: A splits into A1 and A2,
  # then only A2 survives, so A1 must not name the group
  conc <- conc_of(c(2011, 2011, 2012, 2012), c("A", "A", "A1", "A2"),
                  c("A1", "A2", "", "A2"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_equal(unique(out[["c_harmonized"]]), "A2")
})

test_that("with several last-year codes, the group takes the smallest", {
  conc <- conc_of(c(2011, 2011, 2011), c("B", "C", "C"), c("Y", "Y", "X"))
  out <- mdi_make_conc(conc, 2011, "c")
  expect_equal(unique(out[["c_harmonized"]]), "X")
})

test_that("a group gone before the last year keeps its latest code, with D", {
  conc <- conc_of(c(2011, 2011, 2012, 2012), c("A", "B", "A2", "B"),
                  c("A2", "B", "", "B"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_equal(label_of(out, 2010, "A"), "A2D")
  expect_equal(label_of(out, 2012, "B"), "B")
})

test_that("codes that disappear in the same year stay in separate groups", {
  # the mditools port reused a row filter after re-sorting, which grouped
  # every disappearing code together
  conc <- conc_of(rep(2011, 4), c("A", "A", "B", "C"), c("A1", "A2", "", ""))
  out <- mdi_make_conc(conc, 2011, "c")
  expect_equal(label_of(out, 2010, "B"), "BD")
  expect_equal(label_of(out, 2010, "C"), "CD")
  expect_equal(label_of(out, 2010, "A"), "A1")
})

test_that("each (year, code) gets exactly one harmonized code", {
  conc <- conc_of(c(2011, 2011, 2011, 2012, 2012), c("A", "A", "B", "A1", "A2"),
                  c("A1", "A2", "A2", "Z", "Z"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_false(anyDuplicated(out[, c("year", "c"), with = FALSE]) > 0)
  expect_equal(unique(out[["c_harmonized"]]), "Z")
})

test_that("mdi_make_conc does not modify the caller's table", {
  conc <- conc_of(c(2012, 2011), c("B", "A"), c("B", "B"))
  before <- data.table::copy(conc)
  mdi_make_conc(conc, 2011:2012, "c")
  expect_identical(conc, before)
})

test_that("an unchanged code is its own group", {
  conc <- conc_of(c(2011, 2011, 2012, 2012), c("A", "B", "A", "B"), c("A", "B", "A", "B"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_equal(nrow(out), 6L)
  expect_true(all(out[["c"]] == out[["c_harmonized"]]))
})

test_that("merged codes form one group", {
  conc <- conc_of(c(2011, 2011, 2012, 2012), c("A", "B", "A", "B"), c("A", "B", "C", "C"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_equal(label_of(out, 2010, "A"), "C")
  expect_equal(label_of(out, 2011, "B"), "C")
})

test_that("codes are linked through other codes and years", {
  # A -> B, then B splits into C and D, and E merges into D: one group
  conc <- conc_of(c(2011, 2011, 2012, 2012, 2012), c("A", "E", "B", "B", "E"), c("B", "E", "C", "D", "D"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_equal(unique(out[["c_harmonized"]]), "C")
  expect_setequal(unique(out[["c"]]), c("A", "B", "C", "D", "E"))
})

test_that("a new code is its own group", {
  conc <- conc_of(c(2011, 2012, 2012), c("A", "A", NA), c("A", "A", "N"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_equal(label_of(out, 2012, "N"), "N")
})

test_that("a real link wins over a contradicting new-code row", {
  # B is listed as new in 2012 and also as A's successor: it is A's successor
  conc <- conc_of(c(2011, 2012, 2012), c("A", "A", NA), c("A", "B", "B"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_equal(label_of(out, 2011, "A"), "B")
  expect_equal(sum(out[["year"]] == 2012 & out[["c"]] == "B"), 1L)
})

test_that("spaces and dots are stripped from codes, and blanks read as missing", {
  conc <- conc_of(c(2011, 2012, 2012), c("01.11", "0111", ""), c("01 11", "0111", "0222"))
  out <- mdi_make_conc(conc, 2011:2012, "c")
  expect_setequal(out[["c"]], c("0111", "0222"))
})

test_that("a year without concordance rows is named in the error", {
  conc <- conc_of(c(2011, 2013), c("A", "A"), c("A", "A"))
  expect_error(mdi_make_conc(conc, 2011:2013, "c"), "no concordance rows for year\\(s\\) 2012")
})

test_that("year_list defaults to every year of the concordance", {
  conc <- conc_of(c(2011, 2012, 2013), c("A", "A", "B"), c("A", "B", "B"))
  expect_identical(mdi_make_conc(conc, code_name = "c"), mdi_make_conc(conc, 2011:2013, "c"))
  # a narrower range ends earlier: A is not yet B in 2011
  expect_equal(unique(mdi_make_conc(conc, 2011, "c")[["c_harmonized"]]), "A")
})
