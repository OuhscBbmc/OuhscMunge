library(testthat)

# ---- first-nonmissing --------------------------------------------------------
test_that("first_nonmissing -normal", {
  testthat::expect_equal(first_nonmissing(letters), "a")
  testthat::expect_equal(first_nonmissing(c(NA, "b", "c")), "b")
})

test_that("first_nonmissing -all NAs", {
  testthat::expect_equal(first_nonmissing(c(NA, NA)), NA)
  testthat::expect_equal(first_nonmissing(c(NA_integer_, NA_integer_)), NA_integer_)
  testthat::expect_equal(first_nonmissing(c(NA_character_, NA_character_)), NA_character_)
})

test_that("first_nonmissing -empty", {
  testthat::expect_equal(first_nonmissing(integer(0)), NA_integer_)
  testthat::expect_equal(first_nonmissing(character(0)), NA_character_)
})


test_that("first_nonmissing -normal with codes", {
  testthat::expect_equal(first_nonmissing(c("unknown", "b", "c"), na_codes = c("unknown")), "b")
  testthat::expect_equal(first_nonmissing(c("unknown", "b", "c"), na_codes = c("missing", "unknown")), "b")
})

test_that("first_nonmissing -all NAs or codes", {
  testthat::expect_equal(first_nonmissing(c(NA_integer_, NA_integer_), na_codes = 254L), NA_integer_)
  testthat::expect_equal(first_nonmissing(c(NA_integer_, NA_integer_, 254L), na_codes = 254L), NA_integer_)
  testthat::expect_equal(first_nonmissing(c(NA_character_), na_codes = "unknown"), NA_character_)
  testthat::expect_equal(first_nonmissing(c(NA_character_, "unknown"), na_codes = "unknown"), NA_character_)
})


test_that("first_nonmissing -all NAs or codes and value_if_all_na", {
  testthat::expect_equal(first_nonmissing(c(NA_integer_, NA_integer_, 254L), na_codes = 254L, value_if_all_na = 100L), 100L)
  testthat::expect_equal(first_nonmissing(c(NA_character_, "unknown"), na_codes = "unknown", value_if_all_na = "missing"), "missing")
})
