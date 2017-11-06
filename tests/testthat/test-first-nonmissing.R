library(testthat)

# ---- first-nonmissing --------------------------------------------------------
context("first-nonmissing")

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


