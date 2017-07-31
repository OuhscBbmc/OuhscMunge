library(testthat)


# ---- first-nonmissing --------------------------------------------------------
context("first-nonmissing")


test_that("first_nonmissing", {
  testthat::expect_equal(first_nonmissing(letters), "a")
  testthat::expect_equal(first_nonmissing(c(NA, "b", "c")), "b")
  testthat::expect_equal(first_nonmissing(c(NA, NA)), NA)
  # testthat::expect_equal(first_nonmissing(character(0)), NA)
})


