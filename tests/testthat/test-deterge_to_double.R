library(testthat)

# ---- typical-cases -------------------------------------------------------
test_that("deterge double with round numbers", {
  expected <- c(NA, 1:10)
  observed <- OuhscMunge::deterge_to_double(c(NA, 1:10))

  expect_equal(observed, expected)
  expect_equal(class(observed), "numeric", "The returned array should remain a double-precision data type.")
})

test_that("deterge double with round numbers; w/ boundaries", {
  expected <- c(NA, NA, NA, NA, 4, 5, 6, 7, 8, NA, NA)
  observed <- OuhscMunge::deterge_to_double(c(NA, 1:10), 4, 8)

  expect_equal(observed, expected)
  expect_equal(class(observed), "numeric", "The returned array should remain a double-precision data type.")
})

test_that("deterge double with decimals", {
  expected <- c(NA, NA, NA, NA, 4, 5, 6, 7, 8, NA, NA)
  observed <- OuhscMunge::deterge_to_double(c(NA, 1:10), 3.2, 8.5)

  expect_equal(observed, expected)
  expect_equal(class(observed), "numeric", "The returned array should remain a double-precision data type.")
})

test_that("deterge double with decimals; w/ boundaries", {
  expected <- c(NA, 1:10)
  observed <- OuhscMunge::deterge_to_double(c(NA, 1:10))

  expect_equal(observed, expected)
  expect_equal(class(observed), "numeric", "The returned array should remain a double-precision data type.")
})

# ---- expected-errors -------------------------------------------------------
test_that("deterge double --bad lower bound type", {
  expect_error(
    OuhscMunge::deterge_to_double(5:40, bound_lower = "ds")
    , regexp = "The parameter `bound_lower` must be a numeric or integer vector with exactly one element."
  )
})
test_that("deterge double --bad lower bound length", {
  expect_error(
    OuhscMunge::deterge_to_double(5:40, bound_lower = 12:23)
    , regexp = "The parameter `bound_lower` must be a numeric or integer vector with exactly one element."
  )
})
test_that("deterge double --bad upper  bound type", {
  expect_error(
    OuhscMunge::deterge_to_double(5:40, bound_upper = "ds")
    , regexp = "The parameter `bound_upper` must be a numeric or integer vector with exactly one element."
  )
})
test_that("deterge double --bad upper bound length", {
  expect_error(
    OuhscMunge::deterge_to_double(5:40, bound_upper = 12:23)
    , regexp = "The parameter `bound_upper` must be a numeric or integer vector with exactly one element."
  )
})
