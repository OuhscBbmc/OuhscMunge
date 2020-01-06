library(testthat)

# ---- typical-cases ------------------------------------------------------
test_that("deterge integers", {
  expected <- c(NA, 1:10)
  observed <- OuhscMunge::deterge_to_integer(c(NA, 1:10))

  expect_equal(observed, expected)
  expect_equal(class(observed), "integer", "The returned array should remain a double-precision data type.")
})
test_that("deterge integers; w/ boundaries", {
  expected <- c(NA, NA, NA, NA, 4, 5, 6, 7, 8, NA, NA)
  observed <- OuhscMunge::deterge_to_integer(c(NA, 1:10), 4L, 8L)

  expect_equal(observed, expected)
  expect_equal(class(observed), "integer", "The returned array should remain a double-precision data type.")
})


# ---- expected-errors -------------------------------------------------------
test_that("deterge integer --bad lower bound type", {
  expect_error(
    OuhscMunge::deterge_to_integer(5:40, bound_lower = "ds")
    , regexp = "The parameter `bound_lower` must be a numeric or integer vector with exactly one element."
  )
})
test_that("deterge integer --bad lower bound length", {
  expect_error(
    OuhscMunge::deterge_to_integer(5:40, bound_lower = 12:23)
    , regexp = "The parameter `bound_lower` must be a numeric or integer vector with exactly one element."
  )
})
test_that("deterge integer --bad upper  bound type", {
  expect_error(
    OuhscMunge::deterge_to_integer(5:40, bound_upper = "ds")
    , regexp = "The parameter `bound_upper` must be a numeric or integer vector with exactly one element."
  )
})
test_that("deterge integer --bad upper bound length", {
  expect_error(
    OuhscMunge::deterge_to_integer(5:40, bound_upper = 12:23)
    , regexp = "The parameter `bound_upper` must be a numeric or integer vector with exactly one element."
  )
})
