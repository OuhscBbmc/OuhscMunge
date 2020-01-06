library(testthat)

test_that("trim_integer -good", {
  x           <- c(NA, -4:10)
  bounds      <- c(-1L, 8L)
  replacement <- 99L

  expected_1  <- c(NA, NA, NA, NA, -1L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, NA, NA)
  expected_2  <- c(NA, 99L, 99L, 99L, -1L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 99L, 99L)

  observed_1  <- OuhscMunge::trim_integer(x, bounds)
  observed_2  <- OuhscMunge::trim_integer(x, bounds, replacement)

  expect_equal(observed_1, expected_1)
  expect_equal(observed_2, expected_2)

  expect_equal(class(observed_1), "integer", "The returned array should remain a double-precision data type.")
  expect_equal(class(observed_2), "integer", "The returned array should remain a double-precision data type.")
})

test_that("trim_integer -degenerate", {
  bounds      <- c(-1L, 8L)

  expect_equal(OuhscMunge::trim_integer(NA_integer_, bounds), NA_integer_)
  expect_equal(OuhscMunge::trim_integer(integer(0), bounds), integer(0))
})

test_that("trim_integer -bad values", {
  x           <- c(NA, -4:10)
  bounds      <- c(-1L, 8L)
  replacement <- 99L

  expect_error(
    OuhscMunge::trim_integer(x, NA_integer_),
    "Assertion on 'bounds' failed: Must have length >= 2, but has length 1."
  )
  expect_error(
    OuhscMunge::trim_integer(x, c(6L, 2L)),
    "The lower element of `bounds` must be equal or less than the upper element of `bounds`."
  )

  expect_error(
    OuhscMunge::trim_integer(x, c(1L, 2L, 3L)),
    "Assertion on 'bounds' failed: Must have length <= 2, but has length 3."
  )
})
