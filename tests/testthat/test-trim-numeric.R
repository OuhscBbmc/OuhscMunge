library(testthat)

test_that("trim_numeric -good", {
  x           <- c(NA, -0.4, -0.3, -0.2, -.16, -.15, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.80001, 0.9, 1)
  bounds      <- c(-.15, .8)
  replacement <- 99

  expected_1  <- c(NA, NA, NA, NA, NA, -.15, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, NA, NA, NA)
  expected_2  <- c(NA, 99, 99, 99, 99, -.15, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 99, 99, 99)

  observed_1  <- OuhscMunge::trim_numeric(x, bounds)
  observed_2  <- OuhscMunge::trim_numeric(x, bounds, replacement)

  expect_equal(observed_1, expected_1)
  expect_equal(observed_2, expected_2)

  expect_equal(class(observed_1), "numeric", "The returned array should remain a double-precision data type.")
  expect_equal(class(observed_2), "numeric", "The returned array should remain a double-precision data type.")
})

test_that("trim_numeric -degenerate", {
  bounds      <- c(-.15, .8)

  expect_equal(OuhscMunge::trim_numeric(NA_real_, bounds), NA_real_)
  expect_equal(OuhscMunge::trim_numeric(numeric(0), bounds), numeric(0))
})

test_that("trim_numeric -bad values", {
  x           <- c(NA, -4:10) / 10

  expect_error(
    OuhscMunge::trim_numeric(x, NA_real_),
    "Assertion on 'bounds' failed: Must have length >= 2, but has length 1\\."
  )
  expect_error(
    OuhscMunge::trim_numeric(x, c(6, 2)),
    "The lower element of `bounds` must be equal or less than the upper element of `bounds`."
  )

  expect_error(
    OuhscMunge::trim_numeric(x, c(1, 2, 3)),
    "Assertion on 'bounds' failed: Must have length <= 2, but has length 3\\."
  )
})
