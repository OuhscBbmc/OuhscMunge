library(testthat)

test_that("only NAs", {
  observed_min_1 <- min_with_nas_integer(c(NA))
  observed_min_2 <- min_with_nas_integer(c(NA, NA, NA))
  observed_max_1 <- max_with_nas_integer(c(NA))
  observed_max_2 <- max_with_nas_integer(c(NA, NA, NA))
  observed_range_1 <- range_with_nas_integer(c(NA))
  observed_range_2 <- range_with_nas_integer(c(NA, NA, NA))

  expect_true(is.na(observed_min_1), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_min_2), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_1), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_2), label = "A vector of NAs should produce NA.")
  expect_equal(observed_range_1, c(NA_integer_, NA_integer_), label = "A vector of NAs should produce NAs.")
  expect_equal(observed_range_2, c(NA_integer_, NA_integer_), label = "A vector of NAs should produce NAs.")
})

test_that("empty", {
  observed_min_1 <- min_with_nas_integer(integer(0))
  observed_max_1 <- max_with_nas_integer(integer(0))
  observed_range_1 <- range_with_nas_integer(integer(0))

  expect_true(is.na(observed_min_1), label = "An empty vector should produce NA.")
  expect_true(is.na(observed_max_1), label = "An empty vector should produce NA.")
  expect_equal(observed_range_1, c(NA_real_, NA_real_), label = "A vector of NAs should produce NAs.")
})

test_that("some nonmissing values", {
  x <- c(2.3, 435.4, NA_real_)
  expected_min   <- 2.3
  expected_max   <- 435.4
  expected_range   <- c(expected_min, expected_max)
  observed_min_1 <- min_with_nas_integer(x)
  observed_max_1 <- max_with_nas_integer(x)
  observed_range_1 <- range_with_nas_integer(x)

  expect_equal(observed_min_1, expected_min, label = "The nonmissing value should be returned for the min.")
  expect_equal(observed_max_1, expected_max, label = "The nonmissing value should be returned for the max.")
  expect_equal(observed_range_1, expected_range, label = "A vector of NAs should produce NAs.")
})
