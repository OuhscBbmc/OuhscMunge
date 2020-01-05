library(testthat)

# ---- range-date -------------------------------------------------------------------
context("range-date")

test_that("only NAs", {
  observed_min_1 <- date_min_with_nas(c(NA))
  observed_min_2 <- date_min_with_nas(c(NA, NA, NA))
  observed_max_1 <- date_max_with_nas(c(NA))
  observed_max_2 <- date_max_with_nas(c(NA, NA, NA))
  observed_range_1 <- date_range_with_nas(c(NA))
  observed_range_2 <- date_range_with_nas(c(NA, NA, NA))

  expect_true(is.na(observed_min_1), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_min_2), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_1), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_2), label = "A vector of NAs should produce NA.")
  expect_equal(observed_range_1, as.Date(c(NA, NA)), label = "A vector of NAs should produce NAs.")
  expect_equal(observed_range_2, as.Date(c(NA, NA)), label = "A vector of NAs should produce NAs.")
})

test_that("empty", {
  observed_min_1 <- date_min_with_nas(as.Date(character(0)))
  observed_max_1 <- date_max_with_nas(as.Date(character(0)))
  observed_range_1 <- date_range_with_nas(as.Date(character(0)))

  expect_true(is.na(observed_min_1), label = "An empty vector should produce NA.")
  expect_true(is.na(observed_max_1), label = "An empty vector should produce NA.")
  expect_equal(observed_range_1, as.Date(c(NA, NA)), label = "A vector of NAs should produce NAs.")
})

test_that("some nonmissing values", {
  x <- as.Date(c("2009-04-21", "2017-12-27", NA_character_))
  expected_min   <- as.Date("2009-04-21")
  expected_max   <- as.Date("2017-12-27")
  expected_range   <- c(expected_min, expected_max)
  observed_min_1 <- date_min_with_nas(x)
  observed_max_1 <- date_max_with_nas(x)
  observed_range_1 <- date_range_with_nas(x)

  expect_equal(observed_min_1, expected_min, label = "The nonmissing value should be returned for the min.")
  expect_equal(observed_max_1, expected_max, label = "The nonmissing value should be returned for the max.")
  expect_equal(observed_range_1, expected_range, label = "A vector of NAs should produce NAs.")
})



# ---- range-numeric -------------------------------------------------------------------
context("range-numeric")

test_that("only NAs", {
  observed_min_1 <- min_with_nas_numeric(c(NA))
  observed_min_2 <- min_with_nas_numeric(c(NA, NA, NA))
  observed_max_1 <- max_with_nas_numeric(c(NA))
  observed_max_2 <- max_with_nas_numeric(c(NA, NA, NA))
  observed_range_1 <- range_with_nas_numeric(c(NA))
  observed_range_2 <- range_with_nas_numeric(c(NA, NA, NA))

  expect_true(is.na(observed_min_1), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_min_2), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_1), label = "A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_2), label = "A vector of NAs should produce NA.")
  expect_equal(observed_range_1, c(NA_real_, NA_real_), label = "A vector of NAs should produce NAs.")
  expect_equal(observed_range_2, c(NA_real_, NA_real_), label = "A vector of NAs should produce NAs.")
})

test_that("empty", {
  observed_min_1 <- min_with_nas_numeric(numeric(0))
  observed_max_1 <- max_with_nas_numeric(numeric(0))
  observed_range_1 <- range_with_nas_numeric(numeric(0))

  expect_true(is.na(observed_min_1), label = "An empty vector should produce NA.")
  expect_true(is.na(observed_max_1), label = "An empty vector should produce NA.")
  expect_equal(observed_range_1, c(NA_real_, NA_real_), label = "A vector of NAs should produce NAs.")
})

test_that("some nonmissing values", {
  x <- c(2.3, 435.4, NA_real_)
  expected_min   <- 2.3
  expected_max   <- 435.4
  expected_range   <- c(expected_min, expected_max)
  observed_min_1 <- min_with_nas_numeric(x)
  observed_max_1 <- max_with_nas_numeric(x)
  observed_range_1 <- range_with_nas_numeric(x)

  expect_equal(observed_min_1, expected_min, label = "The nonmissing value should be returned for the min.")
  expect_equal(observed_max_1, expected_max, label = "The nonmissing value should be returned for the max.")
  expect_equal(observed_range_1, expected_range, label = "A vector of NAs should produce NAs.")
})

# ---- range-integer -------------------------------------------------------------------
context("range-integer")

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
