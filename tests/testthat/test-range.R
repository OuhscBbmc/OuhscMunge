library(testthat)

# ---- range-date -------------------------------------------------------------------
context("range-date")

test_that("only NAs", {
  observed_min_1 <- date_min_with_nas(c(NA))
  observed_min_2 <- date_min_with_nas(c(NA, NA, NA))
  observed_max_1 <- date_max_with_nas(c(NA))
  observed_max_2 <- date_max_with_nas(c(NA, NA, NA))

  expect_true(is.na(observed_min_1), label="A vector of NAs should produce NA.")
  expect_true(is.na(observed_min_2), label="A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_1), label="A vector of NAs should produce NA.")
  expect_true(is.na(observed_max_2), label="A vector of NAs should produce NA.")
})

test_that("empty", {
  observed_min_1 <- date_min_with_nas(as.Date(character(0)))
  observed_max_1 <- date_max_with_nas(as.Date(character(0)))

  expect_true(is.na(observed_min_1), label="An empty vector should produce NA.")
  expect_true(is.na(observed_max_1), label="An empty vector should produce NA.")
})

test_that("some nonmissing values", {
  x <- as.Date(c("2009-04-21", "2017-12-27", NA_character_))
  expected_min   <- as.Date("2009-04-21")
  expected_max   <- as.Date("2017-12-27")
  observed_min_1 <- date_min_with_nas(x)
  observed_max_1 <- date_max_with_nas(x)

  expect_equal(observed_min_1, expected_min, label="The nonmissing value should be returned for the min.")
  expect_equal(observed_max_1, expected_max, label="The nonmissing value should be returned for the max.")
})
