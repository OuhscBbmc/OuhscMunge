library(testthat)

test_that("Replace with NAs -character", {
  input    <- c("a", "b", "", "d", "")
  expected <- c("a", "b", NA, "d", NA)

  actual_1 <- replace_with_nas(input)
  actual_2 <- replace_with_nas(input, return_type = "character")

  expect_equal(actual_1, expected, label = "The blank values should have been replaced.")
  expect_equal(class(actual_1), "character", "The returned array should remain a character.")

  expect_equal(actual_2, expected, label = "The blank values should have been replaced.")
  expect_equal(class(actual_2), "character", "The returned array should remain a character.")
})

test_that("Replace with NAs -integer", {
  input      <- c(1, 2, "", "", 5)
  expected_1 <- c(1, 2, NA_character_, NA_character_, 5)
  expected_2 <- expected_1
  expected_3 <- c(1, 2, NA_integer_  , NA_integer_  , 5)
  expected_4 <- c(1, 2, NA_real_     , NA_real_     , 5)

  actual_1   <- replace_with_nas(input, return_type = "character")
  actual_2   <- replace_with_nas(input) #Equivalent to previous line.
  actual_3   <- replace_with_nas(input, return_type = "integer")
  actual_4   <- replace_with_nas(input, return_type = "numeric")

  expect_equal(actual_1, expected_1, label = "The blank values should have been replaced.")
  expect_equal(class(actual_1), "character", "The returned array should remain a character.")

  expect_equal(actual_2, expected_2, label = "The blank values should have been replaced.")
  expect_equal(class(actual_2), "character", "The returned array should remain a character.")

  expect_equal(actual_3, expected_3, label = "The blank values should have been replaced.")
  expect_equal(class(actual_3), "integer", "The returned array should be converted to an integer.")

  expect_equal(actual_4, expected_4, label = "The blank values should have been replaced.")
  expect_equal(class(actual_4), "numeric", "The returned array should be converted to a numeric.")
})

test_that("Replace with NAs -date", {
  input    <- c("2011-02-03", "", "", "2011-02-24")
  expected <- as.Date(c("2011-02-03", NA, NA, "2011-02-24"))

  actual_1 <- replace_with_nas(input, return_type = "Date")

  expect_equal(actual_1, expected, label = "The blank values should have been replaced.")
  expect_equal(class(actual_1), "Date", "The returned array should be converted to a Date")
})

test_that("Replace with NAs -logical", {
  input_1  <- c("T", "", "", "F", "FALSE", "", "TRUE")
  input_2  <- c("1", "", "", "0", "0"    , "", "1")
  expected <- c(T  , NA, NA, F  , F      , NA, T)

  actual_1 <- replace_with_nas(input_1, return_type = "logical")
  actual_2 <- replace_with_nas(input_2, return_type = "logical")

  expect_equal(actual_1, expected, label = "The blank values should have been replaced.")
  expect_equal(class(actual_1), "logical", "The returned array should be converted to a Boolean/logical.")

  expect_equal(actual_2, expected, label = "The blank values should have been replaced.")
  expect_equal(class(actual_2), "logical", "The returned array should be converted to a Boolean/logical.")
})

test_that("Replace with NAs -actual date", {
  input_1  <- as.Date(c("2018-05-21", "2001-01-04", "1940-03-02", NA_character_))
  expected <- input_1

  actual_1 <- replace_with_nas(input_1, return_type = "Date")

  expect_equal(actual_1, expected, label = "The blank values should have been replaced.")
})
