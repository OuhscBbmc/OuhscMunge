library(testthat)

###########
context("Deterge")
###########

test_that("deterge double with round numbers", {
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

test_that("deterge integers", {
  expected <- c(NA, NA, NA, NA, 4, 5, 6, 7, 8, NA, NA)
  observed <- OuhscMunge::deterge_to_integer(c(NA, 1:10), 4, 8)
  
  expect_equal(observed, expected)
  expect_equal(class(observed), "integer", "The returned array should remain a double-precision data type.")
})
