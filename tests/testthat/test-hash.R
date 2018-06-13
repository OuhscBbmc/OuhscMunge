library(testthat)

context("hash_and_salt_sha_256")

test_that("hash", {
  input <- letters
  salt  <- "abc123"
  expected <- rep("05c4c3e956c9d246f23f554e84d1c700eba51f9f769d46b0b15b3ce62cda8fcc", 26) #This is wrong
  actual   <- hash_and_salt_sha_256(input, salt)

  testthat::expect_equal(actual, expected)
})
