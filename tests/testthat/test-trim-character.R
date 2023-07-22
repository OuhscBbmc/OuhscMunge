library(testthat)

test_that("trim_character -good", {
  x           <- c("12345", "a2345", "54321-6789", "54321-67890", NA_character_)
  pattern     <- "^\\d{5}(-\\d{4})?$"
  replacement <- "-----"

  expected_1  <- c("12345", NA, "54321-6789", NA, NA)
  expected_2  <- c("12345", "-----", "54321-6789", "-----", "-----")

  observed_1  <- OuhscMunge::trim_character(x, pattern)
  observed_2  <- OuhscMunge::trim_character(x, pattern, replacement)

  expect_equal(observed_1, expected_1)
  expect_equal(observed_2, expected_2)

  expect_is(observed_1, "character", "The returned array should remain a double-precision data type.")
  expect_is(observed_2, "character", "The returned array should remain a double-precision data type.")
})

test_that("trim_numeric -degenerate", {
  pattern     <- "^\\w+$"

  expect_equal(OuhscMunge::trim_character(NA_character_, pattern), NA_character_)
  expect_equal(OuhscMunge::trim_character(character(0), pattern), character(0))
})

test_that("trim_numeric -bad values", {
  x           <- c(NA_character_, letters[4])

  expect_error(
    OuhscMunge::trim_character(x, NA_character_),
    "Assertion on 'pattern' failed: Contains missing values"
  )

  expect_error(
    OuhscMunge::trim_numeric(x, 4L),
    "Assertion on 'x' failed: Must be of type 'numeric', not 'character'\\."
  )
})
