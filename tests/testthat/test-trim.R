library(testthat)

# ---- trim_numeric() -------------------------------------------------------
context("Trim Numeric")

test_that("trim_numeric -good", {
  x           <- c(NA, -0.4, -0.3, -0.2, -.16, -.15, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.80001, 0.9, 1)
  bounds      <- c( -.15, .8)
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
  bounds      <- c( -.15, .8)

  expect_equal(OuhscMunge::trim_numeric(NA_real_, bounds), NA_real_)
  expect_equal(OuhscMunge::trim_numeric(numeric(0), bounds), numeric(0))
})

test_that("trim_numeric -bad values", {
  x           <- c(NA, -4:10) / 10
  bounds      <- c( -.15, .8)
  replacement <- 99
  
  expect_error(
    OuhscMunge::trim_numeric(x, NA_real_),
    "Assertion on 'bounds' failed: Must have length >= 2, but has length 1."
  )
  expect_error(
    OuhscMunge::trim_numeric(x, c(6, 2)),
    "The lower element of `bounds` must be equal or less than the upper element of `bounds`."
  )
  
  expect_error(
    OuhscMunge::trim_numeric(x, c(1, 2, 3)),
    "Assertion on 'bounds' failed: Must have length <= 2, but has length 3."
  )
})

# ---- trim_integer() -------------------------------------------------------
context("Trim Integer")

test_that("trim_integer -good", {
  x           <- c(NA, -4:10) 
  bounds      <- c( -1L, 8L)
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
  bounds      <- c( -1L, 8L)

  expect_equal(OuhscMunge::trim_integer(NA_integer_, bounds), NA_integer_)
  expect_equal(OuhscMunge::trim_integer(integer(0), bounds), integer(0))
})

test_that("trim_integer -bad values", {
  x           <- c(NA, -4:10) 
  bounds      <- c( -1L, 8L)
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

# ---- trim_date() -------------------------------------------------------
context("Trim Date")

test_that("trim_date -good", {
  x           <- as.Date(c("1902-02-02", "1989-12-31", "1990-01-01", "1999-09-09", "2020-02-22", "2030-01-01", "2030-01-02"))
  bounds      <- as.Date(c("1990-01-01", "2030-01-01"))
  replacement <- as.Date("1977-07-27")
  
  expected_1  <-as.Date(c(NA_character_,  NA_character_, "1990-01-01", "1999-09-09", "2020-02-22", "2030-01-01", NA_character_))
  expected_2  <-as.Date(c("1977-07-27", "1977-07-27", "1990-01-01", "1999-09-09", "2020-02-22", "2030-01-01", "1977-07-27"))
  
  observed_1  <- OuhscMunge::trim_date(x, bounds)
  observed_2  <- OuhscMunge::trim_date(x, bounds, replacement)
   
  expect_equal(observed_1, expected_1)
  expect_equal(observed_2, expected_2)
  
  expect_equal(class(observed_1), "Date", "The returned array should remain a double-precision data type.")
  expect_equal(class(observed_2), "Date", "The returned array should remain a double-precision data type.")
})

test_that("trim_date -degenerate", {
  bounds      <- as.Date(c("1990-01-01", "2030-01-01"))

  expect_equal(OuhscMunge::trim_date(as.Date(NA_character_), bounds), as.Date(NA_character_))
  expect_equal(OuhscMunge::trim_date(as.Date(character(0)), bounds), as.Date(character(0)))
})

test_that("trim_date -bad values", {
  x           <- as.Date(c("1902-02-02", "1999-09-09", "2020-02-22", "2030-01-01", "2030-01-02"))
  bounds      <- as.Date(c("1990-01-01", "2030-01-01"))
  replacement <- as.Date("1977-07-27")
  
  expect_error(
    OuhscMunge::trim_date(x, as.Date(NA_character_)),
    "Assertion on 'bounds' failed: Must have length >= 2, but has length 1."
  )
  expect_error(
    OuhscMunge::trim_date(x, as.Date(c("2030-01-01", "1990-01-01"))),
    "The lower element of `bounds` must be equal or less than the upper element of `bounds`."
  )
  
  expect_error(
    OuhscMunge::trim_date(x,  as.Date(c("1990-01-01", "2010-01-01", "2030-01-01"))),
    "Assertion on 'bounds' failed: Must have length <= 2, but has length 3."
  )
})
