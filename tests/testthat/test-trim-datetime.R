library(testthat)

test_that("trim_datetime -good", {
  x           <- as.POSIXct(c("1902-02-02", "1989-12-31", "1990-01-01", "1999-09-09", "2020-02-22", "2030-01-01", "2030-01-02"))
  bounds      <- as.POSIXct(c("1990-01-01", "2030-01-01"))
  replacement <- as.POSIXct("1977-07-27")

  expected_1  <- as.POSIXct(c(NA_character_,  NA_character_, "1990-01-01", "1999-09-09", "2020-02-22", "2030-01-01", NA_character_))
  expected_2  <- as.POSIXct(c("1977-07-27", "1977-07-27", "1990-01-01", "1999-09-09", "2020-02-22", "2030-01-01", "1977-07-27"))

  observed_1  <- OuhscMunge::trim_datetime(x, bounds)
  observed_2  <- OuhscMunge::trim_datetime(x, bounds, replacement)

  expect_equal(observed_1, expected_1)
  expect_equal(observed_2, expected_2)

  expect_equal(class(observed_1), c("POSIXct", "POSIXt"), "The returned array should remain a double-precision data type.")
  expect_equal(class(observed_2), c("POSIXct", "POSIXt"), "The returned array should remain a double-precision data type.")
})

test_that("trim_datetime -degenerate", {
  bounds      <- as.POSIXct(c("1990-01-01", "2030-01-01"))

  expect_equal(OuhscMunge::trim_datetime(as.POSIXct(NA_character_), bounds), as.POSIXct(NA_character_))
  expect_equal(OuhscMunge::trim_datetime(as.POSIXct(character(0)), bounds), as.POSIXct(character(0)))
})

test_that("trim_datetime -bad values", {
  x           <- as.POSIXct(c("1902-02-02", "1999-09-09", "2020-02-22", "2030-01-01", "2030-01-02"))
  bounds      <- as.POSIXct(c("1990-01-01", "2030-01-01"))
  replacement <- as.POSIXct("1977-07-27")

  expect_error(
    OuhscMunge::trim_datetime(x, as.POSIXct(NA_character_)),
    "Assertion on 'bounds' failed: Must have length >= 2, but has length 1."
  )
  expect_error(
    OuhscMunge::trim_datetime(x, as.POSIXct(c("2030-01-01", "1990-01-01"))),
    "The lower element of `bounds` must be equal or less than the upper element of `bounds`."
  )

  expect_error(
    OuhscMunge::trim_datetime(x,  as.POSIXct(c("1990-01-01", "2010-01-01", "2030-01-01"))),
    "Assertion on 'bounds' failed: Must have length <= 2, but has length 3."
  )
})
