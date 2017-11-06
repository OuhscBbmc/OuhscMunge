library(testthat)

# Verify Data Frame -------------------------------------------------------
context("Verify Data Frame")

test_that("OrchardSprays -regular row count", {
  testthat::expect_silent(
    verify_data_frame(datasets::OrchardSprays)
  )
})

test_that("OrchardSprays -passes small row count threshold", {
  d <- datasets::OrchardSprays[1:5, ]

  testthat::expect_silent(
    verify_data_frame(datasets::OrchardSprays, 4L)
  )
})

test_that("OrchardSprays -fails small row count threshold", {
  d <- datasets::OrchardSprays[1:5, ]
  testthat::expect_error(
    verify_data_frame(d)
  )
})

test_that("OrchardSprays -fails sql error message", {
  pretend_sql_message <- "A database error was thrown.  Check your SQL code."

  expected_error_message <- "The object is not a valid data frame.  If the dataset originated from a database table,check that the SQL code is correct, and a database error was not thrown."
  testthat::expect_error(
    verify_data_frame(pretend_sql_message),
    expected_error_message
  )
})
