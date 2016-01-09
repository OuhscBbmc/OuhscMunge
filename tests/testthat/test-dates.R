library(testthat)

###########
context("Dates")
###########

test_that("clump date -typical", {
  detailed <- seq.Date(from=as.Date("2011-04-21"), to=as.Date("2011-05-14"), by="day")
  expected <- structure(c(15079, 15079, 15079, 15079, 15079, 15079, 15079, 
                          15079, 15079, 15079, 15109, 15109, 15109, 15109, 15109, 15109, 
                          15109, 15109, 15109, 15109, 15109, 15109, 15109, 15109), class = "Date")
  observed <- OuhscMunge::clump_month_date(detailed)#; dput(clumped)
  
  expect_equal(observed, expected, label="The dates should be clumped corrected.")
  expect_equal(class(observed), "Date", "The returned array should remain a date data type.")
})

test_that("clump date -not a date", {
  detailed <- letters
  expect_error(
    OuhscMunge::clump_month_date(detailed)
    , regexp = "The `date_detailed` parameter must be a Date, POSIXct, or POSIXlt data type\\."
  )
})

test_that("clump date -bad day character", {
  detailed <- seq.Date(from=as.Date("2011-04-21"), to=as.Date("2011-05-14"), by="day")
  expect_error(
    OuhscMunge::clump_month_date(detailed, day_of_month="a")
    , regexp = "The `day_of_month` parameter must be an integer or numeric data type\\."
  )
})

test_that("clump date -day not a scalar", {
  detailed <- seq.Date(from=as.Date("2011-04-21"), to=as.Date("2011-05-14"), by="day")
  expect_error(
    OuhscMunge::clump_month_date(detailed, day_of_month=1:4)
    , regexp = "The `day_of_month` contains more than one element; it should contain only one\\."
  )
})

test_that("clump date -bad day 32", {
  detailed <- seq.Date(from=as.Date("2011-04-21"), to=as.Date("2011-05-14"), by="day")
  expect_error(
    OuhscMunge::clump_month_date(detailed, day_of_month=32)
    , regexp = "Error in OuhscMunge::clump_month_date\\(detailed, day_of_month = 32\\) :\\s+The `day_of_month` parameter must be bound by \\[1, 31]\\."
  )
})

test_that("clump date -bad day 31.5", {
  detailed <- seq.Date(from=as.Date("2011-04-21"), to=as.Date("2011-05-14"), by="day")
  expect_error(
    OuhscMunge::clump_month_date(detailed, day_of_month=31.5)
    , regexp = "Error in OuhscMunge::clump_month_date\\(detailed, day_of_month = 31.5\\) :\\s+The `day_of_month` parameter must be bound by \\[1, 31]\\."
  )
})
