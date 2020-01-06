library(testthat)

test_that("assert_non_na_and_unique -all - passes", {
  expect_silent(assert_non_na_and_unique(1:100                                                              , "integer"   ))
  expect_silent(assert_non_na_and_unique(runif(100)                                                         , "numeric"   ))
  expect_silent(assert_non_na_and_unique(letters                                                            , "character" ))
  expect_silent(assert_non_na_and_unique(sample(c(T,F), size=2, replace = FALSE)                            , "logical"   ))
  expect_silent(assert_non_na_and_unique(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  , "Date"      ))
})

test_that("assert_non_na_and_unique -all - fails", {
  error_pattern <- "The vector elements should be unique, but \\d+ element\\(s\\) were duplicated\\."
  expect_error(assert_non_na_and_unique(c(1L, 1:100                                                                                ) , "integer"   ), error_pattern)
  expect_error(assert_non_na_and_unique(c(.2, .2, runif(100)                                                                       ) , "numeric"   ), error_pattern)
  expect_error(assert_non_na_and_unique(c("a", letters                                                                             ) , "character" ), error_pattern)
  expect_error(assert_non_na_and_unique(c(T, T, F                                                                                  ) , "logical"   ), error_pattern)
  expect_error(assert_non_na_and_unique(c(as.Date("2017-04-30"), seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days") ) , "Date"      ), error_pattern)
})
