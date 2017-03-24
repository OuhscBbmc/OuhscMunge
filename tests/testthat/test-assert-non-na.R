library(testthat)

context("assert_non_na")

test_that("assert_non_na -all - passes", {
  expect_silent( assert_non_na(1:100                                                              , "integer"   ))
  expect_silent( assert_non_na(runif(100)                                                         , "numeric"   ))
  expect_silent( assert_non_na(letters                                                            , "character" ))
  expect_silent( assert_non_na(sample(c(T,F), size=100, replace = TRUE)                           , "logical"   ))
  expect_silent( assert_non_na(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  , "Date"      ))
  
  expect_silent( assert_non_na(1:100                                                              , "integer"   , 1.0))
  expect_silent( assert_non_na(runif(100)                                                         , "numeric"   , 1.0))
  expect_silent( assert_non_na(letters                                                            , "character" , 1.0))
  expect_silent( assert_non_na(sample(c(T,F), size=100, replace = TRUE)                           , "logical"   , 1.0))
  expect_silent( assert_non_na(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  , "Date"      , 1.0))
})

test_that("assert_non_na -all - fails", {
  expect_error( assert_non_na(c(NA, 1:100                                                                       ) , "integer"   ))
  expect_error( assert_non_na(c(NA, runif(100)                                                                  ) , "numeric"   ))
  expect_error( assert_non_na(c(NA, letters                                                                     ) , "character" ))
  expect_error( assert_non_na(c(NA, sample(c(T,F), size=100, replace = TRUE)                                    ) , "logical"   ))
  expect_error( assert_non_na(c(as.Date(NA), seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  ) , "Date"      ))
})


test_that("assert_non_na -proportion - passes", {
  expect_silent( assert_non_na(1:1                                                               , "integer"   , 1.0))
  expect_silent( assert_non_na(runif(100)                                                        , "numeric"   , 1.0))
  expect_silent( assert_non_na(letters                                                           , "character" , 1.0))
  expect_silent( assert_non_na(sample(c(T,F), size=100, replace = TRUE)                          , "logical"   , 1.0))
  expect_silent( assert_non_na(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days") , "Date"      , 1.0))
  
  expect_silent( assert_non_na(c(NA, 1:100                                                                        ) , "integer"   , 0.5))
  expect_silent( assert_non_na(c(NA, runif(100)                                                                   ) , "numeric"   , 0.5))
  expect_silent( assert_non_na(c(NA, letters                                                                      ) , "character" , 0.5))
  expect_silent( assert_non_na(c(NA, sample(c(T,F), size=100, replace = TRUE)                                     ) , "logical"   , 0.5))
  expect_silent( assert_non_na(c(as.Date(NA), seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")   ) , "Date"      , 0.5))
})

test_that("assert_non_na -proportion - fails", {
  expect_error( assert_non_na(c(NA, 1:100                                                                       ) , "integer"   , 0.999))
  expect_error( assert_non_na(c(NA, runif(100)                                                                  ) , "numeric"   , 0.999))
  expect_error( assert_non_na(c(NA, letters                                                                     ) , "character" , 0.999))
  expect_error( assert_non_na(c(NA, sample(c(T,F), size=100, replace = TRUE)                                    ) , "logical"   , 0.999))
  expect_error( assert_non_na(c(as.Date(NA), seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  ) , "Date"      , 0.999))
})
