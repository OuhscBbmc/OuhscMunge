library(testthat)

test_that("assert_non_na -all - passes", {
  expect_silent(assert_non_na(1:100                                                              ))
  expect_silent(assert_non_na(runif(100)                                                         ))
  expect_silent(assert_non_na(letters                                                            ))
  expect_silent(assert_non_na(sample(c(T,F), size=100, replace = TRUE)                           ))
  expect_silent(assert_non_na(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  ))

  expect_silent(assert_non_na(1:100                                                              , "integer"   ))
  expect_silent(assert_non_na(runif(100)                                                         , "numeric"   ))
  expect_silent(assert_non_na(letters                                                            , "character" ))
  expect_silent(assert_non_na(sample(c(T,F), size=100, replace = TRUE)                           , "logical"   ))
  expect_silent(assert_non_na(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  , "Date"      ))

  expect_silent(assert_non_na(1:100                                                              , "integer"   , 1.0))
  expect_silent(assert_non_na(runif(100)                                                         , "numeric"   , 1.0))
  expect_silent(assert_non_na(letters                                                            , "character" , 1.0))
  expect_silent(assert_non_na(sample(c(T,F), size=100, replace = TRUE)                           , "logical"   , 1.0))
  expect_silent(assert_non_na(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  , "Date"      , 1.0))
})

test_that("assert_non_na -all - fails", {
  error_pattern <- "The vector should not have any NA values, but \\d+ element\\(s\\) were NA\\."
  expect_error(assert_non_na(c(NA, 1:100                                                                       ) , "integer"   ), error_pattern)
  expect_error(assert_non_na(c(NA, runif(100)                                                                  ) , "numeric"   ), error_pattern)
  expect_error(assert_non_na(c(NA, letters                                                                     ) , "character" ), error_pattern)
  expect_error(assert_non_na(c(NA, sample(c(T,F), size=100, replace = TRUE)                                    ) , "logical"   ), error_pattern)
  expect_error(assert_non_na(c(as.Date(NA), seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  ) , "Date"      ), error_pattern)
})


test_that("assert_non_na -proportion - passes", {
  expect_silent(assert_non_na(1:1                                                               , "integer"   , 1.0))
  expect_silent(assert_non_na(runif(100)                                                        , "numeric"   , 1.0))
  expect_silent(assert_non_na(letters                                                           , "character" , 1.0))
  expect_silent(assert_non_na(sample(c(T,F), size=100, replace = TRUE)                          , "logical"   , 1.0))
  expect_silent(assert_non_na(seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days") , "Date"      , 1.0))

  expect_silent(assert_non_na(c(NA, 1:100                                                                        ) , "integer"   , 0.5))
  expect_silent(assert_non_na(c(NA, runif(100)                                                                   ) , "numeric"   , 0.5))
  expect_silent(assert_non_na(c(NA, letters                                                                      ) , "character" , 0.5))
  expect_silent(assert_non_na(c(NA, sample(c(T,F), size=100, replace = TRUE)                                     ) , "logical"   , 0.5))
  expect_silent(assert_non_na(c(as.Date(NA), seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")   ) , "Date"      , 0.5))
})
test_that("assert_non_na -wrong class", {
  error_pattern <- "The vector must inherit from the class `\\w+`, but it is a `\\w+`\\."
  expect_error(assert_non_na(1:100, "character"   , 0.999))
  expect_error(assert_non_na(1:100, "Date"        , 0.999))
  expect_error(assert_non_na(1:100, "double"      , 0.999))
})
test_that("assert_non_na -proportion - fails", {
  error_pattern <- "The vector must have a proportion of at least 0\\.999 of nonmissing elements\\.  However the actual nonmissing proportion is 0\\.\\d+\\."
  expect_error(assert_non_na(c(NA, 1:100                                                                       ) , "integer"   , 0.999))
  expect_error(assert_non_na(c(NA, runif(100)                                                                  ) , "numeric"   , 0.999))
  expect_error(assert_non_na(c(NA, letters                                                                     ) , "character" , 0.999))
  expect_error(assert_non_na(c(NA, sample(c(T,F), size=100, replace = TRUE)                                    ) , "logical"   , 0.999))
  expect_error(assert_non_na(c(as.Date(NA), seq.Date(as.Date("2015-01-02"), as.Date("2017-04-30"), by="days")  ) , "Date"      , 0.999))
})
