library(testthat)

###########
context("Snake Case")
###########

test_that("snake -OrchardSprays", {
  expected <- c("decrease", "rowpos", "colpos", "treatment")
  actual   <- snake_case(colnames(datasets::OrchardSprays))

  testthat::expect_equal(actual, expected)
})

test_that("snake -Iris", {
  expected <- c("sepal_length", "sepal_width", "petal_length", "petal_width",  "species")
  actual   <- snake_case(colnames(datasets::iris))

  testthat::expect_equal(actual, expected)
})
