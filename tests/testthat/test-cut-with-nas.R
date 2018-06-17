library(testthat)

# ---- cut_with_nas --------------------------------------------------------------
context("cut_with_nasf")

test_that("default -OrchardSprays", {
  w      <- c(0L, NA_integer_, 22:25, NA_integer_, 40)
  breaks <- c(0, 25, 50)
  labels <- c("lower", "upper")

  expected_1  <- structure(c(1L, 3L, 2L, 2L, 2L, 2L, 3L, 2L), .Label = c("(-0.04,20]", "(20,40]", "Unknown"), class = "factor")
  expected_2  <- structure(c(3L, 3L, 1L, 1L, 1L, 1L, 3L, 2L), .Label = c("lower", "upper", "Unknown"), class = "factor")
  expected_3  <- structure(c(3L, 3L, 1L, 1L, 1L, 1L, 3L, 2L), .Label = c("(0,25]", "(25,50]", "Unknown"), class = "factor")
  expected_4  <- structure(c(1L, 3L, 1L, 1L, 1L, 1L, 3L, 2L), .Label = c("[0,25]", "(25,50]", "Unknown"), class = "factor")
  expected_5  <- structure(c(1L, 3L, 1L, 1L, 1L, 2L, 3L, 2L), .Label = c("[0,25)", "[25,50]", "Unknown"), class = "factor")
  expected_6  <- structure(c(1L, 3L, 1L, 1L, 1L, 2L, 3L, 2L), .Label = c("[0,25)", "[25,50)", "Unknown"), class = "factor")

  actual_1  <- cut_with_nas(w, breaks=2)
  actual_2  <- cut_with_nas(w, breaks=breaks, labels=labels)
  actual_3  <- cut_with_nas(w, breaks=breaks               )
  actual_4  <- cut_with_nas(w, breaks=breaks               , include.lowest=T)
  actual_5  <- cut_with_nas(w, breaks=breaks               , include.lowest=T, right=F)
  actual_6  <- cut_with_nas(w, breaks=breaks                                 , right=F)

  testthat::expect_equal(actual_1, expected_1)
  testthat::expect_equal(actual_2, expected_2)
  testthat::expect_equal(actual_3, expected_3)
  testthat::expect_equal(actual_4, expected_4)
  testthat::expect_equal(actual_5, expected_5)
  testthat::expect_equal(actual_6, expected_6)

})

