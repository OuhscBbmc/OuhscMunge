library(testthat)

###########
context("metadata")
###########

test_that("column rename -OrchardSprays", {
  expected <-
', "decrease"   = "decrease"
 , "rowpos"     = "rowpos"
 , "colpos"     = "colpos"
 , "treatment"  = "treatment"'
  
  testthat::expect_output(
    column_rename_headstart(datasets::OrchardSprays)
    , expected
  )
})
