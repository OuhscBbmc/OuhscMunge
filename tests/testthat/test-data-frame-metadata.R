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

test_that("column rename -OrchardSprays", {
  expected <-
', "decrease"   = "numeric"
 , "rowpos"     = "numeric"
 , "colpos"     = "numeric"
 , "treatment"  = "factor"'
  
  testthat::expect_output(
    column_class_headstart(datasets::OrchardSprays)
    , expected
  )
})
test_that("column value -OrchardSprays", {
  expected <-
', "A"  = "A"
 , "B"  = "B"
 , "C"  = "C"
 , "D"  = "D"
 , "E"  = "E"
 , "F"  = "F"
 , "G"  = "G"
 , "H"  = "H"'
  
  testthat::expect_output(
    column_value_headstart(datasets::OrchardSprays$treatment)
    , expected
  )
})
