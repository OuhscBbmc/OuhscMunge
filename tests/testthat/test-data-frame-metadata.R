library(testthat)


# ---- metadata-se ----------------------------------------------------------------
test_that("column rename -OrchardSprays", {
  expected <-
'dplyr::select\\(!!c\\(    # `dplyr::select\\(\\)` drops columns not mentioned\\.
  "decrease"               = "decrease",
  "rowpos"                 = "rowpos",
  "colpos"                 = "colpos",
  "treatment"              = "treatment",
\\)\\)'

  testthat::expect_output(
    column_rename_headstart(datasets::OrchardSprays, use_nse = FALSE)
    , expected
  )
})

test_that("column rename -Iris", {
  expected <-
'dplyr::select\\(!!c\\(    # `dplyr::select\\(\\)` drops columns not mentioned\\.
  "sepal_length"              = "Sepal.Length",
  "sepal_width"               = "Sepal.Width",
  "petal_length"              = "Petal.Length",
  "petal_width"               = "Petal.Width",
  "species"                   = "Species",
\\)\\)'

  testthat::expect_output(
    column_rename_headstart(datasets::iris, use_nse = FALSE)
    , expected
  )
  # column_rename_headstart(datasets::iris, use_nse = F); cat(expected)
})


test_that("column rename w/o snake-Iris", {
  expected <-
'dplyr::select\\(!!c\\(    # `dplyr::select\\(\\)` drops columns not mentioned\\.
  "Sepal.Length"              = "Sepal.Length",
  "Sepal.Width"               = "Sepal.Width",
  "Petal.Length"              = "Petal.Length",
  "Petal.Width"               = "Petal.Width",
  "Species"                   = "Species",
\\)\\)'

  testthat::expect_output(
    column_rename_headstart(datasets::iris, try_snake_case=FALSE, use_nse = FALSE)
    , expected
  )
})
test_that("column class -OrchardSprays", {
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
test_that("column class -Iris", {
  expected <-
', "Sepal.Length"  = "numeric"
, "Sepal.Width"   = "numeric"
, "Petal.Length"  = "numeric"
, "Petal.Width"   = "numeric"
, "Species"       = "factor"'

  testthat::expect_output(
    column_class_headstart(datasets::iris)
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


# ---- metadata-nse ----------------------------------------------------------------
test_that("column rename nse -OrchardSprays", {
  expected <-
'dplyr::select\\(    # `dplyr::select\\(\\)` drops columns not included\\.
  decrease                 = decrease,
  rowpos                   = rowpos,
  colpos                   = colpos,
  treatment                = treatment,
\\)'

  testthat::expect_output(
    column_rename_headstart(datasets::OrchardSprays, use_nse = T)
    , expected
  )
 # column_rename_headstart(datasets::OrchardSprays, use_nse = T); cat(expected)
})
test_that("column rename -Iris", {
  expected <-
'dplyr::select\\(    # `dplyr::select\\(\\)` drops columns not included\\.
  sepal_length                = Sepal.Length,
  sepal_width                 = Sepal.Width,
  petal_length                = Petal.Length,
  petal_width                 = Petal.Width,
  species                     = Species,
\\)'

  testthat::expect_output(
    column_rename_headstart(datasets::iris, use_nse = T)
    , expected
  )
  # column_rename_headstart(datasets::iris, use_nse = T); cat(expected)
})


test_that("column rename w/o snake-Iris", {
  expected <-
'dplyr::select\\(    # `dplyr::select\\(\\)` drops columns not included\\.
  Sepal.Length                = Sepal.Length,
  Sepal.Width                 = Sepal.Width,
  Petal.Length                = Petal.Length,
  Petal.Width                 = Petal.Width,
  Species                     = Species,
\\)'

  testthat::expect_output(
    column_rename_headstart(datasets::iris, try_snake_case=FALSE, use_nse = T)
    , expected
  )
  # column_rename_headstart(datasets::iris, try_snake_case=FALSE, use_nse = T); cat(expected)
})
