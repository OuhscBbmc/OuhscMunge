library(testthat)


test_that("readr_spec_aligned -subject-1", {
  expected <-
'col_types <- readr::cols_only\\(
  `subject id`    = readr::col_double\\(\\),
  `county_id`     = readr::col_double\\(\\),
  `gender_id`     = readr::col_double\\(\\),
  `race`          = readr::col_character\\(\\),
  `ethnicity`     = readr::col_character\\(\\),
  `dob`           = readr::col_date\\(format = ""\\),
  `dod`           = readr::col_character\\(\\)
\\)'

  testthat::expect_output(
    readr_spec_aligned(system.file("test-data/subject-1.csv", package = "OuhscMunge"))
    , expected
  )
})
test_that("readr_spec_aligned -package-dependency-list.csv", {
  expected <-
'col_types <- readr::cols_only\\(
  `package_name`      = readr::col_character\\(\\),
  `on_cran`           = readr::col_logical\\(\\),
  `install`           = readr::col_logical\\(\\),
  `github_username`   = readr::col_character\\(\\),
  `description`       = readr::col_character\\(\\)
\\)'

  testthat::expect_output(
    readr_spec_aligned(system.file("package-dependency-list.csv", package = "OuhscMunge"))
    , expected
  )
})
