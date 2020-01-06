library(testthat)

test_that("verify_value_headstart", {
  testthat::expect_output(
    verify_value_headstart(datasets::OrchardSprays)
  )
  testthat::expect_output(
    verify_value_headstart(datasets::iris)
  )
  testthat::expect_output(
    verify_value_headstart(datasets::BOD)
  )
  testthat::expect_output(
    verify_value_headstart(dplyr::band_members)
  )

  storms_2 <- dplyr::storms %>%
    dplyr::mutate(
      storm_date = as.Date(ISOdate(year, month, day))
    )
  testthat::expect_output(
    verify_value_headstart(storms_2)
  )
})
