library(testthat)

# ---- verify_value_headstart -------------------------------------------------------
context("verify_value_headstart")

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
})
