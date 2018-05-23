library(testthat)

# ---- update_packages_addin --------------------------------------------------------------
context("update_packages_addin")

test_that("update_packages_addin", {
  testthat::skip("This function is too invasive to run on other people's machines.")
  update_packages_addin()
  testthat::expect_true(TRUE)
})
