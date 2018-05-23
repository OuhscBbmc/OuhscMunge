library(testthat)

# ---- install_packages_addin --------------------------------------------------------------
context("install_packages_addin")

test_that("install_packages_addin", {
  testthat::skip("This function is too invasive to run on other people's machines.")
  install_packages_addin()
  testthat::expect_true(TRUE)
})
