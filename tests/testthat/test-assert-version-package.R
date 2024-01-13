library(testthat)

test_that("old minimum", {
  r <- assert_version_package("base", "3.1.0")
  expect_true(r)

  r <- assert_version_package("OuhscMunge", "0.1.0")
  expect_true(r)

  assert_version_package("base", package_version("3.1.0"))
  expect_true(r)

  assert_version_package("OuhscMunge", package_version("0.1.0"))
  expect_true(r)
})

test_that("minimum that throws an error", {
  expected_error_message <- "Your version of the `base` package is too old"
  expect_error(
    assert_version_package("base", "99.1.0"),
    expected_error_message
  )

  expect_error(
    assert_version_package("base", package_version("99.1.0")),
    expected_error_message
  )
})
stop("TODO: test installation message for non-existent package.")
stop("TODO: test installation message for an old package.")
