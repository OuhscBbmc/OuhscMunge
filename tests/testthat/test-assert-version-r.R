library(testthat)

test_that("old minimum", {
  r <- assert_version_r("3.1.0")
  expect_true(r)

  assert_version_r(package_version("3.1.0"))
  expect_true(r)
})

test_that("default minimum", {
  r <- assert_version_r()
  expect_true(r)
})

test_that("minimum that throws an error", {
  expected_error_message <- "Your R version is too old"
  expect_error(
    assert_version_r("99.1.0"),
    expected_error_message
  )

  expect_error(
    assert_version_r(package_version("99.1.0")),
    expected_error_message
  )
})
