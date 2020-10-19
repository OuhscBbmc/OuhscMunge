library(testthat)

name <- "pretend-dsn"
version_min <- numeric_version("0.1.0")
version_max <- numeric_version("5.0.0")

test_that("open_dsn_channel -good arguments", {
  expect_error(
    open_dsn_channel_sqls_odbc(name, version_min, version_max),
    "does not exist on your local machine."
  )
})
test_that("open_dsn_channel -blank dsn", {
  expect_error(
    open_dsn_channel_sqls_odbc("", version_min, version_max),
    "Assertion on 'dsn_name' failed: All elements must have at least 1 characters."
  )
})

test_that("open_dsn_channel -string min", {
  expect_error(
    open_dsn_channel_sqls_odbc(name, "0.1.0", version_max), # This isn't a `numeric_version`.
    "Assertion on 'driver_version_minimum' failed: Must (inherit from|have) class 'numeric_version', but has class 'character'."
  )
})
test_that("open_dsn_channel -string max", {
  expect_error(
    open_dsn_channel_sqls_odbc(name, version_min, "0.1.0"), # This isn't a `numeric_version`.
    "Assertion on 'driver_version_maximum' failed: Must (inherit from|have) class 'numeric_version', but has class 'character'."
  )
})
