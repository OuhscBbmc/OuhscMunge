library(testthat)

# ---- retrieve_key_value ------------------------------------------------------
key       <- "k"
project   <- "project"
name      <- "pretend-dsn"

test_that("open_dsn_channel -good arguments", {
  expect_error(
    retrieve_key_value(key, project, name),
    "does not exist on your local machine."
  )
})
