library(testthat)

# ---- execute_sql_file --------------------------------------------------------
context("execute_sql_file")

path  <- base::file.path(devtools::inst(name="OuhscMunge"), "hdid-select.sql")
dsn   <- "cdw_cache_staging"

test_that("sql file -bad path", {
  expect_failure(
    execute_sql_file("bad-file.txt", dsn)
  )
})

test_that("sql file -bad dsn", {
  expect_failure(
    execute_sql_file(path, "")
  )
})

test_that("sql file -empty", {
  expect_error(
    execute_sql_file(base::file.path(devtools::inst(name="OuhscMunge"), "test-data/empty.sql"), dsn)
  )
})
