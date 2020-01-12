library(testthat)

path  <- system.file("test-data/hdid-select.sql", package = "OuhscMunge") # See ?pkgload::system.file
dsn   <- "cdw_cache_staging"

test_that("sql file -bad path", {
  expect_failure(
    execute_sql_file("bad-file.txt", dsn)
  )
})

test_that("sql file -bad dsn", {
  expect_error(
    execute_sql_file(path, "")
  )
})

test_that("sql file -empty", {
  expect_error(
    execute_sql_file(
      system.file("test-data/empty.sql", package = "OuhscMunge"),  # See ?pkgload::system.file
      dsn
    )
  )
})
