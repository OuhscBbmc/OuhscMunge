library(testthat)

d             <- mtcars
table_name    <- "pretend_schema.pretend_table"
dsn_name      <- "pretend-dsn"

test_that("upload_sqls_rodbc -good arguments", {
  expect_warning(
    expect_error(
      upload_sqls_rodbc(d, table_name, dsn_name)
    )
  )
})
