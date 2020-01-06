library(testthat)

# ---- upload_sqls_odbc ---------------------------------------------------
d             <- mtcars
schema_name   <- "pretend_schema"
table_name    <- "pretend_table"
dsn_name      <- "pretend-dsn"

test_that("upload_sqls_odbc -good arguments", {
  expect_error(
    upload_sqls_odbc(d, schema_name, table_name, dsn_name),
    "Data source name not found,? and no default driver specified"
  )
})
