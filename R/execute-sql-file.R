#' @name execute_sql_file
#' @export
#'
#' @title Execute a SQL file
#'
#' @description Read a SQL file, and execute its text using the `odbc` and `DBI` packages.
#'
#' @param path_sql A vector to of names to convert.  Required character.
#' @param dsn The name of a [DSN](https://en.wikipedia.org/wiki/Data_source_name) defined on your local machine  Required character.
#'
#' @return A vector of converted names.
#'
#' @author Will Beasley
#'
#' @examples
#' \dontrun{
#' execute_sql_file("inst/hdid-select.sql", "cdw_cache_staging")
#' execute_sql_file("inst/condense-date.sql", "cdw_cache_staging")
#' }

execute_sql_file <- function( path_sql, dsn ) {
  checkmate::expect_file_exists(path_sql)
  checkmate::assert_character(path_sql, min.chars=2, min.len=1, max.len=1, any.missing=F)
  checkmate::assert_character(dsn     , min.chars=2, min.len=1, max.len=1, any.missing=F)

  sql   <- readr::read_file(path_sql)
  checkmate::assert_character(sql     , min.chars=2, min.len=1, max.len=1, any.missing=F)

  tryCatch({
    channel <- odbc::dbConnect(odbc::odbc(), dsn)
    returned_value <- DBI::dbExecute(channel, sql)
  }, finally={
    odbc::dbDisconnect(channel)
  })

  return( returned_value )
}
