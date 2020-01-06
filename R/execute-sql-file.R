#' @name execute_sql_file
#' @export
#'
#' @title Execute a SQL file
#'
#' @description Read a SQL file, and execute its text using the `odbc` and `DBI` packages.
#'
#' @param path_sql A vector to of names to convert.  Required character.
#' @param dsn The name of a [DSN](https://en.wikipedia.org/wiki/Data_source_name) defined on your local machine  Required character.
#' @param execute Indicates if `DBI::dbExecute()` should be used
#' (which typically returns a scalar).  Otherwise, `DBI::dbGetQuery()` is used,
#' (which will return a `tibble::tibble`).  Required `logical`.
#' @param minimum_row_count If `execute` is false, the returned dataset should have at least this many rows, or an error will be thrown.  Default of 0.  Required integer.
#' @param timezone The server time zone.  Passed to [DBI::dbConnect()].
#' @param timezone_out The time zone returned to R. Passed to [DBI::dbConnect()].  See https://www.tidyverse.org/blog/2019/12/odbc-1-2-0/.
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

execute_sql_file <- function(
  path_sql,
  dsn,
  execute                       = TRUE,
  minimum_row_count             = 0L,
  timezone                      = "UTC",
  timezone_out                  = "UTC"
) {
  checkmate::expect_file_exists(path_sql)
  checkmate::assert_character(path_sql, min.chars=2, min.len=1, max.len=1, any.missing=FALSE)
  checkmate::assert_character(dsn     , min.chars=2, min.len=1, max.len=1, any.missing=FALSE)
  checkmate::assert_logical(  execute              , len=1, any.missing=FALSE)
  checkmate::assert_character(timezone                                      , len=1L, any.missing=FALSE)
  checkmate::assert_character(timezone_out                                  , len=1L, any.missing=FALSE)

  sql   <- readr::read_file(path_sql)
  checkmate::assert_character(sql     , min.chars=2, min.len=1, max.len=1, any.missing=FALSE)

  tryCatch({
    channel <- odbc::dbConnect(
      odbc::odbc(),
      dsn,
      timezone      = timezone,
      timezone_out  = timezone_out
    )

    if (execute) {
      returned_value <- DBI::dbExecute(channel, sql)
    } else {
      returned_value <- DBI::dbGetQuery(channel, sql) %>%
        tibble::as_tibble()

      checkmate::assert_tibble(returned_value, min.rows = minimum_row_count)
    }
  }, finally = {
    odbc::dbDisconnect(channel)
  })

  returned_value
}
