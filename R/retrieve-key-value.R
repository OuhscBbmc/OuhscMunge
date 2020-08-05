#' @name retrieve_key_value
#' @export
#' @title Read a value stored in a database.
#'
#' @description Facilitates retrieval of key-value pairs that are stored in a SQL Server database.
#'
#' @param key The key associated with the desired value.  Required character vector with one element
#' @param project_name The project name associated with the desired value.  Required character vector with one element
#' @param dsn_name Name of the locally-defined DSN passed to [RODBC::odbcConnect()](RODBC::odbcConnect()).
#' @param channel An *optional* connection handle as returned by [RODBC::odbcConnect()].  See Details below. Optional.
#' @return A `character` vector with one element.
#'
#' @details
#' The database table and stored procedure must defined as:
#'
#' ```sql
#' CREATE TABLE security_private.tbl_key_value_static(
#'   id                     smallint     IDENTITY(1,1) PRIMARY KEY,
#'   project                varchar(50)  NOT NULL,
#'   attribute              varchar(90)  NOT NULL,
#'   value                  varchar(200) NOT NULL,
#'   file_last_updated_date date         NOT NULL,
#'   retired                bit          NOT NULL
#' )
#'
#' CREATE PROCEDURE security.prc_key_value_static
#'   @project varchar(50),
#'   @attribute varchar(90)
#' AS
#' BEGIN
#' 	 SET NOCOUNT ON;
#' 	 SELECT value from security_private.tbl_key_value_static
#' 	 WHERE project=@project AND attribute=@attribute
#' END
#' ````
#' @note
#' Currently only the 'static' key-value pairs are retrieved through this function.
#' Talk to Will if you need to retrieve the 'rolling' or the 'personal' key-value pairs.
#'
#' @author Will Beasley
#'
#' @examples
#' \dontrun{
#' value <- retrieve_key_value("file-server", "bbmc", "BbmcSecurity")
#' }

retrieve_key_value <- function(
  key,
  project_name,
  dsn_name,
  channel       = NULL
) {
  pattern <- "^[-a-zA-Z0-9_]+$"
  checkmate::assert_character(key         , min.chars=1, pattern=pattern, any.missing=FALSE, len=1)
  checkmate::assert_character(project_name, min.chars=1, pattern=pattern, any.missing=FALSE, len=1)
  checkmate::assert_character(dsn_name    , min.chars=1, pattern=pattern, any.missing=FALSE, len=1)

  if (!requireNamespace("odbc", quietly = TRUE))
    stop("The function `retrieve_key_value()` cannot run if the `odbc` package is not installed.  Please install it and try again.")
  # if (!requireNamespace("RODBC", quietly = TRUE))
  #   stop("The function `retrieve_key_value()` cannot run if the `RODBC` package is not installed.  Please install it and try again.")
  # if (!requireNamespace("RODBCext", quietly = TRUE))
  #   stop("The function `retrieve_key_value()` cannot run if the `RODBCext` package is not installed.  Please install it and try again.")

  sql <- "EXEC security.prc_key_value_static @project=?, @attribute = ?"

  # d_input <- data.frame(
  #   project_name       = project_name,
  #   key                = key,
  #   stringsAsFactors   = FALSE
  # )

  if (base::missing(channel) | base::is.null(channel)) {
    if (base::missing(dsn_name) | base::is.null(dsn_name))
      stop("The 'dsn_name' parameter can be missing only if a 'channel' has been passed to `retrieve_key_value()`.")

    channel <- open_dsn_channel_sqls_odbc(dsn_name)
    close_channel_on_exit <- TRUE
  } else {
    close_channel_on_exit <- FALSE
  }

  # browser()
  base::tryCatch(
    expr = {
      query     <- DBI::dbSendQuery(channel, sql)
      bind      <- DBI::dbBind(query, list(project_name, key))
      ds_value  <- DBI::dbFetch(query)

    }, finally = {
      if (exists("query"))       DBI::dbClearResult(query)
      # if (exists("bind"))        DBI::dbClearResult(bind)
      if (close_channel_on_exit) DBI::dbDisconnect(channel)
    }
  )
  # base::tryCatch(
  #   expr = {
  #     ds_value <- RODBCext::sqlExecute(channel, sql, d_input, fetch = TRUE, stringsAsFactors = FALSE)
  #   }, finally = {
  #     if (close_channel_on_exit) RODBC::odbcClose(channel)
  #   }
  # )

  if (nrow(ds_value) == 0L) {
    stop("No row was found with the desired [key]-by-[project_name] combination.")
  } else if (nrow(ds_value) >= 2L) {
    stop("No more than one row should be retrieved.  The [key]-by-[project_name] should be unique in the table.")
  }

  ds_value$value[[1]]
}
# a <- retrieve_key_value("file-server", "bbmc", "BbmcSecurity")
