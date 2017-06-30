#' @name open_dsn_channel_sqls
#' @export
#' @title Open an ODBC channel to a SQL Server database
#'  
#' @description Creates & opens a channel and checks its important characteristics.
#' 
#' @param dsn_name Name of the locally-defined DSN passed to [RODBC::odbcConnect()](RODBC::odbcConnect()).
#' @param driver_version_minimum Represnted as a [base::numeric_version()]
#' 
#' @details 
#' A DSN channel requires more code than usual to diagnose problems, because the DSN
#' is defined on the local computer, and is not under the control of the repository.

#' This function wraps the basic [RODBC::odbcConnect()](RODBC::odbcConnect()) function with some
#' checks.  If unsuccessful, it returns some hints how to correct the problem, such as downloading
#' the newest version from the [Microsoft website](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server).
#' 
#' @examples 
#' \dontrun{
#' requireNamespace("OuhscMunge") 
#' 
#' OuhscMunge::open_dsn_channel_sqls(
#'   dsn_name        = "miechv_eval"
#' )
#' }


open_dsn_channel_sqls <- function( dsn_name, driver_version_minimum=numeric_version("13.0") ) {
  requireNamespace("RODBC")
  
  checkmate::assert_character(dsn_name, min.chars=1, any.missing=F)

  # Uses Trusted/integrated authentication
  channel <- RODBC::odbcConnect(dsn = dsn_name)
  testit::assert("The ODBC channel should open successfully.", channel != -1L)

  info <- RODBC::odbcGetInfo(channel)

  m           <- "The SQL Server ODBC driver version must be at least %s.  Please download the newest version at %."
  driver_link <- "https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server"
    
  testit::assert(
    sprintf(m, driver_version_minimum, driver_link),
    numeric_version(info["Driver_Ver"]) >= driver_version_minimum
  )

  return( channel )
}
