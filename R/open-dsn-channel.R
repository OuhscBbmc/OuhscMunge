#' @name open_dsn_channel_sqls
#' @export
#' @title Open an ODBC channel to a SQL Server database
#'  
#' @description Creates & opens a channel and checks its important characteristics.
#' 
#' @param dsn_name Name of the locally-defined DSN passed to [RODBC::odbcConnect()](RODBC::odbcConnect()).
#' @param driver_version_minimum The driver must be at least this version number.  Represented as a [base::numeric_version()]
#' @param driver_version_maximum The driver must not exceed this version number.  Represented as a [base::numeric_version()]
#' 
#' @details 
#' A DSN channel requires more code than usual to diagnose problems, because the DSN
#' is defined on the local computer, and is not under the control of the repository.

#' This function wraps the basic [RODBC::odbcConnect()](RODBC::odbcConnect()) function with some
#' checks.  If unsuccessful, it returns some hints how to correct the problem, such as downloading
#' the newest version from the [Microsoft website](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server).
#' 
#' @note 
#' Assuring a minimum version is important, because driver versions can interpret values differently.
#' For example, earlier version (before 11.0)  returned dates as characters, which would
#' propogate undetected through our code until it broke something with an unhelpful error message.
#' 
#' @examples 
#' \dontrun{
#' requireNamespace("OuhscMunge") 
#' 
#' OuhscMunge::open_dsn_channel_sqls(
#'   dsn_name        = "miechv_eval"
#' )
#' }


open_dsn_channel_sqls <- function( 
  dsn_name, 
  driver_version_minimum=numeric_version("13.0"), 
  driver_version_maximum=numeric_version("99.0") 
) {
  
  requireNamespace("RODBC")
  
  checkmate::assert_character(dsn_name, min.chars=1, any.missing=F)
  checkmate::assert_class(driver_version_minimum, "numeric_version")
  checkmate::assert_class(driver_version_maximum, "numeric_version")
  checkmate::assert_character(as.character(driver_version_minimum), min.chars=1, any.missing=F)
  checkmate::assert_character(as.character(driver_version_maximum), min.chars=1, any.missing=F)

  # Uses Trusted/integrated authentication
  channel <- RODBC::odbcConnect(dsn = dsn_name)
  testit::assert("The ODBC channel should open successfully.", channel != -1L)

  info <- RODBC::odbcGetInfo(channel)

  driver_link <- "https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server"
    
  if( driver_version_minimum <= numeric_version(info["Driver_Ver"]) ) {
    m <- "The SQL Server ODBC driver version must be at least %s.  Please download the newest version at %."
    stop(sprintf(m, driver_version_minimum, driver_link))
  } else if ( numeric_version(info["Driver_Ver"]) <= driver_version_minimum) {
    m <- "The SQL Server ODBC driver version must be not exceed %s.  Please download an earlier version at %."
    stop(sprintf(m, driver_version_maximum, driver_link))
  }

  return( channel )
}
