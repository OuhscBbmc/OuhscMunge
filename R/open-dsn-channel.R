#' @name open_dsn_channel_sqls
#' @aliases open_dsn_channel_sqls open_dsn_channel_sqls_odbc
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


#' @export
open_dsn_channel_sqls <- function(
  dsn_name,
  driver_version_minimum = numeric_version("13.0"),
  driver_version_maximum = numeric_version("99.0")
) {

  requireNamespace("RODBC")

  checkmate::assert_character(dsn_name, min.chars=1, min.len=1, max.len=1, any.missing=FALSE)
  checkmate::assert_class(driver_version_minimum, "numeric_version")
  checkmate::assert_class(driver_version_maximum, "numeric_version")
  checkmate::assert_character(as.character(driver_version_minimum), min.chars=1, min.len=1, max.len=1, any.missing=FALSE)
  checkmate::assert_character(as.character(driver_version_maximum), min.chars=1, min.len=1, max.len=1, any.missing=FALSE)
  # Check if the DSN even exists on the local machine.

  create_link <- "https://github.com/OuhscBbmc/BbmcResources/blob/master/instructions/odbc-dsn.md"
  driver_link <- "https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server"

  dsn_exists <- (dsn_name %in% names(RODBC::odbcDataSources()))
  if (!dsn_exists) {
    m <- "The DSN `%s` does not exist on your local machine.  Please see the installation instructions at %s."
    stop(sprintf(m, dsn_name, create_link))
  }

  # Uses Trusted/integrated authentication
  channel <- RODBC::odbcConnect(dsn = dsn_name)
  if (channel == -1L) {
    RODBC::odbcClose(channel)
    m <- "The ODBC channel should open successfully.  Please see the installation instructions at %s."
    stop(sprintf(m, dsn_name, create_link))
  }

  info <- RODBC::odbcGetInfo(channel)

  # print(numeric_version(info["Driver_Ver"]))

  if (!(driver_version_minimum <= numeric_version(info["Driver_Ver"]))) {
    RODBC::odbcClose(channel)
    m <- "The SQL Server ODBC driver version must be at least %s.  Please download the newest version at %s.  Please see the installation instructions at %s.  The DSN name is `%s`."
    stop(sprintf(m, as.character(driver_version_minimum), driver_link, create_link, dsn_name))
  } else if (!(numeric_version(info["Driver_Ver"]) <= driver_version_maximum)) {
    RODBC::odbcClose(channel)
    m <- "The SQL Server ODBC driver version must be not exceed %s.  Please download an earlier version at %s.  Please see the installation instructions at %s.  The DSN name is `%s`."
    stop(sprintf(m, as.character(driver_version_maximum), driver_link, create_link, dsn_name))
  }

  channel
}

#' @export
open_dsn_channel_sqls_odbc <- function(
  dsn_name,
  driver_version_minimum = numeric_version("13.0"),
  driver_version_maximum = numeric_version("99.0")
) {

  requireNamespace("odbc")

  checkmate::assert_character(dsn_name, min.chars=1, min.len=1, max.len=1, any.missing=FALSE)
  checkmate::assert_class(driver_version_minimum, "numeric_version")
  checkmate::assert_class(driver_version_maximum, "numeric_version")
  checkmate::assert_character(as.character(driver_version_minimum), min.chars=1, min.len=1, max.len=1, any.missing=FALSE)
  checkmate::assert_character(as.character(driver_version_maximum), min.chars=1, min.len=1, max.len=1, any.missing=FALSE)
  # Check if the DSN even exists on the local machine.

  create_link <- "https://github.com/OuhscBbmc/BbmcResources/blob/master/instructions/odbc-dsn.md"
  driver_link <- "https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server"

  if (requireNamespace("RODBC", quietly = TRUE)) {
    dsn_exists <- (dsn_name %in% names(RODBC::odbcDataSources()))
    if (!dsn_exists) {
      m <- "The DSN `%s` does not exist on your local machine.  Please see the installation instructions at %s."
      stop(sprintf(m, dsn_name, create_link))
    }
  }

  # Uses Trusted/integrated authentication
  tryCatch({
    # channel <- DBI::dbConnect(odbc::odbc(), dsn = "MiechvEvaluation")
    channel <- DBI::dbConnect(odbc::odbc(), dsn = dsn_name)
  } , error = function(e) {
    m <- "The ODBC channel should open successfully.  Please see the installation instructions at %s."
    stop(sprintf(m, dsn_name, create_link))
  })

  info <- DBI::dbGetInfo(channel)

  if (!(driver_version_minimum <= numeric_version(info$driver.version))) {
    DBI::dbDisconnect(channel)
    m <- "The SQL Server ODBC driver version must be at least %s.  Please download the newest version at %s.  Please see the installation instructions at %s.  The DSN name is `%s`."
    stop(sprintf(m, as.character(driver_version_minimum), driver_link, create_link, dsn_name))
  } else if (!(numeric_version(info$driver.version) <= driver_version_maximum)) {
    DBI::dbDisconnect(channel)
    m <- "The SQL Server ODBC driver version must be not exceed %s.  Please download an earlier version at %s.  Please see the installation instructions at %s.  The DSN name is `%s`."
    stop(sprintf(m, as.character(driver_version_maximum), driver_link, create_link, dsn_name))
  }

  channel
}
