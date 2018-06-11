#' @name upload_sqls_odbc
#' @export
#' @title Upload to a SQL Server database using odbc
#'
#' @description The function performs some extra configuration to improve robustness.
#'
#' @param d Dataset to be uploaded to the dataset.  The object must inherit from `data.frame`.
#' @param schema_name Name of the database destination table.
#' @param table_name Name of the database destination table.
#' @param dsn_name Name of the locally-defined DSN passed to [DBI::dbConnect()].
#' @param clear_table If `TRUE`, calls deletes or truncates all records before writing to the table.
#' @param create_table If the table structure has not yet been defined in the database, it will be created if `create_table` is `TRUE`.
#' @param convert_logical_to_integer Convert all `logical` columns to `integer`.  This helps the database store the values as bits.
#' @param transaction Should the clear and upload steps be wrapped in a rollback transaction?
#' @param verbose Write a message about the status of a successful upload.
#'
#' @details
#' If `transaction` is `TRUE` and the upload fails, the table is rolled back to the state before function was callled.
#' This includes rolling back the (optional) clearing of records, and uploading the new records.
#' Decide if it's more robust to rollback to the previous state, or if it's better to leave the table in the incomplete state.
#' The latter is helpful diagnosing which record caused the write to fail; look at the last successful record contained in the database
#'
#' @examples
#' \dontrun{
#' requireNamespace("OuhscMunge")
#'
#' OuhscMunge::upload_sqls_odbc(
#'   d                          = ds_client,          # Some data.frame that exists in RAM
#'   schema_name                = "dbo",
#'   table_name                 = "tbl_client",
#'   dsn_name                   = "miechv_eval",
#'   create_table               = FALSE,
#'   clear_table                = TRUE,
#'   transaction                = TRUE,
#'   verbose                    = TRUE,
#'   convert_logical_to_integer = TRUE
#' )
#' }


upload_sqls_odbc <- function(
  d,
  schema_name,
  table_name,
  dsn_name,
  clear_table                   = FALSE,
  create_table                  = FALSE,
  convert_logical_to_integer    = FALSE,
  transaction                   = FALSE,
  verbose                       = TRUE
) {

  checkmate::assert_class(    d                             , "data.frame"          , null.ok    =F)
  checkmate::assert_character(schema_name                   , min.chars=1L  , len=1L, any.missing=F)
  checkmate::assert_character(table_name                    , min.chars=1L  , len=1L, any.missing=F)
  checkmate::assert_character(dsn_name                      , min.chars=1L  , len=1L, any.missing=F)

  checkmate::assert_logical(  clear_table                                   , len=1L, any.missing=F)
  checkmate::assert_logical(  create_table                                  , len=1L, any.missing=F)
  checkmate::assert_logical(  convert_logical_to_integer                    , len=1L, any.missing=F)
  checkmate::assert_logical(  transaction                                   , len=1L, any.missing=F)
  checkmate::assert_logical(  verbose                                       , len=1L, any.missing=F)

  start_time <- base::Sys.time()
  print(start_time)

  if( convert_logical_to_integer ) {
    d <- dplyr::mutate_if(d, is.logical, as.integer)
  }

  requireNamespace("DBI")
  requireNamespace("odbc")

  channel <- DBI::dbConnect(
    drv   = odbc::odbc(),
    dsn   = dsn_name
  )
  table_id <- DBI::Id(
    schema  = schema_name,
    name    = table_name
  )

  tryCatch( {
    if( transaction ) {
      DBI::dbBegin(channel)
    }

    if( verbose ) {
      DBI::dbGetInfo(channel)
    }

    # message("overwrite: ", !create_table & clear_table)

    result <- DBI::dbWriteTable(
      conn        = channel,
      name        = table_id,
      value       = d,
      overwrite   = !create_table & clear_table,
      append      = FALSE
    )

    if( transaction ) {
      DBI::dbCommit(channel)
    }

    if( verbose ) {
      duration <- round(as.numeric(difftime(Sys.time(), start_time, units="mins")), 3)
      message("The table `", table_name, "` was written over dsn `", dsn_name, "` in ", duration, " minutes.")
    }
  }, error = function( e ) {

    if( transaction ) {
      DBI::dbRollback(channel)
    }
    stop("Writing to the database was not successful.  Attempted to write table `", table_name, "` over dsn `", dsn_name, "`.\n", e)

  }, finally = {
    DBI::dbDisconnect(channel)
  })
}
