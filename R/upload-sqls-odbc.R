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
#' @param timezone The server time zone.  Passed to [DBI::dbConnect()].
#' @param timezone_out The time zone returned to R. Passed to [DBI::dbConnect()].  See https://www.tidyverse.org/blog/2019/12/odbc-1-2-0/.
#' @param verbose Write a message about the status of a successful upload.
#'
#' @details
#' If `transaction` is `TRUE` and the upload fails, the table is rolled back to the state before function was called.
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
#'   convert_logical_to_integer = TRUE,
#'   timezone                   = "America/Chicago",
#'   timezone_out               = "America/Chicago"
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
  timezone                      = "UTC",
  timezone_out                  = "UTC",
  transaction                   = FALSE,

  verbose                       = TRUE
) {

  checkmate::assert_data_frame(d                            , null.ok=FALSE             , any.missing=TRUE)
  checkmate::assert_character(schema_name                   , min.chars=1L  , len=1L, any.missing=FALSE)
  checkmate::assert_character(table_name                    , min.chars=1L  , len=1L, any.missing=FALSE)
  checkmate::assert_character(dsn_name                      , min.chars=1L  , len=1L, any.missing=FALSE)

  checkmate::assert_logical(  clear_table                                   , len=1L, any.missing=FALSE)
  checkmate::assert_logical(  create_table                                  , len=1L, any.missing=FALSE)
  checkmate::assert_logical(  convert_logical_to_integer                    , len=1L, any.missing=FALSE)
  checkmate::assert_logical(  transaction                                   , len=1L, any.missing=FALSE)
  checkmate::assert_character(timezone                                      , len=1L, any.missing=FALSE)
  checkmate::assert_character(timezone_out                                  , len=1L, any.missing=FALSE)
  checkmate::assert_logical(  verbose                                       , len=1L, any.missing=FALSE)

  start_time <- base::Sys.time()
  print(start_time)

  if (convert_logical_to_integer) {
    d <- dplyr::mutate_if(d, is.logical, as.integer)
  }

  requireNamespace("DBI")
  requireNamespace("odbc")

  if (schema_name == "dbo") {
    table_id <- DBI::Id(
      table   = table_name
    )
  } else {
    table_id <- DBI::Id(
      schema  = schema_name,
      table   = table_name
    )
  }

  # Accepts a vanilla name, or a name enclosed in square brackets.
  # pattern <- "^(?:\\[\\w+\\]|\\w+)$"
  pattern <- "^\\w+$"

  # The real way would be to use a conditional, but it's not supported: ^(\[)?\w+(?(1)\])$
  if (!grepl(pattern, schema_name))
    stop("The table's database schema's name must containly only letters, digits, and underscores.  Current versions may be more flexible.")

  if (!grepl(pattern, table_id@name[["table"]]))
    stop("The table's name must containly only letters, digits, and underscores.  Current versions may be more flexible.")


  channel <- DBI::dbConnect(
    drv           = odbc::odbc(),
    dsn           = dsn_name,
    timezone      = timezone,
    timezone_out  = timezone_out
  )

  if (create_table) {
    overwrite <- TRUE
    append    <- FALSE
  } else {
    overwrite <- FALSE
    append    <- TRUE
  }

  tryCatch({
    if (transaction) {
      DBI::dbBegin(channel)
    }

    if (verbose) {
      DBI::dbGetInfo(channel)
    }

    # Check the *qualified* table exists.
    if (!create_table & !DBI::dbExistsTable(channel, table_id))
      stop(glue::glue("The following table does not exist, or is not accessible on this DSN: {schema}.{tbl}", schema = schema_name, tbl=table_name))

    # if( !create_table ) {
    #   sql_count         <- glue::glue("SELECT COUNT(*) FROM {schema}.{tbl}", schema=schema_name, tbl=table_id@name[["table"]])
    #   result_count      <- DBI::dbGetQuery(channel, sql_count)
    #   DBI::dbClearResult(result_count)
    # }

    # Truncate the table's rows/records
    if (!create_table & clear_table) {
      sql_truncate      <- glue::glue(
        "TRUNCATE TABLE {schema}.{tbl}",
        schema  = schema_name,
        tbl     = table_id@name[["table"]]
      )
      result_truncate   <- DBI::dbSendQuery(channel, sql_truncate)
      DBI::dbClearResult(result_truncate)
    }

    # Write the data to the table
    result <- DBI::dbWriteTable(
      conn        = channel,
      name        = table_id,
      value       = d,
      overwrite   = overwrite,
      append      = append
    )

    if (transaction) {
      DBI::dbCommit(channel)
    }

    if (verbose) {
      message(
        sprintf(
          "The table `%s.%s` had %s rows written over dsn `%s` in %0.3f minutes.",
          schema_name,
          table_name,
          format(nrow(d), big.mark = ",", scientific = FALSE),
          dsn_name,
          difftime(Sys.time(), start_time, units = "mins")
        )
      )
    }

  }, error = function(e) {

    if (transaction) {
      DBI::dbRollback(channel)
    }
    stop("Writing to the database was not successful.  Attempted to write table `", table_name, "` over dsn `", dsn_name, "`.\n", e)

  }, finally = {
    if (exists("channel"))
      DBI::dbDisconnect(channel)

    # suppressWarnings(DBI::dbClearResult(result_count))    # A warning message is produced if it was already cleared above.

    if (exists("result_truncate"))
      suppressWarnings(DBI::dbClearResult(result_truncate)) # A warning message is produced if it was already cleared above.
  })
}
