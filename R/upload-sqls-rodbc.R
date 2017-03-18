#' @name upload_sqls_rodbc
#' @export
#' @title Upload to a SQL Server database using RODBC
#'  
#' @description The function performs some extra configuration to improve robustness.
#' 
#' @param d Dataset to be uploaded to the dataset.  The object must inherit from `data.frame`.
#' @param table_name Name of the database destination table.
#' @param dsn_name Name of the locally-defined DSN passed to [RODBC::odbcConnect()](RODBC::odbcConnect()).
#' @param clear_table If `TRUE`, calls [RODBC::sqlClear()](RODBC::sqlClear()) before writing to the table.
#' @param create_table If the table structure has not yet been defined in the database, it will be created if `create_table` is `TRUE`.

upload_sqls_rodbc <- function( d, table_name, dsn_name, clear_table=FALSE, create_table=FALSE) {
  requireNamespace("RODBC")
  channel <- RODBC::odbcConnect(dsn_name)
  # RODBC::getSqlTypeInfo("Microsoft SQL Server")
  # RODBC::odbcGetInfo(channel)
  column_info           <- RODBC::sqlColumns(channel, table_name)
  var_types             <- as.character(column_info$TYPE_NAME)
  names(var_types)      <- as.character(column_info$COLUMN_NAME)  #varTypes
  
  if( clear_table )
    RODBC::sqlClear(channel, table_name)

  if( create_table ) {
    RODBC::sqlSave(channel, d, table_name, append=TRUE, rownames=FALSE, fast=TRUE, varTypes=var_types)
  } else {
    RODBC::sqlSave(channel, d, table_name, append=TRUE, rownames=FALSE, fast=FALSE)#, varTypes=var_types)
  }

  RODBC::odbcClose(channel)
}
