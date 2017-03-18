

upload_sqls_rodbc <- function( d, channel, table_name, first_upload=FALSE) {
  requireNamespace("RODBC")
  channel <- RODBC::odbcConnect("DhsWaiver")
  # RODBC::getSqlTypeInfo("Microsoft SQL Server")
  # RODBC::odbcGetInfo(channel)
  column_info           <- RODBC::sqlColumns(channel, table_name)
  var_types             <- as.character(column_info$TYPE_NAME)
  names(var_types)      <- as.character(column_info$COLUMN_NAME)  #varTypes

  if( first_upload ) {
    RODBC::sqlSave(channel, d, table_name, append=TRUE, rownames=FALSE, fast=TRUE, varTypes=var_types)
  } else {
    RODBC::sqlSave(channel, d, table_name, append=TRUE, rownames=FALSE, fast=FALSE)#, varTypes=var_types)
  }

  RODBC::odbcClose(channel)
}
