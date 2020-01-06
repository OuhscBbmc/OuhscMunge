retrieve_column_info <- function(dsn_name, table_name) {
  tryCatch(expr = {
      channel <- RODBC::odbcConnect(dsn_name)
      RODBC::getSqlTypeInfo("Microsoft SQL Server")
      RODBC::odbcGetInfo(channel)
      d_column_info <- RODBC::sqlColumns(channel, table_name)
    },
    finally = RODBC::odbcClose(channel)
  )
  d_column_info
}

validate_column_names <- function(dsn_name, table_name, d) {
  # See if the names match exactly.
  d_column_info <- retrieve_column_info(dsn_name, table_name)

  data.frame(
    db_column_name   = d_column_info$COLUMN_NAME, #as.character(d_column_info$COLUMN_NAME),
    df_variable_name = colnames(d),
    violation        = (colnames(d) != d_column_info$COLUMN_NAME) #(colnames(d) != as.character(d_column_info$COLUMN_NAME))
  )
}

validate_non_nulls <- function(dsn_name, table_name, d) {
  #See if the there's a violation of nulls
  d_column_info <- retrieve_column_info(dsn_name, table_name)

  data.frame(
    column_name     = colnames(d),
    db_allows_nulls = as.logical(d_column_info$NULLABLE),
    df_has_nulls    = vapply(d, function(x) any(is.na(x)), logical(1)),
    violation       = (!as.logical(d_column_info$NULLABLE) & vapply(d, function(x) any(is.na(x)), logical(1)))
  )
}

inspect_variable_types <- function(dsn_name, table_name, d) {
  #See if types are consistent.
  d_column_info <- retrieve_column_info(dsn_name, table_name)

  data.frame(
    db_type = d_column_info$TYPE_NAME,
    df_type = vapply(d, class, character(1))
  )
}

validate_character_length <- function(dsn_name, table_name, d) {
  #See if the there's a violation of characters being too long. This only works for strings, not dates or numbers
  d_column_info <- retrieve_column_info(dsn_name, table_name)

  data.frame(
    column_name = colnames(d),
    db_type = d_column_info$TYPE_NAME,
    db_max_size = d_column_info$COLUMN_SIZE,
    df_max_size = vapply(d, function(x) max(nchar(x)), numeric(1)),
    violation_possible = (!as.logical(d_column_info$COLUMN_SIZE) & vapply(d, function(x) max(nchar(x)), numeric(1)))
  )
}

#


### Upload Visit Records to SQL Server
#############################
# sapply(ds, function(x) max(nchar(x)))
# dput(colnames(ds))


# startTime <- base::Sys.time()
# dbTable <- "Osdh.tblC1ClientActivity"
# channel <- RODBC::odbcConnect("MiechvEvaluation")
# RODBC::getSqlTypeInfo("Microsoft SQL Server")
# RODBC::odbcGetInfo(channel)
# columnInfo <- RODBC::sqlColumns(channel, dbTable)
# varTypes <- as.character(columnInfo$TYPE_NAME)
# names(varTypes) <- as.character(columnInfo$COLUMN_NAME)  #varTypes
# RODBC::sqlSave(channel, dsSlim, dbTable, append=TRUE, rownames=FALSE, fast=TRUE, varTypes=varTypes)
# RODBC::odbcClose(channel)
# (elapsedDuration <-  Sys.time() - startTime) # 0.519052 secs 2013-10-27
#
# #Visually inspect the dataset you're trying to upload
# head(dsSlim, 15)
# summary(dsSlim)
#
# # #See if the names match exactly
# # data.frame(DBColumnName = as.character(columnInfo$COLUMN_NAME),
# #            DFVariableName = colnames(dsSlim),
# #            Violation = (colnames(dsSlim)!=as.character(columnInfo$COLUMN_NAME))
# # )
#
# # #See if the there's a violoation of nulls
# # data.frame(ColumnName = colnames(dsSlim),
# #            DBAllowsNulls = as.logical(columnInfo$NULLABLE),
# #            DFHasNulls = sapply(dsSlim, function(x) { any(is.na(x)) }),
# #            Violation = (!as.logical(columnInfo$NULLABLE) & sapply(dsSlim, function(x) { any(is.na(x)) }))
# # )
#
# #See if types are consistent
# data.frame(DBType=varTypes, DFType=sapply(dsSlim, class))
#
# #See if the there's a violoation of characters being too long. This only works for strings, not dates or numbers
# data.frame(ColumnName = colnames(dsSlim),
#            DBType = varTypes,
#            DBMaxSize = columnInfo$COLUMN_SIZE,
#            DFMaxSize = sapply(dsSlim, function(x) { max(nchar(x)) }),
#            PossibleViolation = (!as.logical(columnInfo$COLUMN_SIZE) & sapply(dsSlim, function(x) { max(nchar(x)) }))
# )
