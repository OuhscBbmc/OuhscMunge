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
