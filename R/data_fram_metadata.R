column_rename_headstart <- function( d ) {
  max_column_name <- max(nchar(colnames(d)))
  extra_character_length <- 5L #a comma, two quotes, and two backslashes.
  
  left_side <- paste0(", \"", colnames(d), "\"")
  padded_format <- paste0("%-", max_column_name + extra_character_length, "s")
  left_side <- sprintf(padded_format, left_side)
  
  right_side <- paste0("\"", colnames(d), "\"\n")
  
  cat(paste0(left_side, " = ", right_side)) #Gives a headstart to plyr::rename
}
# column_rename_headstart(ds)

column_class_headstart <- function( d ) {
  max_column_name <- max(nchar(colnames(d)))
  extra_character_length <- 5L #a comma, two quotes, and two backslashes.
  
  left_side <- paste0(", \"", colnames(d), "\"")
  padded_format <- paste0("%-", max_column_name + extra_character_length, "s")
  left_side <- sprintf(padded_format, left_side)
  
  right_side <- paste0("\"", sapply(d, class), "\"\n")
  
  cat(paste0(left_side, " = ", right_side))
}
# column_class_headstart(ds)

column_value_headstart <- function( x ) {
  values <- sort(unique(x))
  max_value_length <- max(nchar(values))
  extra_character_length <- 5L #a comma, two quotes, and two backslashes.
  
  left_side <- paste0(", \"", values, "\"")
  padded_format <- paste0("%-", max_value_length + extra_character_length, "s")
  left_side <- sprintf(padded_format, left_side)
  
  cat(paste0(left_side, " = \"", values, "\"\n"))
}
# column_value_headstart(ds$Activity)
