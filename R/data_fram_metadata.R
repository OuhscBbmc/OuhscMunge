column_rename_headstart <- function( d ) {
  max_column_name <- max(nchar(colnames(d)))
  extra_character_length <- 5L #a comma, two quotes, and two backslashes.
  
  left_side <- paste0(", \"", colnames(d), "\"")
  padded_format <- paste0("%-", max_column_name + extra_character_length, "s")
  left_side <- sprintf(padded_format, left_side)
  
  cat(paste0(left_side, " = ", "\"", colnames(d), "\"", "\n")) #Gives a headstart to plyr::rename
}
# column_rename_headstart(ds)

column_class_headstart <- function( d ) {
  for( i in seq_along(colnames(d)) ) {
    cat( ", \"", colnames(d)[i], "\" = \"", class(d[, i]), "\"\n", sep="")
  }
}
# column_class_headstart(ds)

column_value_headstart <- function( x ) {
  for( value in sort(unique(x)) ) {
    cat( ", \"", value, "\" = \"", value, "\"\n", sep="")
  }
}
# column_value_headstart(ds$Activity)