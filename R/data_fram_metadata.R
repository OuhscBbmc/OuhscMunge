column_rename_headstart <- function( d ) {
  cat(paste0(", \"", colnames(d), "\"", " = ", "\"", colnames(d), "\"", "\n")) #Gives a headstart to plyr::rename
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