#' @name headstart_utilities
#' @aliases column_rename_headstart column_class_headstart column_value_headstart
#' @export column_rename_headstart column_class_headstart column_value_headstart
#' 
#' @title Utilities for outputting characteristics of a datatset used it code.
#'  
#' @description These functions are used during the execution of a program.  Rather they produce snippets
#' that can be pasted into code, and help the developer avoid some typing.
#' 
#' @usage 
#' column_rename_headstart( d, try_snake_case=TRUE )
#' column_class_headstart( d )
#' column_value_headstart( x )
#' 
#' @param d A `data.frame` to describe.
#' @param x A vector to describe.
#' @param try_snake_case If `TRUE` column names are attempted to be converted to snake_case.
#'
#' @return Prints formatted code to the console.
#' 
#' @author Will Beasley
#' 
#' @examples
#' column_rename_headstart(datasets::OrchardSprays)
#' column_rename_headstart(datasets::iris)
#' column_class_headstart(datasets::OrchardSprays)
#' column_value_headstart(datasets::OrchardSprays$treatment)

column_rename_headstart <- function( d, try_snake_case=TRUE ) {
  max_column_name <- max(nchar(colnames(d)))
  extra_character_length <- 5L # A comma, two quotes, and two backslashes.
  extra_padding <- 10L         # Extra space for convenience.
  
  if( try_snake_case ) {
    left_names <- snake_case(colnames(d)) 
  } else {
    left_names <- colnames(d)
  }
  
  left_side <- paste0(", \"", left_names, "\"")
  padded_format <- paste0("%-", max_column_name + extra_character_length + extra_padding, "s")
  left_side <- sprintf(padded_format, left_side)
  
  right_side <- paste0("\"`", colnames(d), "`\"\n")
  
  cat(paste0(left_side, " = ", right_side)) #Gives a headstart to dplyr::rename_() & plyr::rename()
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
  if( is.factor(x) )
    x <- as.character(x)
  
  values <- sort(unique(x))
  max_value_length <- max(nchar(values))
  extra_character_length <- 5L #a comma, two quotes, and two backslashes.
  
  left_side <- paste0(", \"", values, "\"")
  padded_format <- paste0("%-", max_value_length + extra_character_length, "s")
  left_side <- sprintf(padded_format, left_side)
  
  cat(paste0(left_side, " = \"", values, "\"\n"))
}
# column_value_headstart(ds$Activity)
