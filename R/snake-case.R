#' @name snake_case
#' @export
#' 
#' @title Convert variable names to snake_case
#'  
#' @description This function attempts to convert variables to snake_case, even if it's already in snake_case.  
#' The important regex lines were posted by Stack Overflow user \href{http://stackoverflow.com/users/129879/epost}{epost}
#' in \href{http://stackoverflow.com/a/1176023/1082435}{"Elegant Python function to convert CamelCase to snake_case?"}.
#' 
#' @param x A vector to of names to convert.
#' 
#' @return A vector of converted names.
#' 
#' @author Will Beasley
#' 
#' @examples
#' snake_case(colnames(datasets::OrchardSprays))
#' snake_case(colnames(datasets::iris))

snake_case <- function( x ) {
  #Second & third lines use http://stackoverflow.com/questions/1175208/elegant-python-function-to-convert-camelcase-to-snake-case
  s <- gsub("\\.", "_", x)                                    # Replace dots with underscores.
  s <- gsub("(.)([A-Z][a-z]+)", "\\1_\\2", s)                 # Separate w/ dashes basd on capitalization
  s <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", s))
  s <- gsub(" ", "_", s)                                      # Replace spaces with underscores.
  s <- gsub("__", "_", s)                                     # Replace double-underscores with single.
  
  return( s )
}
