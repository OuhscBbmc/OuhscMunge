#' @name first_nonmissing
#'
#' @title First nonmissing element in a vector
#'
#' @description Take the first value that isn't missing.  Adapted from http://stackoverflow.com/a/40515261/1082435.
#' 
#' @param x A vector to of names to convert.
#'
#' @return A vector of converted names.
#'
#' @author Will Beasley

#' @export
first_nonmissing <- function( x ) {
  x[which(!is.na(x))[1]]
}
