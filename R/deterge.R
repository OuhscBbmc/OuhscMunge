#' @name deterge
#' @aliases deterge_to_double deterge_to_integer
#' @title Convert (and possibly clean) a vector
#'  
#' @description Cast values to desired data type.
#' 
#' @usage 
#' deterge_to_double(x, bound_lower, bound_upper)
#' deterge_to_integer(x, bound_lower, bound_upper)
#' 
#' @param x The input vector that needs to be cast.  Required
#' @param bound_lower Elements below this inclusive threshold will be set to `NA`.
#' @param bound_upper Elements above this inclusive threshold will be set to `NA`.
#' 
#' @return An array of values.
#' 
#' @details 
#' Accepts character representations of a number, and returns a `numeric` or `integer`
#' vector.  Elements outside `bound_lower` and `bound_upper` are converted to `NA_real_`/`NA_integer_`.
#' 
#' @author Will Beasley
#' 
#' @examples
#' library(OuhscMunge)
#' deterge_to_double(c(NA, 1:10), 4, 8)
#' deterge_to_integer(c(NA, 1:10), 4L, 8L)


#' @export
deterge_to_double <- function( x, bound_lower=-Inf, bound_upper=Inf ) {
  
  if( !(class(bound_lower) %in% c("numeric", "integer") & length(bound_lower)==1L) ) 
    stop("The parameter `bound_lower` must be a numeric or integer vector with exactly one element.")
  if( !(class(bound_upper) %in% c("numeric", "integer") & length(bound_upper)==1L) ) 
    stop("The parameter `bound_upper` must be a numeric or integer vector with exactly one element.")
  
  # Remove commas and convert to a double-precision data type.
  x <- as.numeric(gsub(",", "", x, perl=TRUE))
  
  # Set values that are outside the thresholds to NA.
  
  # Set values that are outside the thresholds to NA.
  trim_numeric(x, c(bound_lower, bound_upper))
}

#' @export
deterge_to_integer <- function( x, bound_lower=-2147483647L, bound_upper=2147483647L ) {
  
  # checkmate::assert_number(bound_lower, mi
  
  if( !(class(bound_lower) %in% c("numeric", "integer") & length(bound_lower)==1L) ) 
    stop("The parameter `bound_lower` must be a numeric or integer vector with exactly one element.")
  if( !(class(bound_upper) %in% c("numeric", "integer") & length(bound_upper)==1L) ) 
    stop("The parameter `bound_upper` must be a numeric or integer vector with exactly one element.")
  
  # Remove commas and convert to a double-precision data type.
  x <- as.integer(gsub(",", "", x, perl=TRUE))
  
  # Set values that are outside the thresholds to NA.
  trim_integer(x, c(bound_lower, bound_upper))
}
