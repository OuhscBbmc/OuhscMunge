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
#' @param bound_lower Elements below this inclusive threshold will be set to \code{NA}.
#' @param bound_upper Elements above this inclusive threshold will be set to \code{NA}.
#' 
#' @return An array of values.
#' 
#' @details 
#' --write something here--
#' 
#' @author Will Beasley
#' 
#' @examples
#' library(OuhscMunge)
#' deterge_to_double(c(NA, 1:10), 4, 8)
#' deterge_to_integer(c(NA, 1:10), 4, 8)


#' @export
deterge_to_double <- function( x, bound_lower, bound_upper) {
  
  # Remove commas and convert to a double-precision data type.
  x <- as.numeric(gsub(",", "", x, perl=TRUE))
  
  # Set values that are outside the thresholds to NA.
  dplyr::if_else(
    condition  = dplyr::between(x, bound_lower, bound_upper),
    true       = x, 
    false      = NA_real_,
    missing    = NA_real_ 
  )
}

#' @export
deterge_to_integer <- function( x, bound_lower, bound_upper) {
  # Remove commas and convert to a double-precision data type.
  x <- as.integer(gsub(",", "", x, perl=TRUE))
   
  # Set values that are outside the thresholds to NA.
  dplyr::if_else(
    condition  = dplyr::between(x, bound_lower, bound_upper),
    true       = x, 
    false      = NA_integer_,
    missing    = NA_integer_ 
  )
}
