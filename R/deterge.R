#' @name deterge
#' @aliases deterge_to_double deterge_to_integer deterge_to_ascii
#' @title Convert (and possibly clean) a vector
#'
#' @description Cast values to desired data type.
#'
#' @usage
#' deterge_to_double(x, bound_lower, bound_upper)
#' deterge_to_integer(x, bound_lower, bound_upper)
#' deterge_to_ascii(x, substitution_character)
#'
#' @param x The input vector that needs to be cast/converted.  Required.
#' @param bound_lower Elements below this inclusive threshold will be set to `NA`.
#' @param bound_upper Elements above this inclusive threshold will be set to `NA`.
#' @param substitution_character If the character does not have an equivalent in ASCII,
#' replace it with this character.  Defaults to a question mark (*i.e.*, '?').
#'
#' @return An array of values.
#'
#' @details
#' The functions `deterge_to_double()` and `deterge_to_integer()` accept character representations of a number, and return a `numeric` or `integer`
#' vector.  Elements outside `bound_lower` and `bound_upper` are converted to `NA_real_`/`NA_integer_`.
#'
#' The function `deterge_to_ascii()` accepts a character vector and returns a character vector.
#' The encoding is changed to ASCII.  Individual elements are allowed to be `NA_character_`.
#'
#' @author Will Beasley
#'
#' @seealso The real work in `deterge_to_ascii()` is performed by [`base::iconv()`].
#' `base::iconv(x=x, from="latin1", to="ASCII//TRANSLIT", sub=substitution_character)`
#'
#' @examples
#' library(OuhscMunge)
#' deterge_to_double(c(NA, 1:10), 4, 8)
#' deterge_to_integer(c(NA, 1:10), 4L, 8L)
#'
#' x <- c("Ekstr\xf8m", "J\xf6reskog", "bi\xdfchen Z\xfcrcher")
#' deterge_to_ascii(x)


#' @export
deterge_to_double <- function( x, bound_lower=-Inf, bound_upper=Inf ) {

  if( !(class(bound_lower) %in% c("numeric", "integer") & length(bound_lower)==1L) )
    stop("The parameter `bound_lower` must be a numeric or integer vector with exactly one element.")
  if( !(class(bound_upper) %in% c("numeric", "integer") & length(bound_upper)==1L) )
    stop("The parameter `bound_upper` must be a numeric or integer vector with exactly one element.")

  # Remove commas and convert to a double-precision data type.
  # x <- as.numeric(gsub(",", "", x, perl=TRUE))
  x <- readr::parse_number(x)

  # Set values that are outside the thresholds to NA.
  trim_numeric(x, c(bound_lower, bound_upper))
}

#' @export
deterge_to_integer <- function( x, bound_lower=-2147483647L, bound_upper=2147483647L ) {

  if( !(class(bound_lower) %in% c("numeric", "integer") & length(bound_lower)==1L) )
    stop("The parameter `bound_lower` must be a numeric or integer vector with exactly one element.")
  if( !(class(bound_upper) %in% c("numeric", "integer") & length(bound_upper)==1L) )
    stop("The parameter `bound_upper` must be a numeric or integer vector with exactly one element.")

  # Remove commas and convert to a double-precision data type.
  x <- as.integer(gsub(",", "", x, perl=TRUE))

  # Set values that are outside the thresholds to NA.
  trim_integer(x, c(bound_lower, bound_upper))
}

#' @export
deterge_to_ascii <- function( x, substitution_character="?" ) {

  # Verify the vector is a `character`.
  checkmate::assert_character(x, null.ok=FALSE,  any.missing=FALSE)

  # Verify the substitution value is a single, nonmissing character.
  checkmate::assert_character(substitution_character, len=1L, any.missing=FALSE)

  # Convert encoding.
  base::iconv(x=x, from="latin1", to="ASCII//TRANSLIT", sub=substitution_character)
}
