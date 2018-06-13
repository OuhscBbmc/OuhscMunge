#' @name hash_and_salt_sha_256
#' @export
#'
#' @title Salt and hash a value
#'
#' @description Uses [digest::digest()] to hash as (salted) value, using 'SHA-256'.
#' If the `x` isn't already a `character` vector, it's converted to one (even if `x` inherits from character).
#'
#' @param x A vector of values to convert.  it should be a `character` vector, or something that can be cast to a `character` vector.
#' @param salt A single-element character vector.
#' @param min_characters The minimum count of characters that `x` is allowed to be.  Must be an integer.
#' @param max_characters The maximum count of characters that `x` is allowed to be.  Must be an integer.
#'
#' @return A `character vector.
#'
#' @author Will Beasley
#'
#' @examples
#' x    <- letters[1:5]
#'
#' salt <- "abc123"
#' hash_and_salt_sha_256(x, salt)
#'
#' # If an unsalted hash is desired, leave it blank
#' hash_and_salt_sha_256(x)

hash_and_salt_sha_256 <- function( x, salt="", min_characters=1L, max_characters=2048L ) {
  checkmate::assert_character(salt          , any.missing=F)
  checkmate::assert_integer(  min_characters, any.missing=F, len=1, lower=0)
  checkmate::assert_integer(  max_characters, any.missing=F, len=1, lower=min_characters)

  if( class(x) != "character" )
    x <- as.character(x)

  checkmate::assert_character(x, any.missing = T) #, pattern=sprintf("^.{%s,%s}$", min_characters, max_characters))

  if( !all(is.na(x) | dplyr::between(nchar(x), min_characters, max_characters)) )
    stop("All elements to be hashed should be either NA, or be between the specified min and max character length.")

  # x <- ifelse(x==0, NA_integer_, x)
  salted <- paste0(x, salt)

  hash <- purrr::map_chr(salted, digest::digest, algo="sha256")

  return( dplyr::if_else(is.na(x), NA_character_, hash) )
}
