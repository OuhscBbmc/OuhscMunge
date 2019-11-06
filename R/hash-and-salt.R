#' @name hash_and_salt_sha_256
#' @export
#'
#' @title Salt and hash a value
#'
#' @description Uses [digest::digest()] to hash as (salted) value, using 'SHA-256'.
#' If the `x` isn't already a `character` vector, it's converted to one (even if `x` inherits from character).
#'
#' This approach protects the actual value of `x`, while still allowing a downstream user
#' to determine which cells were derived from the same `x`.
#'
#' For example, suppose a patient's
#' [mrn](https://ushik.ahrq.gov/ViewItemDetails?system=ps&itemKey=88720000)
#' of '111' is hashed, and the output is 'abc'.
#' (To view it's real value, execute `OuhscMunge::hash_and_salt_sha_256(111)`.)
#' When given the value 'abc', is it computational infeasible to determine the input
#' had been '111' (especially when *salted*).  However, when you see that two visits
#' have an mrn of 'abc', you can determine the same patient generated both visits.
#'
#' @param x A vector of values to convert.  it should be a `character` vector, or something that can be cast to a `character` vector.
#' @param salt A single-element character vector.
#' @param min_characters The minimum count of characters that `x` is allowed to be.  Must be an `integer` or `numeric` data type.
#' @param max_characters The maximum count of characters that `x` is allowed to be.  Must be an `integer` or `numeric` data type.
#' @param na_if A vector of characters that should produce a has of `NA_character_`.  Default of `c("")`.
#'
#' @return A character vector.
#'
#' @author Will Beasley
#'
#' @references
#' [cryptographic hash function](https://en.wikipedia.org/wiki/Cryptographic_hash_function)
#' and [salts](https://en.wikipedia.org/wiki/Salt_(cryptography)).
#'
#' @examples
#' x    <- letters[1:5]
#'
#' salt <- "abc123"
#' hash_and_salt_sha_256(x, salt)
#'
#' # If an unsalted hash is desired, leave the `salt` parameter blank
#' hash_and_salt_sha_256(x)
#'
#' # By default, a zero-length character produces hash of NA.
#' hash_and_salt_sha_256(c("a", "", "c"))

hash_and_salt_sha_256 <- function( x, salt="", min_characters=1L, max_characters=2048L, na_if=c("") ) {
  checkmate::assert_character( salt          , any.missing=F, min.chars = 0L)
  checkmate::assert_integerish(min_characters, any.missing=F, len=1, lower=0)
  checkmate::assert_integerish(max_characters, any.missing=F, len=1, lower=min_characters)
  checkmate::assert_character( na_if         , any.missing=T, min.chars = 0)

  if( class(x) != "character" )
    x <- as.character(x)

  checkmate::assert_character(x, any.missing = T) #, pattern=sprintf("^.{%s,%s}$", min_characters, max_characters))


  x[x %in% na_if] <- NA_character_

  if( !all(is.na(x) | dplyr::between(nchar(x), min_characters, max_characters)) )
    stop("All elements to be hashed should be either NA, or be between the specified min and max character length.")

  salted <- paste0(x, salt)

  hash <- purrr::map_chr(salted, digest::digest, algo="sha256")

  return( dplyr::if_else(is.na(x), NA_character_, hash) )
}
