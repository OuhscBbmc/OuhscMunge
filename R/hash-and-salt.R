hash_and_salt_sha256 <- function( x, minLengthInclusive, maxLengthInclusive, requiredMode, saltToAdd ) {
  stopifnot(mode(x)==requiredMode)
  x <- ifelse(x==0, NA_integer_, x)
  stopifnot(all(is.na(x) | (minLengthInclusive <= stringr::str_length(x) & stringr::str_length(x)<=maxLengthInclusive) ))
  salted <- paste0(x, saltToAdd)
  hash <- digest::digest(object=salted, algo="sha256")
  return( ifelse(is.na(x), NA_character_, hash) )
}
