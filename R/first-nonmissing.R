#' @name first_nonmissing
#'
#' @title First nonmissing element in a vector
#'
#' @description Take the first value that isn't missing.  Adapted from http://stackoverflow.com/a/40515261/1082435.
#'
#' @param x A vector of values to collapse.
#' @param value_if_all_na A scalar value to return if all elements are missing
#' (*i.e.*, they are all either `NA`, or one of the `na_codes`).
#' @param na_codes A vector of codes that represent missing values.
#'
#' @return A scalar of converted names.
#'
#' @details `value_if_all_na` and `na_codes`must have the same data type as `x`.
#'
#' If `value_if_all_na` is null, then an `NA` will be returned.
#' If `na_codes` is null, then all non-NA values are considered.
#'
#' @author Will Beasley
#' @examples
#' first_nonmissing(c(NA, "b", "c"))
#' first_nonmissing(c(NA_character_, NA_character_))
#' first_nonmissing(character(0))

#' @export
first_nonmissing <- function(
  x,
  value_if_all_na = NULL,
  na_codes = NULL
) {
  # x[which(!is.na(x))[1]]
  if (rlang::is_null(na_codes)) {
    indices_na <- which(!is.na(x))
  } else {
    indices_na <- which(!is.na(x) & !(x %in% na_codes))
  }

  y <- x[indices_na[1]]

  if (rlang::is_null(value_if_all_na)) {
    return(y)
  } else {
    return(dplyr::coalesce(y, value_if_all_na))
  }
}
