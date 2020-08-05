#' @name trim
#' @aliases trim_numeric trim_integer trim_date
#' @title Trim extreme values
#'
#' @description Trim extreme values from an atomic vector, and replace with a specific value (typically `NA_*`).
#'
#' @usage
#' trim_numeric(x, bounds=c(-Inf, Inf), replacement=NA_real_)
#' trim_integer(x, bounds=c(-2147483647L, 2147483647L), replacement=NA_integer_)
#' trim_date(
#'   x,
#'   bounds      = as.Date(c("1940-01-01", "2030-01-01")),
#'   replacement = as.Date(NA_character_)
#' )
# trim_datetime(
#   x,
#   bounds      = as.POSIXct(c("1940-01-01", "2030-01-01")),
#   replacement = as.POSIXct(NA_character_)
# )
#'
#' @param x The input vector to be trimmed.  Required
#' @param bounds A two-element vector that establishes the lower and upper *inclusive* bounds of `x`.
#' @param replacement A scalar that will replace all instances of `x` that fall outside of `bounds`.
#'
#' @return An atomic vector with the same number of elements as `x`.
#'
#' @note
#' The data type of `x`, `bounds`, and `replacement` must match the atomic data type of the function.
#' In other words, `trim_numeric()` accepts only parameters of type 'numeric' (otherwise known as
#' 'double-precision floating point').  Likewise, `trim_date()` accepts only parameters of type `Date`.
#'
#' The lower bound must be less than or equal the upper bound.
#'
#' The default bounds for numerics and integers are at the extremes of the data type.
#' The default bounds for [dates](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Dates.html) are
#' arbitrary, because the origin is slippery.
#'
#' @author Will Beasley
#'
#' @examples
#' library(OuhscMunge)
#' trim_numeric(runif(10, -1, 10), bounds=c(4, 8))
#' trim_integer(c(NA, 1:10), bounds=c(4L, 8L))
#' trim_date(
#'   x      = as.Date(c("1902-02-02", "1999-09-09", "2020-02-22", "1930-01-01", "1930-01-02")),
#'   bounds = as.Date(c("1990-01-01", "2030-01-01"))
#' )
#' trim_datetime(
#'   x      = as.POSIXct(c("1902-02-02", "1999-09-09", "2020-02-22", "1930-01-01", "1930-01-02")),
#'   bounds = as.POSIXct(c("1990-01-01", "2030-01-01"))
#' )


#' @export
trim_numeric <- function(x, bounds = c(-Inf, Inf), replacement = NA_real_) {
  checkmate::assert_numeric(x, any.missing=TRUE)
  checkmate::assert_numeric(bounds, min.len=2, max.len=2, any.missing=FALSE)
  checkmate::assert_numeric(replacement, min.len=1, max.len=1)

  if (!(bounds[1] <= bounds[2]))
    stop("The lower element of `bounds` must be equal or less than the upper element of `bounds`.")

  # Set values that are outside the thresholds to replacement.
  dplyr::if_else(
    condition = dplyr::between(x, bounds[1], bounds[2]),
    true      = x,
    false     = replacement
    # missing   = replacement # A future option is to set missing values to the replacement value also
  )
}

#' @export
trim_integer <- function(x, bounds = c(-2147483647L, 2147483647L), replacement = NA_integer_) {
  checkmate::assert_integer(x, any.missing=TRUE)
  checkmate::assert_integer(bounds, min.len=2, max.len=2, any.missing=FALSE)
  checkmate::assert_integer(replacement, min.len=1, max.len=1)

  if (!(bounds[1] <= bounds[2]))
    stop("The lower element of `bounds` must be equal or less than the upper element of `bounds`.")

  # Set values that are outside the thresholds to replacement.
  dplyr::if_else(
    condition = dplyr::between(x, bounds[1], bounds[2]),
    true      = x,
    false     = replacement
  )
}

#' @export
trim_date <- function(x, bounds = as.Date(c("1940-01-01", "2029-12-31")), replacement = as.Date(NA_character_)) {
  checkmate::assert_date(x, any.missing=TRUE)
  checkmate::assert_date(bounds, min.len=2, max.len=2, any.missing=FALSE)
  checkmate::assert_date(replacement, min.len=1, max.len=1)

  if (!(bounds[1] <= bounds[2]))
    stop("The lower element of `bounds` must be equal or less than the upper element of `bounds`.")

  # Set values that are outside the thresholds to replacement.
  dplyr::if_else(
    condition = dplyr::between(x, bounds[1], bounds[2]),
    true      = x,
    false     = replacement
  )
}
#' @export
trim_datetime <- function(x, bounds = as.POSIXct(c("1940-01-01 00:00", "2029-12-31 23:59")), replacement = as.POSIXct(NA_character_)) {
  checkmate::assert_posixct(x, any.missing=TRUE)
  checkmate::assert_posixct(bounds, min.len=2, max.len=2, any.missing=FALSE)
  checkmate::assert_posixct(replacement, min.len=1, max.len=1)

  if (!(bounds[1] <= bounds[2]))
    stop("The lower element of `bounds` must be equal or less than the upper element of `bounds`.")

  # Set values that are outside the thresholds to replacement.
  dplyr::if_else(
    condition = dplyr::between(x, bounds[1], bounds[2]),
    true      = x,
    false     = replacement
  )
}
