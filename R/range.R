#' @name date_range
#' @aliases date_min_with_nas date_max_with_nas
#' @title Find date ranges in the  prescence of missing subsets
#'
#' @description Return `NA` for the min and max of a date vector if no nonmissing values are presence
#'
#' @usage
#' date_min_with_nas(x)
#' date_max_with_nas(x)
#'
#' @param x The input date vector.  Required

#' @return A date value, that's possibly `NA`.
#'
#' @note
#' This function is a workaround for a weakness in `base::min.date()` and `base::max.date()`.
#' If no nonmissing values are present, both functions *return* +/-`Inf`, but *print* `NA`.
#' These two function return and print `NA`, which behaves like SQL
#' (and probably matches the expectations of most users).
#'
#' See Stack Overflow Questions [Using dplyr::group_by() to find min dates with NAs](https://stackoverflow.com/questions/48470746/using-dplyrgroup-by-to-find-min-dates-with-nas)
#' and [R `Inf` when it has class `Date` is printing `NA`](https://stackoverflow.com/questions/27554410/r-inf-when-it-has-class-date-is-printing-na).
#'
#' The foundation of these functions was proposed in a [response](https://stackoverflow.com/a/48471923/1082435)
#' by Edward Visel (SO username [alistaire](https://stackoverflow.com/users/4497050/alistaire)).
#'
#' @author Edward Visel, Will Beasley
#'
#' @examples
#' library(OuhscMunge)
#' date_min_with_nas(c(NA, NA, NA))
#' date_min_with_nas(as.Date(NA_character_))
#' date_min_with_nas(as.Date(c("2009-04-21", "2017-12-27", NA_character_)))

#' @export
date_min_with_nas <- function( x ) {
  if( all(is.na(x)) )
    as.Date(NA_character_)
  else
    min(x, na.rm = TRUE)
}
date_max_with_nas <- function( x ) {
  if( all(is.na(x)) )
    as.Date(NA_character_)
  else
    max(x, na.rm = TRUE)
}
