#' @name clump_month_date
#' @export
#' @title Assign date for a given year & month
#'
#' @description This accepts a date, but changes the day.  Set/degrade/clump all the days within a month to the same day.
#'
#' @param date_detailed The `Date` value containing the desired year and month.  The day will be overwritten. Required
#' @param day_of_month The factor label assigned to the missing value.  Defaults to `15`.
#'
#' @return An array of `Date` values.
#'
#' @details
#' We use this frequently to set/degrade/clump all the days to the middle of their respective month (ie, the 15th day).
#' The midpoint of a month is usually the most appropriate summary location.  It makes graphs more intuitive.
#' Using the midpoint of month can also avoid problems with timezones.  A date won't get nudged to a neighboring month accidentally.
#'
#' @note
#' A `stop` error will be thrown if `date_detailed` is not a `Date`, or if `day_of_month` is not bounded by \code{[1, 31]}.
#' Be careful that if you set a November date the 31st day, the result will be December 1st.
#' Consequently, we recommend not setting the day to a value after the 28.
#'
#' @author Will Beasley
#'
#' @examples
#' library(OuhscMunge)
#' detailed <- seq.Date(from=as.Date("2011-04-21"), to=as.Date("2011-07-14"), by="day")
#' clumped <- clump_month_date(detailed)
#' table(clumped)
#' # 2011-04-15 2011-05-15 2011-06-15 2011-07-15
#' #         10         31         30         14

clump_month_date <- function( date_detailed, day_of_month=15L ) {
  if( !(class(date_detailed) %in% c("Date", "POSIXct", "POSIXlt")) )
    stop("The `date_detailed` parameter must be a Date, POSIXct, or POSIXlt data type.")
  if( !(class(day_of_month) %in% c("integer", "numeric")) )
    stop("The `day_of_month` parameter must be an integer or numeric data type.")
  if( length(day_of_month) != 1L )
    stop("The `day_of_month` contains more than one element; it should contain only one.")
  if( !(1<=day_of_month & day_of_month<=31) )
    stop("The `day_of_month` parameter must be bound by [1, 31].")

  assigned <- date_detailed
  lubridate::day(assigned) <- day_of_month

  return( assigned )
}
