#' @name clump_date
#' @aliases clump_month_date clump_week_date
#' 
#' @title Assign date for a given year & month
#'
#' @description This accepts a date, but changes the day.  Set/degrade/clump all the days within a month/week to the same day.
#'
#' @usage 
#' clump_month_date( date_detailed, day_of_month=15L )
#' clump_week_date( date_detailed, day_of_week=2L )
#' 
#' @param date_detailed The `Date` value containing the desired year and month.  The day will be overwritten. Required
#' @param day_of_month The factor label assigned to the missing value.  Defaults to `15` (*i.e.*, the middle of the month).
#' @param day_of_week The factor label assigned to the missing value.  Defaults to `2` (*i.e.*, Monday).
#'
#' @return An array of `Date` values.
#'
#' @details
#' We use this frequently to set/degrade/clump all the days to the middle of their respective month (ie, the 15th day).
#' The midpoint of a month is usually the most appropriate summary location.  It makes graphs more intuitive.
#' Using the midpoint of month can also avoid problems with timezones.  A date won't get nudged to a neighboring month accidentally.
#'
#' @note
#' A `stop` error will be thrown if `date_detailed` is not a `Date`.  `day_of_month` must be bounded by `[1, 31]`, and 
#' `day_of_week` must be bounded by `[1, 7]`.
#' Be careful that if you set a November date the 31st day, the result will be December 1st.
#' Consequently, we recommend not setting the day to a value after the 28.
#'
#' @author Will Beasley
#' 
#' @seealso These functions are gloves around [lubridate::day()] and [lubridate::wday()].  
#' Essentially the add just error-checking and default values.
#'
#' @examples
#' library(OuhscMunge)
#' detailed <- seq.Date(from=as.Date("2011-04-21"), to=as.Date("2011-07-14"), by="day")
#' 
#' clumped_month <- clump_month_date(detailed)
#' table(clumped_month)
#' # 2011-04-15 2011-05-15 2011-06-15 2011-07-15
#' #         10         31         30         14
#' 
#' clumped_week <- clump_week_date(detailed)
#' table(clumped_week)
#' # 2011-04-18 2011-04-25 2011-05-02 2011-05-09 2011-05-16 2011-05-23 2011-05-30 
#' #          3          7          7          7          7          7          7 
#' # 2011-06-06 2011-06-13 2011-06-20 2011-06-27 2011-07-04 2011-07-11 
#' #          7          7          7          7          7          5 

#' @export
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

#' @export
clump_week_date <- function( date_detailed, day_of_week=2L ) {
  if( !(class(date_detailed) %in% c("Date", "POSIXct", "POSIXlt")) )
    stop("The `date_detailed` parameter must be a Date, POSIXct, or POSIXlt data type.")
  if( !(class(day_of_week) %in% c("integer", "numeric")) )
    stop("The `day_of_week` parameter must be an integer or numeric data type.")
  if( length(day_of_week) != 1L )
    stop("The `day_of_week` contains more than one element; it should contain only one.")
  if( !(1<=day_of_week & day_of_week<=7) )
    stop("The `day_of_week` parameter must be bound by [1, 7].")
  
  assigned <- date_detailed
  lubridate::wday(assigned) <- day_of_week
  
  return( assigned )
}
