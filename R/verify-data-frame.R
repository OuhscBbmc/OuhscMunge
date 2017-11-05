#' @name verify_data_frame
#' @export
#'
#' @title Verify object inherits from [`data.frame`](base::data.frame()).
#'
#' @description Check that the object inherits from [`data.frame`](base::data.frame()).
#' If not, throw an error.
#' 
#' A minimum row count can be used to check tha a trivially small number of rows was not returned.  
#' If this is set to 0, the function is essentially `checkmate::assert_class(d, "data.frame")`.
#' 
#' @param d The object to verify.  Required.
#' @param minimum_row_count The `data.frame` should have at least this may rows.  Defaults to 10.
#'
#' @author Will Beasley
#'
#' @examples
#' verify_data_frame(datasets::OrchardSprays, 20)
#' verify_data_frame(datasets::iris, 4)

verify_data_frame <- function( d, minimum_row_count=10L ) {
  
  # Verify that a legit data.frame was returned (and not an error message)
  # checkmate::assert_class(d, "data.frame")
  if( !inherits(d, "data.frame") ) {
    stop(
      "The object is not a valid data frame.  If the dataset originated from a database table,",
      "check that the SQL code is correct, and a database error was not thrown."
    )
  }
  
  # Check that the parameter is single, nonmissing positive integer.
  checkmate::assert_integer(minimum_row_count, len=1L, lower=0, any.missing=F)
  
  # Verify at least 10 rows were returned (or whatever the argument value was).
  if( nrow(d) < minimum_row_count )
    stop("The dataset passed to `verify_data_frame()` must have at least ", minimum_row_count, " rows.")
}