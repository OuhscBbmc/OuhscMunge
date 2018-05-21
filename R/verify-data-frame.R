#' @name verify_data_frame
#' @export
#'
#' @title Object inherits from [`data.frame`](base::data.frame()).
#'
#' @description Check that the object inherits from [`data.frame`](base::data.frame()).
#' If not, throw an error.
#'
#' This function will be deprecated in the future.  If you're developing new code,
#' consider the superior [checkmate]() functions, [`checkmate::assert_data_frame()`] and [`checkmate::assert_tibble()`]
#'
#' This helps check database-reading functions (*e.g.*, [`RODBC::sqlQuery()`]) that return a `data.frame`
#' if successful, and a `character` vector is unsucessful.
#'
#' A minimum row count can be used to check that a trivially small number of rows was not returned.
#' If `minimum_row_count` is set to 0, the function is similar to `checkmate::assert_class(d, "data.frame")`,
#' but with with a more specific error message.
#'
#' @param d The object to verify.  Required.
#' @param minimum_row_count The `data.frame` should have at least this may rows.  Defaults to 10.
#' The datatype does not have to be an `integer`, but should be safely convertible to an integer.
#'
#' @author Will Beasley
#'
#' @seealso [`checkmate::assert_class()`]
#'
#' @examples
#' verify_data_frame(datasets::OrchardSprays, 20)
#' verify_data_frame(datasets::iris, 4)

verify_data_frame <- function( d, minimum_row_count=10L ) {

  # Verify that a legit data.frame was returned (and not an error message)
  if( !inherits(d, "data.frame") ) {
    stop(
      "The object is not a valid data frame.  If the dataset originated from a database table,",
      "check that the SQL code is correct, and a database error was not thrown."
    )
  }

  # Check that the parameter is single, nonmissing positive integer.
  checkmate::assert_integerish(minimum_row_count, len=1L, lower=0, any.missing=F)

  # Verify at least 10 rows were returned (or whatever the argument value was).
  if( nrow(d) < minimum_row_count )
    stop("The dataset passed to `verify_data_frame()` must have at least ", minimum_row_count, " rows.")
}
