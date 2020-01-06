#' @name verify_data_frame
#' @export
#'
#' @title Object inherits from [`data.frame`](base::data.frame()). (Soft-deprecated.)
#'
#' @description Check that the object inherits from [`data.frame`](base::data.frame()).
#' If not, throw an error.
#'
#' This function has been soft-deprecated (*i.e.*, it works, but with a warning).
#' Please use the superior [checkmate]() functions, [`checkmate::assert_data_frame()`] and [`checkmate::assert_tibble()`]
#'
#' This helps check database-reading functions (*e.g.*, [`RODBC::sqlQuery()`]) that return a `data.frame`
#' if successful, and a `character` vector is unsucessful.
#'
#' A minimum row count can be used to check that a trivially small number of rows was not returned.
#' If `minimum_row_count` is set to 0, the function is similar to `checkmate::assert_class(d, "data.frame")`,
#' but with with a more specific error message.
#'
#' @param d The object to verify.  Required.
#' @param minimum_row_count The `data.frame` should have at least this many rows.  Defaults to 10.
#' The datatype does not have to be an `integer`, but should be safely convertible to an integer.
#'
#' @author Will Beasley
#'
#' @seealso [`checkmate::assert_class()`]
#'
#' @examples
#' verify_data_frame(datasets::OrchardSprays, 20)
#' verify_data_frame(datasets::iris, 4)

verify_data_frame <- function(d, minimum_row_count = 10L) {
  warning(
    "OuhscMunge::verify_data_frame() is deprecated.\n",
    "Instead, please use checkmate::assert_data_frame()\n",
    "with an appropriate value for the `min.rows` parameter."
  )

  # Verify that a legit data.frame was returned (and not an error message)
  if (!inherits(d, "data.frame")) {
    stop(
      "The object is not a valid data frame.  If the dataset originated from a database table,",
      "check that the SQL code is correct, and a database error was not thrown."
    )
  }

  # Check that the parameter is single, nonmissing positive integer.
  checkmate::assert_integerish(minimum_row_count, len = 1L, lower = 0, any.missing = FALSE)

  checkmate::assert_data_frame(
    d,
    min.rows = minimum_row_count
  )
}
