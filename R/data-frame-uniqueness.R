#' @name data_frame_uniqueness_test
#'
#' @title Check uniqueness of column combinations
#'
#' @description Useful checking that duplicates of primary keys don't exist
#'
#' @param d A `data.frame` to examine.  Required.
#' @param keys A `character` vector specifying the (combination of) columns that should be unique.
#' @param display_count Maximum number of uniqueness violations to display.
#'
#' @return A `logical` value indicating if uniqueness is satisfied.
#' If `FALSE`, the top rows are printed to the console.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Will Beasley
#'
#' @examples
#' data_frame_uniqueness_test(mtcars, c("cyl"))
#' data_frame_uniqueness_test(mtcars, c("cyl", "vs"))
#' data_frame_uniqueness_test(mtcars, c("cyl", "hp"))
#' data_frame_uniqueness_test(mtcars, c("cyl", "hp"), display_count=0)
#' data_frame_uniqueness_test(mtcars, c("mpg", "wt"))
#'
#' \dontrun{
#' data_frame_uniqueness_assert(mtcars, c("cyl"))
#' data_frame_uniqueness_assert(mtcars, c("cyl", "vs"))
#' data_frame_uniqueness_assert(mtcars, c("mpg", "wt"))
#' }
#'
#' @export
#' @rdname data_frame_uniqueness
data_frame_uniqueness_test <- function(d, keys, display_count = 10L) {
  checkmate::assert_data_frame(d             , null.ok = FALSE)
  checkmate::assert_character( keys          , null.ok = FALSE, any.missing = FALSE, min.len = 1, min.chars = 1L)
  checkmate::assert_integerish(display_count , null.ok = FALSE, any.missing = FALSE, len     = 1, lower     = 0L)

  key_expr <- rlang::parse_exprs(keys)
  n        <- NULL

  d_duplicates <-
    d %>%
    dplyr::count(!!!key_expr) %>%
    dplyr::filter(2L <= .data$n) %>%
    dplyr::rename(row_count = n)

  has_duplicates <- (1L <= nrow(d_duplicates))
  if (has_duplicates && (1L < display_count)) {
    message("Displaying first ", display_count, " violations of uniqueness:")
    print(dplyr::slice(d_duplicates, seq_len(display_count)))
  }

  !has_duplicates
}

#' @export
#' @rdname data_frame_uniqueness
data_frame_uniqueness_assert <- function(d, keys, display_count = 10L) {
  if (!data_frame_uniqueness_test(d, keys, display_count)) {
    stop(
      "The data.frame did not have unique values for column(s)\n{`",
      paste(keys, collapse = "`, `"),
      "`}."
    )
  }
}
