#' @name data_frame_compare_structure
#'
#' @title Verify equivalent structure of two dataset
#'
#' @description Compare two datasets and throw an error if they have different
#' (a) column counts, (b) column names, and (c) column class
#'
#' @param d_original A `data.frame` that serves as the existing metadata file
#' that potentialy needs to be updated.  Required.
#' @param d_current A `data.frame` that contains records potentialy missing from
#' `d_original`. Required.
#' @param ignore_datestamp A `logical` value indicating whether to ignore a
#' column called `datestamp`.
#'
#' @return If all check pass, and invisible `TRUE` is returned.
#'
#' @note The `datestamp` column is used in metadata operations like
#' [metadata_update_file()] and [data_frame_stack_new()].  In these functions,
#' the `datestamp` column does not have to be present.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Will Beasley
#'
#' @examples
#' # A conventional comparison.
#' ds_original_1 <- tibble::tibble(
#'   x1  = c(1, 3, 4),
#'   x2  = letters[c(1, 3, 4)],
#'   x3  = c(11, 13, 14)
#' )
#' ds_current <- tibble::tibble(
#'   x1  = c(1:5, 1, 5),
#'   x2  = c(letters[1:5], "x", "y"),
#'   x3  = c(11, 12, 13, 14, 15, 11, 15)
#' )
#' data_frame_compare_structure(ds_original_1, ds_current)
#'
#' # When comparing metadata w/ datestamp.
#' ds_original_2 <- tibble::tibble(
#'   x1  = c(1, 3, 4),
#'   x2  = letters[c(1, 3, 4)],
#'   x3  = c(11, 13, 14),
#'   datestamp = Sys.Date()
#' )
#' data_frame_compare_structure(ds_original_2, ds_current, ignore_datestamp = TRUE)
#'
#' @export
data_frame_compare_structure <- function(
  d_original,
  d_current,
  ignore_datestamp = FALSE
) {
  # Check arguments
  checkmate::assert_data_frame(d_original , null.ok = FALSE)
  checkmate::assert_data_frame(d_current  , null.ok = FALSE)
  checkmate::assert_logical(   ignore_datestamp, any.missing = FALSE, len = 1L)

  if (ignore_datestamp) {
    # This doesn't affect the caller's copy of the datasets.
    d_original[["datestamp"]] <- NULL
    d_current[[ "datestamp"]] <- NULL
  }

  # Check column count
  if (ncol(d_original) != ncol(d_current)) {
    stop(
      "The two data.frames have different number of columns.\n",
      "  d_original: ", ncol(d_original), " columns\n",
      "  d_current : ", ncol(d_current ), " columns"
    )
  }

  # Check names
  if (any(colnames(d_original) != colnames(d_current))) {
    stop(
      "The two data.frames have different column names.\n",
      "  d_original: {", paste(colnames(d_original), collapse = ", "), "}\n",
      "  d_current : {", paste(colnames(d_current ), collapse = ", "), "}"
    )
  }

  # Check classes
  class_original <- vapply(d_original, class, character(1))
  class_current  <- vapply(d_current , class, character(1))
  class_mismatch <- (class_original != class_current)
  if (any(class_mismatch)) {
    stop(
      "The two data.frames have different column classes.\n",
      "  d_original: {", paste(names(class_original), class_original, collapse = ", "), "}\n",
      "  d_current : {", paste(names(class_current ), class_current , collapse = ", "), "}"
    )
  }

  invisible(TRUE)
}
