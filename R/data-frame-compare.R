#' @name data_frame_compare_structure
#'
#' @title Verify equivalent structure of two dataset
#'
#' @description Compare two datasets and throw an error if they have different
#' (a) column counts, (b) column names, and (c) column class
#'
#' If `keys` is not `NA`, each dataset is verified to not have more then one
#' row with the same values in the combination of `keys`.
#'
#' @param d_original A `data.frame` that serves as the existing metadata file
#' that potentialy needs to be updated.  Required.
#' @param d_current A `data.frame` that contains records potentialy missing from
#' `d_original`. Required.
#' @param keys Column names that represent a vector to describe. `character` vector. Optional.
#'
#' @return If all check pass, and invisible `TRUE` is returned.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Will Beasley
#'
#' @examples
#' library("magrittr")
#' ds_original <- tibble::tibble(
#'   x1  = c(1, 3, 4),
#'   x2  = letters[c(1, 3, 4)],
#'   x3  = c(11, 13, 14)
#' )
#'
#' ds_current <- tibble::tibble(
#'   x1  = c(1:5, 1, 5),
#'   x2  = c(letters[1:5], "x", "y"),
#'   x3  = c(11, 12, 13, 14, 15, 11, 15)
#' )
#'
#' data_frame_compare_structure(ds_original, ds_current, c("x1", "x2"))
#'
#' ds_current %>%
#'   dplyr::anti_join(ds_original, by = c("x1", "x2"))
#'
#' @export
data_frame_compare_structure <- function(d_original, d_current, keys=NA) {
  # Check arguments
  checkmate::assert_data_frame(d_original , null.ok = FALSE)
  checkmate::assert_data_frame(d_current  , null.ok = FALSE)
  checkmate::assert_character( keys       , null.ok = FALSE, any.missing = TRUE, min.len=1, min.chars=1)

  # Check column count
  if (ncol(d_original) != ncol(d_current)) {
    stop(
      "The two data.frames have different number of columns.\n",
      "  d_original: ", ncol(d_original), " columns\n",
      "  d_current : ", ncol(d_current ), " columns"
    )
  }

  # Check names
  if (any(colnames(d_original) != colnames(d_current ))) {
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

  # Check uniqueness in original
  if (!data_frame_uniqueness_test(d_original, keys)) {
    stop(
      "The `d_original` data.frame has multiple rows with the same ",
      "values for column(s)\n{`",
      paste(keys, collapse = "`, `"),
      "`}."
    )
  }

  # Check uniqueness in current
  if (!data_frame_uniqueness_test(d_current, keys)) {
    stop(
      "The `d_current` data.frame has multiple rows with the same ",
      "values for column(s)\n{`",
      paste(keys, collapse = "`, `"),
      "`}."
    )
  }

  invisible(TRUE)
}

# import::from("magrittr", "%>%")
# ds_original <- tibble::tibble(
#   x1  = c(1, 3, 4),
#   x2  = letters[c(1, 3, 4)],
#   x3  = c(11, 13, 14)
# )
#
# ds_current <- tibble::tibble(
#   x1  = c(1:5, 1, 5),
#   x2  = c(letters[1:5], "x", "y"),
#   x3  = c(11, 12, 13, 14, 15, 11, 15)
# )
#
# ds_new <- data_frame_compare_structure(ds_original, ds_current, c("x1", "x2"))
#
# ds_original %>%
#   dplyr::union_all(ds_current)
