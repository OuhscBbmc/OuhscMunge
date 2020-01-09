#' @name data_frame_stack
#'
#' @title Verify equivalent structure of two dataset
#'
#' @description These functions help to compare two metadata frames and assess
#' if new rows should be added.
#'
#' @param d_original A `data.frame` that serves as the existing metadata file
#' that potentialy needs to be updated.  Required.
#' @param d_current A `data.frame` that contains records potentialy missing
#' from `d_original`. Required.
#' @param keys Column names that represent unique combination.
#' `character` vector. Optional.
#'
#' @return A [`tibble::tibble`] that combines `d_original` with the new records
#' from `d_current`.
#'
#' @note each dataset is verified to not have more then one
#' row with the same values in the combination of `keys`
#'
#' @seealso [data_frame_compare_structure()]
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
#' data_frame_stack(ds_original, ds_current, c("x1", "x2"))
#'
#' ds_current %>%
#'   dplyr::anti_join(ds_original, by = c("x1", "x2"))
#'
#' @export
data_frame_stack <- function(d_original, d_current, keys) {

  # Check arguments: d_original & d_current will be checked in `data_frame_compare_structure()`.
  checkmate::assert_character(keys, null.ok = FALSE, any.missing = TRUE, min.len=1, min.chars=1)

  # Check the structure of the two datasets are equivalent
  data_frame_compare_structure(d_original, d_current)

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

  # Isolate the new rows (using the keys/columns)
  d_new <- d_current %>%
    dplyr::anti_join(d_original, by = keys)

  # Stack the two datasets on top of each other
  d_original %>%
    dplyr::union_all(d_new) %>%
    tibble::as_tibble()
}
