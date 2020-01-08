#' @name data_frame_compare
#' @aliases data_frame_compare
#'
#' @title Identify new rows and add.
#'
#' @description These functions help to compare two metadata frames as assess if new rows
#' should be added.
#'
#'
#' @param d_original A `data.frame` that serves as the existing metadata file
#' that potentialy needs to be updated.  Required.
#' @param d_current A `data.frame` that contains records potentialy missing from
#' `d_original`. Required.
#' @param columns A vector to describe.
#'
#' @return `data.frame`
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
#' data_frame_compare(ds_original, ds_current, c("x1", "x2"))
#'
#' ds_current %>%
#'   dplyr::anti_join(ds_original, by = c("x1", "x2"))
#'
#' @export
data_frame_compare <- function(d_original, d_current, columns) {
  checkmate::assert_data_frame(d_original , null.ok = FALSE)
  checkmate::assert_data_frame(d_current  , null.ok = FALSE)
  checkmate::assert_character( columns    , null.ok = FALSE, any.missing=F, min.len=1, min.chars=1)

  if (!data_frame_uniqueness_test(d_original, columns)) {
    stop(
      "The `d_original` data.frame has multiple rows with the same ",
      "values for column(s)\n{`",
      paste(columns, collapse="`, `"),
      "`}."
    )
  }
  if (!data_frame_uniqueness_test(d_current, columns)) {
    stop(
      "The `d_current` data.frame has multiple rows with the same ",
      "values for column(s)\n{`",
      paste(columns, collapse="`, `"),
      "`}."
    )
  }

  d_current %>%
    dplyr::anti_join(d_original, by = columns)
}

# import::from("magrittr", "%>%")
#
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
# ds_new_1 <-
#   ds_current %>%
#   dplyr::anti_join(ds_original, by = c("x1", "x2"))
# ds_new_1
#
# ds_new_2 <- data_frame_compare(ds_original, ds_current, c("x1", "x2"))
#
# ds_original %>%
#   dplyr::union_all(ds_new_2)
