#' @name data_frame_stack_new
#'
#' @title Verify equivalent structure of two dataset
#'
#' @description These functions help to compare two metadata frames and assess
#' if new rows should be added.
#'
#' @param d_original A `data.frame` that serves as the existing metadata file
#' that potentially needs to be updated.  Required.
#' @param d_current A `data.frame` that contains records potentially missing
#' from `d_original`. Required.
#' @param keys Column names that represent unique combination.
#' `character` vector. Optional.
#' @param path Location of the metadata file to potentially updated.
#' Required `character` vector.
#' @param datestamp_update A `logical` value indicating whether to ignore a
#' column called `datestamp`. Defaults to `FALSE`.
#' @param datestamp_value A `Date` value assigned to the `datestamp` column
#' for the records in `d_current` not present in `d_original` when
#' `datestamp_update` is `TRUE`.
#' Defaults to today.
#' @param stat_columns The name(s) of columns containing values to update.
#' These values in `d_current` with *overwrite* the values in `d_original`.
#'
#' @return A [`tibble::tibble`] that combines `d_original` with the new records
#' from `d_current`.
#'
#' @note Each dataset is verified to not have more then one
#' row with the same values in the combination of `keys`
#'
#' The `stat_columns` typically contain metrics like 'count' or 'mean'
#' which may become obsolete in `d_original`.  These values are dropped
#' from `d_original` and replaced by the columns in `d_current`, after
#' joining on the `keys` column(s).
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
#'   x1         = c(1, 3, 4),
#'   x2         = letters[c(1, 3, 4)],
#'   x3         = c(11, 13, 14),
#'   x4         = c(111, 113, 114),
#'   x5         = c(-11, -13, -14),
#'   datestamp  = as.Date("2020-01-07")
#' )
#'
#' ds_current <- tibble::tibble(
#'   x1   = c(1:5, 1, 5),
#'   x2   = c(letters[1:5], "x", "y"),
#'   x3   = c(11, 12, 13, 14, 15, 11, 15),
#'   x4   = c(211, 212, 213, 214, 215, 211, 215),
#'   x5   = c(311, 312, 313, 314, 315, 311, 315)
#' )
#'
#' # Basic: append the new records.
#' data_frame_stack_new(
#'   d_original       = ds_original,
#'   d_current        = ds_current,
#'   keys             = c("x1", "x2")
#' )
#'
#' # Wrinkle 1: datestamp the new records.
#' data_frame_stack_new(
#'   d_original       = ds_original,
#'   d_current        = ds_current,
#'   keys             = c("x1", "x2"),
#'   datestamp_update = TRUE
#' )
#'
#' # Wrinkle 2a: datestamp the new records; update x4.
#' data_frame_stack_new(
#'   d_original       = ds_original,
#'   d_current        = ds_current,
#'   keys             = c("x1", "x2"),
#'   datestamp_update = TRUE,
#'   stat_columns     = c("x4")
#' )
#'
#' # Wrinkle 2b: datestamp the new records; update x4 & x5.
#' data_frame_stack_new(
#'   d_original       = ds_original,
#'   d_current        = ds_current,
#'   keys             = c("x1", "x2"),
#'   datestamp_update = TRUE,
#'   stat_columns     = c("x4", "x5")
#' )
#'
#' ds_current %>%
#'   dplyr::anti_join(ds_original, by = c("x1", "x2"))
#'
#' # Update a file
#' \dontrun{
#' {
#'   path_temp <- tempfile(fileext = ".csv")
#'   on.exit(unlink(path_temp))
#'   file.copy(
#'     system.file("test-data/metadata-original.csv", package = "OuhscMunge"),
#'     path_temp
#'   )
#' }
#'
#' # Displays 3 rows.
#' readr::read_csv(path_temp)
#'
#' metadata_update_file(
#'   path_temp,
#'   dplyr::mutate(ds_current, x1 = as.character(x1), x3 = as.character(x3)),
#'   c("x1", "x2")
#' )
#'
#' # Displays 7 rows.
#' readr::read_csv(path_temp)
#' }
#'
#' @export
data_frame_stack_new <- function(
  d_original,
  d_current,
  keys,
  datestamp_update  = FALSE,
  datestamp_value   = Sys.Date(),
  stat_columns      = character(0)
) {

  # Check arguments: d_original & d_current will be checked in `data_frame_compare_structure()`.
  checkmate::assert_character(keys             , any.missing = TRUE , min.len = 1, min.chars = 1)
  checkmate::assert_logical(  datestamp_update , any.missing = FALSE, len = 1L)
  checkmate::assert_date(     datestamp_value  , any.missing = FALSE, len = 1L)
  checkmate::assert_character(stat_columns     , any.missing = TRUE , min.len = 0, min.chars = 1)

  # Check the structure of the two datasets are equivalent
  data_frame_compare_structure(d_original, d_current, datestamp_ignore = TRUE)

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

  if (datestamp_update) {
    if (is.null(d_original[["datestamp"]])) {
      stop(
        "If `datestamp_update` is true, then the data.frame `d_original` ",
        "must have a `datestamp` colum. ",
        "The column is allowed to contain missing values."
      )
    }
    checkmate::assert_date(d_original[["datestamp"]], any.missing = TRUE)

    d_new <-
      d_new %>%
      dplyr::mutate(
        datestamp = datestamp_value
      )
  }

  # Stack the two datasets on top of each other
  d <-
    d_original %>%
    dplyr::union_all(d_new) %>%
    tibble::as_tibble()

  if (1L <= length(stat_columns)) {
    # Isolate the key & stat columns
    d_stat <-
      d_current %>%
      dplyr::select(
        tidyselect::one_of(keys),
        tidyselect::one_of(stat_columns)
      )

    # Drop from d_new; replace with d_current
    d <-
      d %>%
      dplyr::select(
        -tidyselect::one_of(stat_columns)
      ) %>%
      dplyr::left_join(d_stat, by = keys)
  }

  d
}

#' @rdname data_frame_stack_new
#' @export
metadata_update_file <- function(
  path,
  d_current,
  keys,
  datestamp_update  = FALSE,
  datestamp_value   = Sys.Date(),
  stat_columns      = character(0)
) {
  checkmate::assert_file_exists(path)

  d_original <-
    readr::read_csv(
      file      = path,
      col_types = readr::cols(.default = readr::col_character())
    ) %>%
    dplyr::mutate_all(as.character)

  d_current <-
    d_current %>%
    dplyr::mutate_all(as.character)

  if (datestamp_update) {
    # Convert
    d_original[["datestamp"]] <- as.Date(d_original[["datestamp"]])
  }

  d_new <- data_frame_stack_new(
    d_original,
    d_current,
    keys,
    datestamp_update  = datestamp_update,
    datestamp_value   = datestamp_value,
    stat_columns      = stat_columns
  )

  readr::write_csv(
    x     = d_new,
    path  = path
  )
}
