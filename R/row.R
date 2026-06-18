#' @name row
#' @title Find the sum of selected columns within a row
#'
#' @description Sums across columns within a row,
#' while accounting for nonmissingness.
#' Specify the desired columns by passing their explicit column names or
#' by passing a regular expression to matches the column names.
#'
#' @param d The data.frame containing the values to sum. Required.
#' @param columns_to_process A character vector containing the columns
#' names to process (_e.g._, to average or to sum).
#' If empty, `pattern` is used to select columns. Optional.
#' @param pattern A regular expression pattern passed to [base::grep()]
#' (with `perl = TRUE`).  Optional
#' @param new_column_name The name of the new column that represents the sum
#' of the specified columns.  Required.
#' @param threshold_proportion Designates the minimum proportion of columns
#' that have a nonmissing values (within each row) in order to return a sum.
#' Required; defaults to to 0.75.
#' In other words, by default, if less than 75% of the specified
#' cells are missing within a row, the row sum will be `NA`.
#' @param nonmissing_count_name If a non-NA value is passed,
#' a second column will be added to `d` that contains the row's count
#' of nonmissing items among the selected columns.
#' Must be a valid column name.  Optional.
#' @param verbose a logical value to designate if extra information is
#' displayed in the console,
#' such as which columns are matched by `pattern`.
#'
#' @return The data.frame `d`,
#' with the additional column containing the row sum.
#' If a valid value is passed to `nonmissing_count_name`,
#' a second column will be added as well.
#'
#' @details
#' If the specified columns are all logicals or integers,
#' the new column will be an [integer].
#' Otherwise the new column will be a [double].
#'
#' @author Will Beasley
#' @importFrom rlang :=
#' @examples
#' mtcars |>
#'   OuhscMunge::row_sum(
#'     columns_to_process = c("cyl", "disp", "vs", "carb"),
#'     new_column_name    = "engine_sum"
#'   )
#'
#' mtcars |>
#'   OuhscMunge::row_sum(
#'     columns_to_process     = c("cyl", "disp", "vs", "carb"),
#'     new_column_name        = "engine_sum",
#'     nonmissing_count_name  = "engine_nonmissing_count"
#'   )
#'
#' mtcars |>
#'   OuhscMunge::row_mean(
#'     columns_to_process     = c("cyl", "disp", "vs", "carb"),
#'     new_column_name        = "engine_mean",
#'     nonmissing_count_name  = "engine_nonmissing_count"
#'   )
#'
#' if (require(tidyr))
#'   tidyr::billboard |>
#'     OuhscMunge::row_sum(
#'       pattern               = "^wk\\d{1,2}$",
#'       new_column_name       = "week_sum",
#'       threshold_proportion  = .1,
#'       verbose               = TRUE
#'     ) |>
#'     dplyr::select(
#'       artist,
#'       date.entered,
#'       week_sum,
#'     )
#'
#'   tidyr::billboard |>
#'     OuhscMunge::row_sum(
#'       pattern               = "^wk\\d$",
#'       new_column_name       = "week_sum",
#'       verbose               = TRUE
#'     ) |>
#'     dplyr::select(
#'       artist,
#'       date.entered,
#'       week_sum,
#'     )

#' @rdname row
#' @export
row_sum <- function(
  d,
  columns_to_process        = character(0),
  pattern                   = "",
  new_column_name           = "row_sum",
  threshold_proportion      = .75,
  nonmissing_count_name     = NA_character_,
  verbose                   = FALSE
) {
  checkmate::assert_data_frame(d)
  checkmate::assert_character(columns_to_process  , any.missing = FALSE)
  checkmate::assert_character(pattern             , len = 1)
  checkmate::assert_character(new_column_name     , len = 1)
  checkmate::assert_double(   threshold_proportion, len = 1)
  checkmate::assert_character(nonmissing_count_name, len = 1, min.chars = 1, any.missing = TRUE)
  checkmate::assert_logical(  verbose             , len = 1)

  if (length(columns_to_process) == 0L) {
    columns_to_process <-
      d |>
      colnames() |>
      grep(
        x         = _,
        pattern   = pattern,
        value     = TRUE,
        perl      = TRUE
      )

    if (verbose) {
      message(
        "The following columns will be processed:\n- ",
        paste(columns_to_process, collapse = "\n- ")
      )
    }
  }

  cast_to_integer <-
    d |>
    dplyr::select(!!columns_to_process) |>
    purrr::every(
      \(x) {
        is.logical(x) | is.integer(x)
      }
    )

  .rs <- .nonmissing_count <- .nonmissing_proportion <- NULL
  d <-
    d |>
    dplyr::mutate(
      .rs =
        rowSums(
          dplyr::across(!!columns_to_process),
          na.rm = TRUE
        ),
      .nonmissing_count =
        rowSums(
          dplyr::across(
            !!columns_to_process,
            .fns = \(x) {
              !is.na(x)
            }
          )
        ),
      .nonmissing_proportion = .nonmissing_count / length(columns_to_process),
      {{new_column_name}} :=
        dplyr::if_else(
          threshold_proportion <= .nonmissing_proportion,
          .rs,
          # .rs / .nonmissing_count,
          NA_real_
        )
    )

  if (!is.na(nonmissing_count_name)) {
    d <-
      d |>
      dplyr::mutate(
        {{nonmissing_count_name}} := .nonmissing_count,
      )
  }

  d <-
    d |>
    dplyr::select(
      -.rs,
      -.nonmissing_count,
      -.nonmissing_proportion,
    )
  # Alternatively, return just the new columns
  # dplyr::pull({{new_column_name}})

  if (cast_to_integer) {
    d[[new_column_name]] <- as.integer(d[[new_column_name]])
  }

  d
}

#' @rdname row
#' @export
row_mean <- function(
  d,
  columns_to_process        = character(0),
  pattern                   = "",
  new_column_name           = "row_mean",
  threshold_proportion      = .75,
  nonmissing_count_name     = NA_character_,
  verbose                   = FALSE
) {
  checkmate::assert_data_frame(d)
  checkmate::assert_character(columns_to_process  , any.missing = FALSE)
  checkmate::assert_character(pattern             , len = 1)
  checkmate::assert_character(new_column_name     , len = 1)
  checkmate::assert_double(   threshold_proportion, len = 1)
  checkmate::assert_character(nonmissing_count_name, len = 1, min.chars = 1, any.missing = TRUE)
  checkmate::assert_logical(  verbose             , len = 1)

  if (length(columns_to_process) == 0L) {
    columns_to_process <-
      d |>
      colnames() |>
      grep(
        x         = _,
        pattern   = pattern,
        value     = TRUE,
        perl      = TRUE
      )

    if (verbose) {
      message(
        "The following columns will be processed:\n- ",
        paste(columns_to_process, collapse = "\n- ")
      )
    }
  }

  .rm <- .nonmissing_count <- .nonmissing_proportion <- NULL
  d <-
    d |>
    dplyr::mutate(
      .rm =
        rowMeans(
          dplyr::across(!!columns_to_process),
          na.rm = TRUE
        ),
      .nonmissing_count =
        rowSums(
          dplyr::across(
            !!columns_to_process,
            .fns = \(x) {
              !is.na(x)
            }
          )
        ),
      .nonmissing_proportion = .nonmissing_count / length(columns_to_process),
      {{new_column_name}} :=
        dplyr::if_else(
          threshold_proportion <= .nonmissing_proportion,
          .rm,
          # .rs / .nonmissing_count,
          NA_real_
        )
    )

  if (!is.na(nonmissing_count_name)) {
    d <-
      d |>
      dplyr::mutate(
        {{nonmissing_count_name}} := .nonmissing_count,
      )
  }

  d <-
    d |>
    dplyr::select(
      -.rm,
      -.nonmissing_count,
      -.nonmissing_proportion,
    )
  # Alternatively, return just the new columns
  # dplyr::pull({{new_column_name}})

  d
}
