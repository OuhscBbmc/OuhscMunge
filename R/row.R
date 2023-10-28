#' @name row_sum
#' @title Find the sum of selected columns within a row
#'
#' @description Sums across columns within a row,
#' while accounting for nonmissingness.
#' Specify the desired columns by passing their explicit column names or
#' by passing a regular expression to matches the column names.
#'
#' @param d The data.frame containing the values to sum. Required.
#' @param columns_to_average A character vector containing the columns
#' names to sum.
#' If empty, `pattern` is used to select columns. Optional.
#' @param pattern A regular expression pattern passed to [base::grep()]
#' (with `perl = TRUE`).  Optional
#' @param new_column_name The name of the new column that represents the sum
#' of the specified columns.  Required.
#' @param threshold_proportion Designates the minimum proportion of columns
#' that have a nonmissing values (within each row) in order to return a sum.
#' Required; defaults to to 0.75.
#' @param verbose a logical value to designate if extra information is
#' displayed in the console,
#' such as which columns are matched by `pattern`.
#'
#' @return The data.frame `d`, with the additional column containing the row sum.
#'
#' @details
#' If the specified columns are all logicals or integers,
#' the new column will be an [integer].
#' Otherwise the new column will be a [double].
#'
#' @note
#' @author Will Beasley
#' @importFrom rlang :=
#' @examples
#' library(OuhscMunge) #Load the package into the current R session.

#'
#' @export
row_sum <- function(
    d,
    columns_to_average        = character(0),
    pattern,
    new_column_name           = "row_sum",
    threshold_proportion      = .75,
    verbose                   = FALSE
) {
  checkmate::assert_data_frame(d)
  checkmate::assert_character(columns_to_average  , any.missing = FALSE)
  checkmate::assert_character(pattern             , min.len = 0, max.len = 1)
  checkmate::assert_character(new_column_name     , len = 1)
  checkmate::assert_double(   threshold_proportion, len = 1)
  checkmate::assert_logical(   verbose             , len = 1)

  if (length(columns_to_average) == 0L) {
    columns_to_average <-
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
        "The following columns will be summed:\n- ",
        paste(columns_to_average, collapse = "\n- ")
      )
    }
  }

  cast_to_integer <-
    d |>
    dplyr::select(!!columns_to_average) |>
    purrr::every(
      \(x) {
        is.logical(x) | is.integer(x)
      }
    )

  rs <- nonmissing_count <- nonmissing_proportion <- NULL
  d <-
    d |>
    dplyr::mutate(
      rs = # Finding the sum (used by m4)
        rowSums(
          dplyr::across(!!columns_to_average),
          na.rm = TRUE
        ),
      nonmissing_count =
        rowSums(
          dplyr::across(
            !!columns_to_average,
            .fns = \(x) { !is.na(x) }
          )
        ),
      nonmissing_proportion = nonmissing_count / length(columns_to_average),
      {{new_column_name}} :=
        dplyr::if_else(
          threshold_proportion <= nonmissing_proportion,
          rs,
          # rs / nonmissing_count,
          NA_real_
        )
    ) |>
    dplyr::select(
      -rs,
      -nonmissing_count,
      -nonmissing_proportion,
    )
  # Alternatively, return just the new columns
  # dplyr::pull({{new_column_name}})

  if (cast_to_integer) {
    d[[new_column_name]] <- as.integer(d[[new_column_name]])
  }

  d
}
