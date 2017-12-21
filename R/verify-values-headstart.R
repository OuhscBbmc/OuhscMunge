#' @name verify_value_headstart
#' @export
#'
#' @title Generates code to verify a [`data.frame`](base::data.frame()).
#'
#' @description Inspects properties of a [`data.frame`](base::data.frame()) and
#' prints code to the console that a developer can use to start to
#' check the properties of a dataset, such as the names and types of each column.
#'
#' @param d The `data.frame` to verify.  Required.
#' @author Will Beasley
#'
#' @seealso [checkmate]
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' verify_value_headstart(datasets::OrchardSprays)
#' verify_value_headstart(datasets::iris)
#' verify_value_headstart(datasets::BOD)

verify_value_headstart <- function( d ) {
  # Verify that a legit data.frame.
  if( !inherits(d, "data.frame") ) {
    stop("The object is not a valid data frame.")
  }

  d_structure <- tibble::tibble(
    name_variable     = colnames(d),
    class             = tolower(purrr::map_chr(d, class)),
    any_missing       = purrr::map_lgl(d, ~any(is.na(.))),
    any_duplicated    = purrr::map_lgl(d, ~any(duplicated(.)))
  )

  d_structure <- d_structure %>%
    dplyr::mutate(
    # d_structure,
      missing_string  = dplyr::if_else(.data$any_missing, ", any.missing=T", ", any.missing=F"),
      unique_string   = dplyr::if_else(!.data$any_duplicated, ", unique=T", "")
    ) %>%
    dplyr::mutate(
      code  = sprintf(
        "checkmate::assert_%s(ds$%s %s %s)",
        class,
        .data$name_variable,
        .data$missing_string,
        .data$unique_string
      )
    )
  # paste(d_structure$code, collapse="\n")
  cat(d_structure$code, sep="\n")
}
