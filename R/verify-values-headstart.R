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

verify_value_headstart <- function( d ) {
  # Verify that a legit data.frame.
  if( !inherits(d, "data.frame") ) {
    stop("The object is not a valid data frame.")
  }

  d_structure <- data.frame(
    name_variable     = colnames(d),
    class             = tolower(purrr::map_chr(d, class)),
    stringsAsFactors  = F
  )

  d_structure <- dplyr::mutate(
    d_structure,
    code  = sprintf("checkmate::assert_%s(ds$%s)", class, .data$name_variable)
  )
  # paste(d_structure$code, collapse="\n")
  cat(d_structure$code, sep="\n")
}
