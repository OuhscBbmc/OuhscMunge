#' @name snake_case
#' @export
#'
#' @title Convert variable names to snake_case
#'
#' @description This function attempts to convert variables to snake_case, even if it's already in snake_case.
#' The important regex lines were posted by Stack Overflow user [epost](http://stackoverflow.com/users/129879/epost)
#' in ["Elegant Python function to convert CamelCase to snake_case?"](http://stackoverflow.com/a/1176023/1082435).
#'
#' @param x A vector of names to convert.
#'
#' @return A vector of converted names.
#'
#' @note This series of regexes has an advantages over the current
#' implementations of [lettercase::str_snake_case()] and [`snakecase::to_snake_case()`].
#' The former converts "PatientDOB" to "patientdob" and the latter converts
#' "patient.dob" to "patient_._dob".  I'll keep an eye on these packages
#' (*i.e.*, [lettercase #1](https://github.com/decisionpatterns/lettercase/issues/1) for 'camelCase'
#' and [snakecase #101](https://github.com/Tazinho/snakecase/issues/101)).  I'd prefer
#' to use one of them, instead of maintaining the functions.
#'
#' @author Will Beasley
#'
#' @examples
#' snake_case(colnames(datasets::OrchardSprays))
#' snake_case(colnames(datasets::iris))
#' snake_case(c("PatientID", "PatientDOB", "DOB", "name.last", "name.first"))

snake_case <- function( x ) {
  #Second & third lines use http://stackoverflow.com/questions/1175208/elegant-python-function-to-convert-camelcase-to-snake-case
  s <- gsub("\\.", "_", x)                                    # Replace dots with underscores.
  s <- gsub("(.)([A-Z][a-z]+)", "\\1_\\2", s)                 # Separate w/ dashes based on capitalization
  s <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", s))
  s <- gsub(" ", "_", s)                                      # Replace spaces with underscores.
  s <- gsub("__", "_", s)                                     # Replace double-underscores with single.

  return( s )
}
