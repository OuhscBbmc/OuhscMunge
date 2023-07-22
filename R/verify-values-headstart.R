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
#' library(magrittr)
#' verify_value_headstart(datasets::OrchardSprays)
#' verify_value_headstart(datasets::iris)
#' verify_value_headstart(datasets::BOD)
#' verify_value_headstart(dplyr::band_members)
#' storms_2 <- dplyr::storms %>%
#'   dplyr::mutate(
#'     storm_date = as.Date(ISOdate(year, month, day))
#'   )
#' verify_value_headstart(storms_2)

verify_value_headstart <- function(d) {
  checkmate::assert_class(d, "data.frame")

  d_structure <- tibble::tibble(
    name_variable     = colnames(d),
    class             = tolower(purrr::map_chr(d, ~class(.)[1])),
    any_missing       = purrr::map_lgl(d, ~any(is.na(.))),
    any_duplicated    = purrr::map_lgl(d, ~any(duplicated(.))),
    boundaries_string = purrr::map_chr(d, boundaries)
  )

  d_structure <- d_structure %>%
    dplyr::mutate(
      class_start     = paste0(.data$class, "("),
      missing_string  = dplyr::if_else(.data$any_missing, ", any.missing=T", ", any.missing=F"),
      unique_string   = dplyr::if_else(!.data$any_duplicated, ", unique=T", "")
    ) %>%
    dplyr::mutate(
      code  = sprintf(
        "checkmate::assert_%-*sds$%-*s %s %-*s %s)",
        max(nchar("character"), nchar(.data$class_start)),
        .data$class_start,
        max(nchar(.data$name_variable)),
        .data$name_variable,
        .data$missing_string,
        max(nchar(.data$boundaries_string)),
        .data$boundaries_string,
        .data$unique_string
      )
    )
  # paste(d_structure$code, collapse="\n")
  cat(d_structure$code, sep = "\n")
}

# Private/non-exposed functions
boundaries <- function(x) {
  data_types <- class(x) # Remember this will have more than one value for columns that inherit multiple datatypes, eg 'factor' and 'ordered'

  if (      "numeric"   %in% data_types) boundaries_number(x)
  else if ( "integer"   %in% data_types) boundaries_number(x)
  else if ( "character" %in% data_types) boundaries_character(x)
  else if ( "Date"      %in% data_types) boundaries_date(x)
  else ""
} # purrr::map_chr(datasets::OrchardSprays, boundaries)

boundaries_number <- function(x) {
  min_number <- suppressWarnings(min(x, na.rm = TRUE))
  max_number <- suppressWarnings(max(x, na.rm = TRUE))

  if (is.infinite(min_number) || is.infinite(max_number))
    cat('stop("The number vector contains only NAs. Set limits you think are appropriate for this variable.")', "\n")

  sprintf(
    ", lower=%s, upper=%s", # Allow for values like 'Inf'
    floor(  min_number),
    ceiling(max_number)
  )
}

boundaries_character <- function(x) {
  min_char_count <- suppressWarnings(as.numeric(min(nchar(x), na.rm = TRUE)))
  max_char_count <- suppressWarnings(as.numeric(max(nchar(x), na.rm = TRUE)))

  if (is.infinite(min_char_count) || is.infinite(max_char_count))
    cat('stop("The character vector contains only NAs. Set limits you think are appropriate for this variable.")', "\n")

  min_char_count <- dplyr::if_else(is.infinite(min_char_count), "NA", as.character(min_char_count))
  max_char_count <- dplyr::if_else(is.infinite(max_char_count), "NA", as.character(max_char_count))

  sprintf(', pattern="^.{%s,%s}$"', min_char_count, max_char_count)
}

boundaries_date <- function(x) {
  min_date <- suppressWarnings(min(x, na.rm = TRUE))
  max_date <- suppressWarnings(max(x, na.rm = TRUE))

  if (is.infinite(min_date) || is.infinite(max_date))
    cat('stop("The date vector contains only NAs. Set limits you think are appropriate for this variable.")', "\n")

  min_date <- dplyr::if_else(is.infinite(min_date), "NA", as.character(min_date))
  max_date <- dplyr::if_else(is.infinite(max_date), "NA", as.character(max_date))

  sprintf(', lower=as.Date("%s"), upper=as.Date("%s")', min_date, max_date)
}
