#' @name readr_spec_aligned
#'
#' @title Align & qualify the `col_types` specification passed to [readr::spec_csv()].
#'
#' @description These functions are used during the execution of a program.  Rather they produce snippets
#' that can be pasted into code, and help the developer avoid some typing.
#'
#' @param ... values passed to [readr::spec_csv()].
#'
#' @return Prints formatted code to the console.
#'
#' @author Will Beasley
#'
#' @importFrom utils capture.output
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' readr_spec_aligned(system.file("test-data/subject-1.csv", package = "OuhscMunge"))
#' readr_spec_aligned(system.file("package-dependency-list.csv", package = "OuhscMunge"))

#' @export
readr_spec_aligned <- function( ... ) {
  readr::spec_csv(...) %>%
    utils::capture.output() %>%
    tibble::enframe(name=NULL) %>%
    dplyr::slice(-1, -dplyr::n()) %>%
    dplyr::mutate(

      # Qualify each col_qqq call with 'readr::'
      value   = sub(" = ", " = readr::", .data$value),

      # Enclose variable name in back ticks (if it's not already).
      value   = sub("(\\s+)([^`]+?) = ", "\\1`\\2` = ", .data$value),

      # Isolate the left-& right-hand sides
      left    = sub("\\s+(.+)\\s+=\\s+(.+)$", "\\1", .data$value),
      right   = sub("\\s+(.+)\\s+=\\s+(.+)$", "\\2", .data$value),

      # Pad an odd number of spaces -just beyond the longest variable name.
      padding = nchar(sub("^(.+) = .+", "\\1", .data$value)),
      padding = max(.data$padding) %/%2 * 2 + 1,

      # Pad the left side before appending the right side.
      aligned = sprintf("  %-*s = %s", .data$padding, .data$left, .data$right)
    ) %>%
    dplyr::select(.data$aligned) %>%
    tibble::add_row(
      # value   = NA_character_,
      aligned = "col_types <- readr::cols_only(",
      .before = 1
    ) %>%
    tibble::add_row(
      # value   = NA_character_,
      aligned = ")"
    ) %>%
    dplyr::pull(.data$aligned) %>%
    paste(collapse="\n") %>%
    # paste0( # I'd prefer this approach, but the `.` is causing problems with R CMD check.
    #   "col_types <- readr::cols_only(\n",
    #   .,
    #   "\n)\n"
    # ) %>%
    cat()
}
