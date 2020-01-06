#' @name readr_spec_aligned
#'
#' @title Align & qualify the `col_types` specification passed to [readr::read_csv()].
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
#' # The aligned values produced by this funciton.
#' readr_spec_aligned(system.file("test-data/subject-1.csv", package = "OuhscMunge"))
#' readr_spec_aligned(system.file("package-dependency-list.csv", package = "OuhscMunge"))
#'
#' # For reference, the unaligned output of `readr::spec_csv()`.
#' readr::spec_csv(system.file("test-data/subject-1.csv", package = "OuhscMunge"))
#' readr::spec_csv(system.file("package-dependency-list.csv", package = "OuhscMunge"))

#' @export
readr_spec_aligned <- function(...) {
  pattern <- "^[ ]+`?(.+?)`? = (col_.+)$"
  # pattern <- "^[ ]+(`?)(.+?)\\1 = (col_.+)$"
  . <- NULL   # This is solely for the sake of avoiding the R CMD check error.

  out <-
    readr::spec_csv(...) %>%
    utils::capture.output() %>%
    tibble::enframe(name = NULL) %>%
    dplyr::slice(-1, -dplyr::n()) %>%
    dplyr::mutate(
      # Isolate the left-hand & right-hand sides. Enclose all variable names in back ticks.
      left    = sub(pattern, "`\\1`", .data$value),
      right   = sub(pattern, "\\2"  , .data$value),

      # Calculate the odd number of spaces -just beyond the longest variable name.
      padding = nchar(.data$left),
      padding = max(.data$padding) %/% 2 * 2 + 3,

      # Pad the left side before appending the right side.
      aligned = sprintf("  %-*s = readr::%s", .data$padding, .data$left, .data$right)
    ) %>%
    dplyr::select(.data$aligned) %>%
    # tibble::add_row(
    #   aligned = "col_types <- readr::cols_only(",
    #   .before = 1
    # ) %>%
    # tibble::add_row(
    #   aligned = ")"
    # ) %>%
    dplyr::pull(.data$aligned) %>%
    paste(collapse = "\n") %>%
    # I'd prefer this approach, but the `.` is causing problems with R CMD check.
    paste0(
      "col_types <- readr::cols_only(\n",
      .,
      "\n)\n"
    )

  cat(out)          # Print to the console.
  invisible(out)    # Also, return to the variable, if captured.
}
