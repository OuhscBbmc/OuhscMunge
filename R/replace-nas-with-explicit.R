#' @name replace_nas_with_explicit
#' @export
#' @title Create explicit factor level for missing values.
#'
#' @description Missing values are converted to a factor level.  This explicit assignment can reduce the chances that missing values are inadvertently ignored.
#' It also allows the presence of a missing to become a predictor in models.
#'
#' The function is retained so existing code doesn't break.  For new code, consider using [dplyr::coalesce()].
#' if you don't need to convert the missing code to a factor level.
#'
#' @param scores An array of values, ideally either factor or character. Required
#' @param new_na_label The factor label assigned to the missing value.  Defaults to `Unknown`.
#' @param create_factor Converts `scores` into a factor, if it isn't one already.   Defaults to `FALSE`.
#' @param add_unknown_level Should a new factor level be created?  (Specify `TRUE` if it already exists.)   Defaults to `FALSE`.
#'
#' @return An array of values, where the `NA` values are now a factor level, with the label specified by the `new_na_label` value.
#'
#' @note
#' The `create_factor` parameter is respected only if `scores` isn't already a factor.  Otherwise, levels without any values would be lost.
#'
#' A `stop` error will be thrown if the operation fails to convert all the `NA` values.
#'
#' @author Will Beasley
#'
#' @examples
#' library(OuhscMunge) #Load the package into the current R session.
#' missing_indices <- c(3, 6, 8, 25)
#' # With a character variable:
#' a <- letters
#' a[missing_indices] <- NA_character_
#' a <- OuhscMunge::replace_nas_with_explicit(a)
#'
#' # With a factor variable:
#' b <- factor(letters, levels=letters)
#' b[missing_indices] <- NA_character_
#' b <- OuhscMunge::replace_nas_with_explicit(b, add_unknown_level=TRUE)

replace_nas_with_explicit <- function(scores, new_na_label = "Unknown", create_factor = FALSE, add_unknown_level = FALSE) {
  if (create_factor && !is.factor(scores)) {
    scores <- factor(scores)
  }

  if (add_unknown_level) {
    if (!is.factor(scores))
      stop("The `replace_nas_with_explicit()` function cannot add a level to `scores` when it is not a factor.  Consider setting `create_factor = TRUE`.")

    levels(scores) <- c(levels(scores), new_na_label)  #Add a new level called 'Unknown'
  }

  scores[is.na(scores)] <- new_na_label #"Unknown"

  if (any(is.na(scores)))
    stop("The reassigned factor variable should not have any NA values.")

  scores
}
