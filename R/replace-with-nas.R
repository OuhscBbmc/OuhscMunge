#' @name replace_with_nas
#' @export
#'
#' @title Convert blank, zero-length values to `NA`s for a variety of data types.
#'
#' @description Elements of zero-length are converted to `NA`s.  Can force
#' cohersion to an optionally-specified data type.
#'
#' The function is retained so existing code doesn't break.  For new code, consider using [dplyr::na_if()](dplyr::na_if()).
#'
#' @param x An array of values. Required
#' @param return_type Data type of returned vector.  Optional

#' @return An array of values with `NA`s.
#'
#' @details If `return_type` is missing, returned data type will match input.
#' Supports cohersion to `integer`, `numeric`, `character`, `logical`,
#' and `Date` vectors.
#'
#' If `return_type=logical`, a `logical` vector will be returned
#' if `x` contains only blanks and the characters `"0"` and `"1"`.
#'
#' @note Contact the package author if you'd like the function generalized so that additional values
#' (other that `""`) are converted to `NA`s.
#'
#' @author Will Beasley
#'
#' @examples
#' library(OuhscMunge) #Load the package into the current R session.
#' replace_with_nas(c("a", "b", "", "d", ""))
#' replace_with_nas(c("a", "b", "", "d", ""), return_type="character")
#'
#' replace_with_nas(c(1, 2, "", "", 5), return_type="character")
#' replace_with_nas(c(1, 2, "", "", 5)) #Equivalent to previous line.
#' replace_with_nas(c(1, 2, "", "", 5), return_type="integer")
#' replace_with_nas(c(1, 2, "", "", 5), return_type="numeric")
#'
#' replace_with_nas(c("2011-02-03", "", "", "2011-02-24"), return_type="Date")
#' replace_with_nas(c("T", "", "", "F", "FALSE", "", "TRUE"), return_type="logical")
#' replace_with_nas(c("1", "", "", "0", "0"    , "", "1")   , return_type="logical")

replace_with_nas <- function( x, return_type=NULL ) {

  if( is.null(return_type) ) {
    #This function accepts character values with blanks (ie, "").
    #   It converts the blanks to NAs.
    ifelse(x=="", NA, x)

  } else if( return_type == "Date" ) {
    #This function accepts character values with blanks (ie, "").
    #   It first converts the blanks to NAs.
    #   It then converts them to dates.
    x <- as.character(x)
    as.Date(dplyr::if_else(x=="", NA_character_, x))
    # as.Date(ifelse(nchar(x)==0L, NA, x))

  } else if( return_type == "character" ) {
    as.character(ifelse(x=="", NA, x))

  } else if( return_type == "integer" ) {
    as.integer(ifelse(x=="", NA, x))

  } else if( return_type == "numeric" ) {
    as.numeric(ifelse(x=="", NA, x))

  } else if( return_type == "logical" ) {
    if( all(x %in% c("", "0", "1")) ) {
      as.logical(as.integer(ifelse(x=="", NA, x)))
    } else {
      as.logical(ifelse(x=="", NA, x))
    }

  } else {
    stop(paste0("The `return_type` of ", return_type, " is not currently supported."))

  }
}

