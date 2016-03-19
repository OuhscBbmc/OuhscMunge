#' @name replace_with_nas
#' @export
#' 
#' @title Convert blank, zero-length values to \code{NA}s for a variety of data types.
#'  
#' @description Elements of zero-length are converted to \code{NA}s.  Can force
#' cohersion to an optionally-specified data type.
#' 
#' @param x An array of values. Required
#' @param return_type Data type of returned vector.  Optional

#' @return An array of values with \code{NA}s.
#' 
#' @details If \code{return_type} is missing, returned data type will match input.
#' Supports cohersion to \code{integer}, \code{numeric}, \code{character}, \code{logical},
#' and \code{Date} vectors.  
#' 
#' If \code{return_type=logical}, a \code{logical} vector will be returned
#' if \code{x} contains only blanks and the characters \code{"0"} and \code{"1"}.
#' 
#' @note Contact the package author if you'd like the function generalized so that additional values
#' (other that \code{""}) are converted to \code{NA}s.
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

replace_with_nas <- function( x, return_type=NULL ) {
  
  if( is.null(return_type) ) {
    #This function accepts character values with blanks (ie, "").
    #   It converts the blanks to NAs.
    ifelse(x=="", NA, x)
    
  } else if( return_type == "Date" ) {
    #This function accepts character values with blanks (ie, "").
    #   It first converts the blanks to NAs.
    #   It then converts them to dates.
    as.Date(ifelse(x=="", NA, x))
    
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

