#' @name assert
#' @aliases assert_non_na assert_non_na_and_unique
#' @title Assert vector characteristics
#'  
#' @description Assert a vector meets important data-quality requirements.
#' 
#' @usage 
#' assert_non_na(x, class_vector, proportion_minimum)
#' assert_non_na_and_unique(x, class_vector)
#' 
#' @param x Vector to inspect.  Required.
#' @param class_vector The required [base::class()] of the vector.  If the parameter is missing, the object's class is not checked.
#' @param proportion_minimum The (inclusive) minimum proportion of the vector's elements that should meet the requirement. If missing, all elements must pass.
#' 
#' @examples 
#' requireNamespace("OuhscMunge") 
#' OuhscMunge::assert_non_na(1:30, "integer")
#' \dontrun{
#' OuhscMunge::assert_non_na(c(1:30, NA_integer_), "integer")
#' }

#' @export
assert_non_na <- function( x, class_vector, proportion_minimum ) {
  
  if( !missing(class_vector) & !inherits(x, class_vector) ) {
    stop("The vector must inherit from the class `", class_vector, "`, but it is a `", class(x), "`.")
  }
  
    
  if( missing(proportion_minimum) ) {
    
    missing_count <- sum(is.na(x))
    if( missing_count > 0L ) {
      stop("The vector should not have any NA values, but `", missing_count, "` elements were NA.")
    }
    
  } else {
    
    proportion_actual <- mean(!is.na(x))
    if( proportion_actual <  proportion_minimum) {
      stop("The vector must have a proportion of at least `", proportion_minimum, "` of nonmissing elements.  However the actual nonmissing proportion is `", proportion_actual, "1.")
    }
    
  }
  
}

#' @export
assert_non_na_and_unique <- function( x, class_vector ) {
  
  assert_non_na(x, class_vector)
    
  duplicate_count <- sum(duplicated(x))
  if( duplicate_count > 0L ) {
    stop("The vector elements should be unique, but `", duplicate_count, "` elements were duplicated.")
  }
}
