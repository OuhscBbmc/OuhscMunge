#' @name match_statistics
#' @export
#' @title Create explicit factor level for missing values.
#'  
#' @description Missing values are converted to a factor level.  This explicit assignment can reduce the chances that missing values are inadvertantly ignored.  
#' It also allows the presence of a missing to become a predictor in models.
#' 
#' @param d_parent A \code{data.frame} of the parent table.
#' @param d_child A \code{data.frame} of the child table.
#' @param join_columns The \code{character} vector of the column names used to join to parent and child tables.
#' 
#' @return A \code{numeric} array of the following elements: \code{parent_in_child}, \code{parent_not_in_child}, \code{deadbeat_proportion}, \code{child_in_parent}, \code{child_not_in_parent}, and \code{orphan_proportion}.
#' 
#' @details 
#' If a nonexistent column is passed to \code{join_columns}, an error will be thrown naming the violating column name.
#'
#' @note
#' The \code{join_columns} parameter is passed directly to \code{dplyr::semi_join} and \code{dplyr::anti_join}.
#' 
#' @author Will Beasley
#' 
#' @examples 
#' ds_parent <- data.frame(
#'   parent_id         = 1L:10L,
#'   letter            = rep(letters[1:5], each=2),
#'   index             = rep(1:2, times=5),
#'   dv                = runif(10),
#'   stringsAsFactors  = FALSE
#' )
#' ds_child <- data.frame(
#'   child_id          = 101:140,
#'   parent_id         = c(4, 5, rep(6L:14L, each=4), 15, 16),
#'   letter            = rep(letters[3:12], each=4),
#'   index             = rep(1:2, each=2, length.out=40),
#'   dv                = runif(40),
#'   stringsAsFactors  = FALSE
#' )
#' 
#' #Match on one column:
#' match_statistics(ds_parent, ds_child, join_columns="parent_id")
#' 
#' #Match on two columns:
#' match_statistics(ds_parent, ds_child, join_columns=c("letter", "index"))

match_statistics <- function( d_parent, d_child, join_columns ) {
  if( is.null(names(join_columns)) ) {
    flipped_join_columns <- join_columns
  } else {
    flipped_join_columns <- names(join_columns)
    names(flipped_join_columns) <- join_columns
    flipped_join_columns <- ifelse(nchar(flipped_join_columns)==0L, names(flipped_join_columns), flipped_join_columns)
  }
  
  for( i in seq_along(join_columns) ) {
    if( !(flipped_join_columns[i] %in% colnames(d_parent)) )
      base::stop("The variable `", flipped_join_columns[i], "` is not found in the parent table passed to `OuhscMunge::match_statistics()`.")
    if( !(join_columns[i] %in% colnames(d_child)) )
      base::stop("The variable `", join_columns[i], "` is not found in the child table passed to `OuhscMunge::match_statistics()`.")
  }
  
  parent_in_child            <- nrow(dplyr::semi_join(d_parent, d_child, by=join_columns))
  parent_not_in_child        <- nrow(dplyr::anti_join(d_parent, d_child, by=join_columns))
  deadbeat_proportion        <- parent_not_in_child / (parent_in_child + parent_not_in_child)
  
  child_in_parent            <- nrow(dplyr::semi_join(d_child, d_parent, by=flipped_join_columns))
  child_not_in_parent        <- nrow(dplyr::anti_join(d_child, d_parent, by=flipped_join_columns))
  orphan_proportion          <- child_not_in_parent / (child_in_parent + child_not_in_parent)
  
  deadbeats <- c(parent_in_child = parent_in_child, parent_not_in_child = parent_not_in_child, deadbeat_proportion  = deadbeat_proportion )
  orphans   <- c(child_in_parent = child_in_parent, child_not_in_parent = child_not_in_parent, orphan_proportion    = orphan_proportion   )
  
  return( c(deadbeats, orphans) )
}
