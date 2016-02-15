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
#' @return A \code{numeric} array of the following elements: \code{in_child}, \code{not_in_child}, \code{deadbeat_proportion}, \code{in_parent}, \code{not_in_parent}, and \code{orphan_proportion}.
#' 
#' @note
#' The \code{join_columns} parameter is passed directly to \code{dplyr::semi_join} and  \code:dplyr::anti_join}.
#' 
#' @author Will Beasley
#' 

match_statistics <- function( d_parent, d_child, join_columns ) {
  in_child            <- nrow(dplyr::semi_join(d_parent, d_child, by=join_columns))
  not_in_child        <- nrow(dplyr::anti_join(d_parent, d_child, by=join_columns))
  deadbeat_proportion <- not_in_child / (in_child + not_in_child)
  
  in_parent            <- nrow(dplyr::semi_join(d_child, d_parent, by=join_columns))
  not_in_parent        <- nrow(dplyr::anti_join(d_child, d_parent, by=join_columns))
  orphan_proportion    <- not_in_parent / (in_parent + not_in_parent)
  
  deadbeats <- c(in_child  = in_child , not_in_child  = not_in_child , deadbeat_proportion  = deadbeat_proportion )
  orphans   <- c(in_parent = in_parent, not_in_parent = not_in_parent, orphan_proportion    = orphan_proportion   )
  return( c(deadbeats, orphans) )
}
