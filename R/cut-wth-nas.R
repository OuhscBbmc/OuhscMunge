#' @name cut_with_nas
#' @export
#'
#' @title Convert numeric to factor, with an explicit level for `NA`s
#'
#' @description Like [base::cut()], but creates a level representing the missing values.
#'
#' @param x A `numeric` or `integer` vector to cut into factor levels.  Required.
#' @param .missing The name of the level representing the `NA` values within `x`.
#' @param ... further arguments passed to [base::cut()].
#'
#' @return A `factor`.
#'
#' @note Discussed in the Stack Overflow question,
#' ["cut() a variable with missing values"](https://stackoverflow.com/questions/50882620/cut-a-variable-with-missing-values)
#'
#' @author Will Beasley
#'
#' @examples
#' w      <- c(0L, NA_integer_, 22:25, NA_integer_, 40)
#' breaks <- c(0, 25, 50)
#' labels <- c("lower", "upper")
#'
#' cut_with_nas(w, breaks=2)
#' cut_with_nas(w, breaks=breaks, labels=labels)
#' cut_with_nas(w, breaks=breaks               )
#' cut_with_nas(w, breaks=breaks               , include.lowest=T)
#' cut_with_nas(w, breaks=breaks               , include.lowest=T, right=F)
#' cut_with_nas(w, breaks=breaks                                 , right=F)

cut_with_nas   <- function( x, .missing="Unknown", ... ) {
  y <- cut(x, ...)
  y <- addNA(y)
  levels(y)[is.na(levels(y))] <- .missing
  return( y )
}

