#' @name match_statistics
#' @aliases match_statistics match_statistics_display
#' @export match_statistics match_statistics_display
#' @title Create explicit factor level for missing values.
#'
#' @description Missing values are converted to a factor level.  This explicit assignment can reduce the chances that missing values are inadvertantly ignored.
#' It also allows the presence of a missing to become a predictor in models.
#'
#' @param d_parent A `data.frame` of the parent table.
#' @param d_child A `data.frame` of the child table.
#' @param join_columns The `character` vector of the column names used to join to parent and child tables.
#'
#' @return
#'   A `numeric` array of the following elements:
#'   * `parent_in_child`          The count of parent records found in the child table.
#'   * `parent_not_in_child`      The count of parent records *not* found in the child table.
#'   * `parent_na_any`            The count of parent records with a `NA` in at least one of the join columns.
#'   * `deadbeat_proportion`      The proportion of parent records *not* found in the child table.
#'   * `child_in_parent`          The count of child records found in the parent table.
#'   * `child_not_in_parent`      The count of child records *not* found in the parent table.
#'   * `child_na_any`             The proportion of child records *not* found in the parent table.
#'   * `orphan_proportion`        The count of child records with a `NA` in at least one of the join columns.
#'
#' @details If a nonexistent column is passed to `join_columns`, an error will be thrown naming the violating column name.
#'
#' More information about the 'parent' and 'child' terminology and concepts can be found in the
#' [Hierarchical Database Model](https://en.wikipedia.org/wiki/Hierarchical_database_model)
#' Wikipedia entry, among many other sources.
#'
#' @note  The `join_columns` parameter is passed directly to [`dplyr::semi_join()`](dplyr::semi_join()) and [`dplyr::anti_join()`](dplyr::anti_join()).
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
#'
#'## Produce better format for humans to read
#' match_statistics_display(ds_parent, ds_child, join_columns="parent_id")
#' match_statistics_display(ds_parent, ds_child, join_columns=c("letter", "index"))

match_statistics <- function(d_parent, d_child, join_columns) {
  checkmate::assert_data_frame(d_parent, null.ok = FALSE)
  checkmate::assert_data_frame(d_child , null.ok = FALSE)
  checkmate::assert_character(join_columns, min.len=1, any.missing=FALSE)

  if (is.null(names(join_columns))) {
    flipped_join_columns <- join_columns
  } else {
    flipped_join_columns <- names(join_columns)
    names(flipped_join_columns) <- join_columns
    flipped_join_columns <- ifelse(nchar(flipped_join_columns) == 0L, names(flipped_join_columns), flipped_join_columns)
  }

  for (i in seq_along(join_columns)) {
    if (!(flipped_join_columns[i] %in% colnames(d_parent)))
      base::stop("The variable `", flipped_join_columns[i], "` is not found in the parent table passed to `OuhscMunge::match_statistics()`.")
    if (!(join_columns[i] %in% colnames(d_child)))
      base::stop("The variable `", join_columns[i], "` is not found in the child table passed to `OuhscMunge::match_statistics()`.")
  }

  parent_in_child            <- nrow(dplyr::semi_join(d_parent, d_child, by = join_columns))
  parent_not_in_child        <- nrow(dplyr::anti_join(d_parent, d_child, by = join_columns))
  deadbeat_proportion        <- parent_not_in_child / nrow(d_parent)
  parent_na_any              <- sum(apply(dplyr::select(d_parent, !! flipped_join_columns), MARGIN = 1, FUN = function(x) any(is.na(x))))

  child_in_parent            <- nrow(dplyr::semi_join(d_child, d_parent, by = flipped_join_columns))
  child_not_in_parent        <- nrow(dplyr::anti_join(d_child, d_parent, by = flipped_join_columns))
  orphan_proportion          <- child_not_in_parent / nrow(d_child)
  child_na_any               <- sum(apply(dplyr::select(d_child, !! join_columns), MARGIN = 1, FUN = function(x) any(is.na(x))))

  parent <- c(parent_in_child = parent_in_child, parent_not_in_child = parent_not_in_child, parent_na_any = parent_na_any, deadbeat_proportion  = deadbeat_proportion )
  child  <- c(child_in_parent = child_in_parent, child_not_in_parent = child_not_in_parent, child_na_any  = child_na_any , orphan_proportion    = orphan_proportion   )

  c(parent, child)
}

match_statistics_display <- function(d_parent, d_child, join_columns) {
  # No need to check parameters, because `match_statistics()` does it.

  m <- match_statistics(d_parent, d_child, join_columns)
  l <- list()

  l$parent_in_child             <- format(m["parent_in_child"]       , big.mark = ",")
  l$parent_not_in_child         <- format(m["parent_not_in_child"]   , big.mark = ",")
  l$parent_na_any               <- format(m["parent_na_any"]         , big.mark = ",")
  l$child_in_parent             <- format(m["child_in_parent"]       , big.mark = ",")
  l$child_not_in_parent         <- format(m["child_not_in_parent"]   , big.mark = ",")
  l$child_na_any                <- format(m["child_na_any"]          , big.mark = ",")
  l$parent_in_child             <- format(m["parent_in_child"]       , big.mark = ",")

  l$deadbeat_proportion         <- sprintf("%0.4f%%", m["deadbeat_proportion"]* 100)
  l$orphan_proportion           <- sprintf("%0.4f%%", m["orphan_proportion"]  * 100)

  d <- tibble::tibble(
      key   = gsub("_", " ", names(l)),
      value = as.character(l)
    ) %>%
    dplyr::mutate(
      key_width_max   = max(nchar(.data$key)),
      value_width_max = max(nchar(.data$value))
    )

  paste0(
    "\n\nMatch stats for `", deparse(substitute(d_parent)), "` vs `", deparse(substitute(d_child)), "` on column(s): ", as.character(deparse(substitute(join_columns))), ".\n",
    paste(
      sprintf("| %-*s | %*s |", d$key_width_max, d$key, d$value_width_max, d$value),
      collapse = "\n"
    ),
    "\n"
  )
}
