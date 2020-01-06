library(testthat)

# ---- match_statistics --------------------------------------------------------
context("match_statistics")

ds_parent <- data.frame(
  parent_id         = 11L:20L,
  letter            = rep(letters[1:5], each = 2),
  index             = rep(1:2, times = 5),
  dv                = runif(10),
  stringsAsFactors  = FALSE
)
ds_child <- data.frame(
  child_id          = 101:140,
  parent_id         = c(14, 15, rep(16L:24L, each = 4), 25, 26),
  letter            = rep(letters[3:12], each = 4),
  index             = rep(1:2, each = 2, length.out = 40),
  dv                = runif(40),
  stringsAsFactors  = FALSE
)

names_returned <- c(
  "parent_in_child", "parent_not_in_child", "parent_na_any", "deadbeat_proportion",
  "child_in_parent", "child_not_in_parent", "child_na_any" , "orphan_proportion"
)

test_that("match_statistics -one column", {
  expected <- structure(c(7, 3, 0, 0.3, 22, 18, 0, 0.45), .Names = names_returned)
  observed <- match_statistics(ds_parent, ds_child, join_columns = "parent_id") #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -two columns", {
  expected <- structure(c(6, 4, 0, 0.4, 12, 28, 0, 0.7), .Names = names_returned)
  observed <- match_statistics(ds_parent, ds_child, join_columns = c("letter", "index")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -one column w/ 1 different name", {
  d_c <- dplyr::rename(ds_child, ParentID = parent_id)

  expected <- structure(c(7, 3, 0, 0.3, 22, 18, 0, 0.45), .Names = names_returned)
  observed <- match_statistics(ds_parent, d_c, join_columns = c("parent_id" = "ParentID")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -two columns w/ 1 different name", {
  d_c <- dplyr::rename(ds_child, Letter = letter)

  expected <- structure(c(6, 4, 0, 0.4, 12, 28, 0, 0.7), .Names = names_returned)
  observed <- match_statistics(ds_parent, d_c, join_columns = c("letter" = "Letter", "index")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -two columns w/ 2 different names", {
  d_c <- dplyr::rename(ds_child, Letter = letter, Index = index)

  expected <- structure(c(6, 4, 0, 0.4, 12, 28, 0, 0.7), .Names = names_returned)
  observed <- match_statistics(ds_parent, d_c, join_columns = c("letter" = "Letter", "index" = "Index")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -bad parent name", {
  expect_error(
    match_statistics(ds_parent, ds_child, join_columns = c("BAD" = "Letter", "index" = "Index"))
    , "The variable `BAD` is not found in the parent table passed to `OuhscMunge::match_statistics\\(\\)`"
  )
})

test_that("match_statistics -bad child name", {
  d_c <- dplyr::rename(ds_child, Letter = letter, Index = index)
  expect_error(
    match_statistics(ds_parent, d_c, join_columns = c("letter" = "Letter", "index" = "BAD"))
    , "The variable `BAD` is not found in the child table passed to `OuhscMunge::match_statistics\\(\\)`"
  )
})

test_that("NAs in single join columns", {
  d_p <- ds_parent
  d_p[c(2,5,7), ]$parent_id <- NA_integer_

  d_c <- ds_child
  d_c[c(20:29), ]$parent_id <- NA_integer_

  expected <- structure(c(8, 2, 3, 0.2, 24, 16, 10, 0.4), .Names = names_returned)
  observed <- match_statistics(d_p, d_c, join_columns = c("parent_id")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("NAs in double join columns", {
  d_p <- ds_parent
  d_p[c(2, 5, 7   ), ]$letter <- NA_integer_
  d_p[c(   5, 7, 8), ]$index  <- NA_integer_

  d_c <- ds_child
  d_c[20:29, ]$letter <- NA_integer_
  d_c[25:34, ]$letter <- NA_integer_

  expected <- structure(c(4, 6, 4, 0.6, 13, 27, 15, 0.675), .Names = names_returned)
  observed <- match_statistics(d_p, d_c, join_columns = c("letter", "index")) # dput(observed)
  expect_equal(observed, expected)
})
