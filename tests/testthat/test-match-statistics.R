library(testthat)

###########
context("match_statistics")
###########

ds_parent <- data.frame(
  parent_id         = 1L:10L,
  letter            = rep(letters[1:5], each=2),
  index             = rep(1:2, times=5),
  dv                = runif(10),
  stringsAsFactors  = F
)
ds_child <- data.frame(
  child_id          = 101:140,
  parent_id         = c(4, 5, rep(6L:14L, each=4), 15, 16),
  letter            = rep(letters[3:12], each=4),
  index             = rep(1:2, each=2, length.out=40),
  dv                = runif(40),
  stringsAsFactors  = F
)

names_returned <- c(
  "parent_in_child", "parent_not_in_child", "deadbeat_proportion", 
  "child_in_parent", "child_not_in_parent", "orphan_proportion"
)

# ds_child
# ds_parent

test_that("match_statistics -one column", {
  expected <- structure(c(7, 3, 0.3, 22, 18, 0.45), .Names = names_returned)
  observed <- match_statistics(ds_parent, ds_child, join_columns="parent_id") #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -two columns", {
  expected <- structure(c(6, 4, 0.4, 12, 28, 0.7), .Names = names_returned)
  observed <- match_statistics(ds_parent, ds_child, join_columns=c("letter", "index")) #dput(observed)
  expect_equal(observed, expected)
})
