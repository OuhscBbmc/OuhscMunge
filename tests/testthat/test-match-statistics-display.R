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
  expected <- "\n\nMatch stats for `ds_parent` vs `ds_child` on column(s): \"parent_id\".\n| parent in child     |        7 |\n| parent not in child |        3 |\n| parent na any       |        0 |\n| child in parent     |       22 |\n| child not in parent |       18 |\n| child na any        |        0 |\n| deadbeat proportion | 30.0000% |\n| orphan proportion   | 45.0000% |\n"
  observed <- match_statistics_display(ds_parent, ds_child, join_columns = "parent_id") #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -two columns", {
  expected <- "\n\nMatch stats for `ds_parent` vs `ds_child` on column(s): c(\"letter\", \"index\").\n| parent in child     |        6 |\n| parent not in child |        4 |\n| parent na any       |        0 |\n| child in parent     |       12 |\n| child not in parent |       28 |\n| child na any        |        0 |\n| deadbeat proportion | 40.0000% |\n| orphan proportion   | 70.0000% |\n"
  observed <- match_statistics_display(ds_parent, ds_child, join_columns = c("letter", "index")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -one column w/ 1 different name", {
  d_c <- dplyr::rename(ds_child, ParentID = parent_id)

  expected <- "\n\nMatch stats for `ds_parent` vs `d_c` on column(s): c(parent_id = \"ParentID\").\n| parent in child     |        7 |\n| parent not in child |        3 |\n| parent na any       |        0 |\n| child in parent     |       22 |\n| child not in parent |       18 |\n| child na any        |        0 |\n| deadbeat proportion | 30.0000% |\n| orphan proportion   | 45.0000% |\n"
  observed <- match_statistics_display(ds_parent, d_c, join_columns = c("parent_id" = "ParentID")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -two columns w/ 1 different name", {
  d_c <- dplyr::rename(ds_child, Letter = letter)

  expected <- "\n\nMatch stats for `ds_parent` vs `d_c` on column(s): c(letter = \"Letter\", \"index\").\n| parent in child     |        6 |\n| parent not in child |        4 |\n| parent na any       |        0 |\n| child in parent     |       12 |\n| child not in parent |       28 |\n| child na any        |        0 |\n| deadbeat proportion | 40.0000% |\n| orphan proportion   | 70.0000% |\n"
  observed <- match_statistics_display(ds_parent, d_c, join_columns = c("letter" = "Letter", "index")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -two columns w/ 2 different names", {
  d_c <- dplyr::rename(ds_child, Letter = letter, Index = index)

  expected <- "\n\nMatch stats for `ds_parent` vs `d_c` on column(s): c(letter = \"Letter\", index = \"Index\").\n| parent in child     |        6 |\n| parent not in child |        4 |\n| parent na any       |        0 |\n| child in parent     |       12 |\n| child not in parent |       28 |\n| child na any        |        0 |\n| deadbeat proportion | 40.0000% |\n| orphan proportion   | 70.0000% |\n"
  observed <- match_statistics_display(ds_parent, d_c, join_columns = c("letter" = "Letter", "index" = "Index")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("match_statistics -bad parent name", {
  expect_error(
    match_statistics_display(ds_parent, ds_child, join_columns = c("BAD" = "Letter", "index" = "Index"))
    , 'The variable `BAD` is not found in the parent table passed to `OuhscMunge::match_statistics\\(\\)`'
  )
})

test_that("match_statistics -bad child name", {
  d_c <- dplyr::rename(ds_child, Letter = letter, Index = index)
  expect_error(
    match_statistics_display(ds_parent, d_c, join_columns = c("letter" = "Letter", "index" = "BAD"))
    , 'The variable `BAD` is not found in the child table passed to `OuhscMunge::match_statistics\\(\\)`'
  )
})

test_that("NAs in single join columns", {
  d_p <- ds_parent
  d_p[c(2,5,7), ]$parent_id <- NA_integer_

  d_c <- ds_child
  d_c[c(20:29), ]$parent_id <- NA_integer_

  expected <-  "\n\nMatch stats for `d_p` vs `d_c` on column(s): c(\"parent_id\").\n| parent in child     |        8 |\n| parent not in child |        2 |\n| parent na any       |        3 |\n| child in parent     |       24 |\n| child not in parent |       16 |\n| child na any        |       10 |\n| deadbeat proportion | 20.0000% |\n| orphan proportion   | 40.0000% |\n"
  observed <- match_statistics_display(d_p, d_c, join_columns = c("parent_id")) #dput(observed)
  expect_equal(observed, expected)
})

test_that("NAs in double join columns", {
  d_p <- ds_parent
  d_p[c(2,5,7  ), ]$letter <- NA_integer_
  d_p[c(  5,7,8), ]$index <- NA_integer_

  d_c <- ds_child
  d_c[c(20:29), ]$letter <- NA_integer_
  d_c[c(25:34), ]$letter <- NA_integer_

  expected <- "\n\nMatch stats for `d_p` vs `d_c` on column(s): c(\"letter\", \"index\").\n| parent in child     |        4 |\n| parent not in child |        6 |\n| parent na any       |        4 |\n| child in parent     |       13 |\n| child not in parent |       27 |\n| child na any        |       15 |\n| deadbeat proportion | 60.0000% |\n| orphan proportion   | 67.5000% |\n"
  observed <- match_statistics_display(d_p, d_c, join_columns = c("letter", "index")) #dput(observed)
  expect_equal(observed, expected)
})
