library(testthat)


# ---- good-values -------------------------------------------------------------
test_that("four new rows", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5),
    x2  = c(letters[1:5], "x", "y"),
    x3  = c(11, 12, 13, 14, 15, 11, 15)
  )

  expect_true(data_frame_compare_structure(ds_original, ds_current))
})
test_that("drop timestamp", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14),
    datestamp = Sys.Date()
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5),
    x2  = c(letters[1:5], "x", "y"),
    x3  = c(11, 12, 13, 14, 15, 11, 15)
  )

  expect_true(data_frame_compare_structure(ds_original, ds_current, ignore_datestamp = TRUE))
})

test_that("zero new rows --shuffled order", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(3, 1, 4),
    x2  = letters[c(3, 1, 4)],
    x3  = c(13, 11, 14)
  )

  expect_true(data_frame_compare_structure(ds_original, ds_current))
})


# ---- throw-errors ------------------------------------------------------------
test_that("different column count", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5),
    x2  = c(letters[1:5], "x", "y")
  )

  expect_error(
    data_frame_compare_structure(ds_original, ds_current)
    , "The two data\\.frames have different number of columns\\."
  )
})

test_that("different column names", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5),
    x2  = c(letters[1:5], "x", "y"),
    zz  = c(11, 12, 13, 14, 15, 11, 15)
  )

  expect_error(
    data_frame_compare_structure(ds_original, ds_current)
    , "The two data\\.frames have different column names\\."
  )
})

test_that("different column classes", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1L, 5L),
    x2  = c(letters[1:5], "x", "y"),
    x3  = c(11, 12, 13, 14, 15, 11, 15)
  )

  expect_error(
    data_frame_compare_structure(ds_original, ds_current)
    , "The two data\\.frames have different column classes\\."
  )
})
