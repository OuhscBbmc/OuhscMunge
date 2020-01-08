library(testthat)

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

  expect_true(data_frame_compare_structure(ds_original, ds_current, c("x1", "x2")))
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

  expect_true(data_frame_compare_structure(ds_original, ds_current, c("x1", "x2")))
})
