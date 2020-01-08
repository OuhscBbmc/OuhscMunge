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

  ds_expected <- tibble::tribble(
    ~x1, ~x2, ~x3,
      1, "a",  11,
      2, "b",  12,
      3, "c",  13,
      4, "d",  14,
      5, "e",  15,
      1, "x",  11,
      5, "y",  15
  )

  ds_actual <- data_frame_stack(ds_original, ds_current, c("x1", "x2"))
  expect_equal(ds_actual, ds_expected)
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

  ds_expected <- ds_original

  ds_actual <- data_frame_stack(ds_original, ds_current, c("x1", "x2"))
  expect_equal(ds_actual, ds_expected)
})

