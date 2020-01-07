library(testthat)


# ---- metadata-se ----------------------------------------------------------------
test_that("four new rows", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5, 5),
    x2  = c(letters[1:5], "a", "e", "e"),
    x3  = c(11, 12, 13, 14, 15, 11, 15, 17)
  )

  ds_expected <- tibble::tibble(
    x1  = c(2, 5, 5, 5),
    x2  = c("b", "e", "e", "e"),
    x3  = c(12, 15, 15, 17)
  )

  ds_actual <- data_frame_compare(ds_original, ds_current, c("x1", "x2"))

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

  ds_expected <- tibble::tibble(
    x1  = double(0),
    x2  = character(0),
    x3  = double(0)
  )

  ds_actual <- data_frame_compare(ds_original, ds_current, c("x1", "x2"))

  expect_equal(ds_actual, ds_expected)
})

