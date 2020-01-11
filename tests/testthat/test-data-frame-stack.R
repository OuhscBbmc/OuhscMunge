library(testthat)
# ---- metdata-update ----------------------------------------------------------
test_that("metadata_update_file", {
  path_temp       <- tempfile(fileext = ".csv")
  on.exit(unlink(path_temp))
  file.copy(
    system.file("test-data/metadata-original.csv", package = "OuhscMunge"),
    path_temp
  )

  ds_original <- readr::read_csv(path_temp)
  expect_equal(3, nrow(ds_original))

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5),
    x2  = c(letters[1:5], "x", "y"),
    x3  = c(11, 12, 13, 14, 15, 11, 15)
  )

  metadata_update_file(
    path_temp,
    dplyr::mutate(ds_current, x1 = as.character(x1), x3 = as.character(x3)),
    c("x1", "x2")
  )

  # Displays 7 rows.
  ds_new <- readr::read_csv(path_temp)
  expect_equal(7, nrow(ds_new))
})


# ---- data_frame_stack_new ----------------------------------------------------
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

  ds_actual <- data_frame_stack_new(ds_original, ds_current, c("x1", "x2"))
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

  ds_actual <- data_frame_stack_new(ds_original, ds_current, c("x1", "x2"))
  expect_equal(ds_actual, ds_expected)
})


# ---- throw-errors ------------------------------------------------------------
test_that("nonunique original", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 3),
    x2  = letters[c(1, 3, 3)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5),
    x2  = c(letters[1:5], "x", "y"),
    x3  = c(11, 12, 13, 14, 15, 11, 15)
  )

  expect_error(
    data_frame_stack_new(ds_original, ds_current, c("x1", "x2"))
    , "The `d_original` data\\.frame has multiple rows with the same values for column\\(s\\)\\\n\\{`x1`, `x2`\\}\\."
  )
})
test_that("nonunique current", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14)
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 2, 5),
    x2  = c(letters[1:5], "b", "y"),
    x3  = c(11, 12, 13, 14, 15, 11, 15)
  )

  expect_error(
    data_frame_stack_new(ds_original, ds_current, c("x1", "x2"))
    , "The `d_current` data\\.frame has multiple rows with the same values for column\\(s\\)\\\n\\{`x1`, `x2`\\}\\."
  )
})