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
test_that("metadata_update_file-with-datestamp", {
  path_temp       <- tempfile(fileext = ".csv")
  on.exit(unlink(path_temp))
  file.copy(
    system.file("test-data/metadata-original-with-datestamp.csv", package = "OuhscMunge"),
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
    c("x1", "x2"),
    datestamp_update = TRUE
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
test_that("with datestamp", {
  ds_original <- tibble::tibble(
    x1  = c(1, 3, 4),
    x2  = letters[c(1, 3, 4)],
    x3  = c(11, 13, 14),
    datestamp = as.Date("2020-01-07")
  )

  ds_current <- tibble::tibble(
    x1  = c(1:5, 1, 5),
    x2  = c(letters[1:5], "x", "y"),
    x3  = c(11, 12, 13, 14, 15, 11, 15)
  )

  ds_expected <- tibble::tribble(
    ~x1, ~x2, ~x3, ~datestamp,
    1, "a",  11,   as.Date("2020-01-07"),
    2, "b",  12,   Sys.Date(),
    3, "c",  13,   as.Date("2020-01-07"),
    4, "d",  14,   as.Date("2020-01-07"),
    5, "e",  15,   Sys.Date(),
    1, "x",  11,   Sys.Date(),
    5, "y",  15,   Sys.Date()
  )

  ds_actual <- data_frame_stack_new(ds_original, ds_current, c("x1", "x2"), datestamp_update = TRUE)
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

# ---- throw-errors-metadata_update_file ---------------------------------------

# ---- throw-errors-data_frame_stack_new ---------------------------------------
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
test_that("missing datestamp in original", {
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

  expect_error(
    data_frame_stack_new(ds_original, ds_current, c("x1", "x2"), datestamp_update = TRUE)
    , "If `datestamp_update` is true, then the data.frame `d_original` must have a `datestamp` colum\\. The column is allowed to contain missing values\\."
  )
})
