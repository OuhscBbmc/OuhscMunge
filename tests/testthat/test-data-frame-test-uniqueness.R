library(testthat)

test_that("data_frame_uniqueness_test", {
  expect_false(data_frame_uniqueness_test(mtcars, c("cyl"))                       )
  expect_false(data_frame_uniqueness_test(mtcars, c("cyl", "vs"))                 )
  expect_false(data_frame_uniqueness_test(mtcars, c("cyl", "hp"))                 )
  expect_false(data_frame_uniqueness_test(mtcars, c("cyl", "hp"), display_count=0))
  expect_true( data_frame_uniqueness_test(mtcars, c("mpg", "wt"))                 )
})

test_that("data_frame_uniqueness_assert", {
  expect_error(data_frame_uniqueness_assert(mtcars, c("cyl"))                       )
  expect_error(data_frame_uniqueness_assert(mtcars, c("cyl", "vs"))                 )
  expect_error(data_frame_uniqueness_assert(mtcars, c("cyl", "hp"))                 )
  expect_error(data_frame_uniqueness_assert(mtcars, c("cyl", "hp"), display_count=0))
  data_frame_uniqueness_assert(mtcars, c("mpg", "wt"))
})
