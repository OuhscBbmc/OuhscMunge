library(testthat)

test_that("data_frame_test_uniqueness", {
  expect_false(data_frame_test_uniqueness(mtcars, c("cyl"))                       )
  expect_false(data_frame_test_uniqueness(mtcars, c("cyl", "vs"))                 )
  expect_false(data_frame_test_uniqueness(mtcars, c("cyl", "hp"))                 )
  expect_false(data_frame_test_uniqueness(mtcars, c("cyl", "hp"), display_count=0))
  expect_true( data_frame_test_uniqueness(mtcars, c("mpg", "wt"))                 )
})
