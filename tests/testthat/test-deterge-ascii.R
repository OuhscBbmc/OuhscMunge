library(testthat)

# ---- deterge_to_ascii --------------------------------------------------------
context("deterge_to_ascii")

test_that("SEM founders --vanilla", {
  x         <- c("Ekstrom", "Joreskog", "bisschen Zurcher")
  expected  <- c("Ekstrom", "Joreskog", "bisschen Zurcher")
  observed  <- deterge_to_ascii(x)

  expect_equal(observed, expected)
})

test_that("SEM founders --normal", {
  skip("I need to find a way that works consistently across different OSes.")

  x         <- c("Ekstr\xf8m", "J\xf6reskog", "bi\xdfchen Z\xfcrcher")
  expected  <- c("Ekstrom", "Joreskog", "bisschen Zurcher")
  observed  <- deterge_to_ascii(x)

  expect_equal(observed, expected)
})

test_that("Russian --normal", {
  skip("I need to find a way that works consistently across different OSes.")

  # x <- "от сотрудницы"
  x <- c(
    "от сотрудницы",
    "мама и сестра",
    "подруга по общежитию"
  )
  expected <- c(
    "D 3/4 N? N?D 3/4 N?N?N?D'D 1/2 D,N?N?",
    "D 1/4 D?D 1/4 D? D, N?DuN?N?N?D?",
    "D?D 3/4 D'N?N?D3D? D?D 3/4  D 3/4 D+-N?DuD?D,N?D,N?"
  )

  observed <- deterge_to_ascii(x) # dput(observed)

  expect_equal(observed, expected)
})



# })
# test_that("deterge double --bad upper bound length", {
#   expect_error(
#     OuhscMunge::deterge_to_double(5:40, bound_upper = 12:23)
#     , regexp = "The parameter `bound_upper` must be a numeric or integer vector with exactly one element."
#   )
# })
