library(testthat)

test_that("Smoke test w/o update", {
  testthat::skip("Test doesn't work when called from R CMD Check")
  withr::local_envvar(R_USER_CACHE_DIR = tempdir())

  path <- system.file( # See ?pkgload::system.file
    "package-dependency-list.csv",
    package   = "OuhscMunge",
    mustWork  = TRUE
  )

  OuhscMunge:::package_janitor_remote(
    url_package_dependencies  = path,
    update_packages           = FALSE,
    verbose                   = TRUE
  )

  testthat::succeed("Smoke test passed")
})
