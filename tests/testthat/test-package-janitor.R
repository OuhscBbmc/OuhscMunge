library(testthat)

test_that("Smoke test w/o update", {
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
})
