library(testthat)

test_that("Smoke test w/o update", {
  path <- base::file.path(devtools::inst(name = "OuhscMunge"), "package-dependency-list.csv")
  OuhscMunge:::package_janitor_remote(url_package_dependencies = path, update_packages = FALSE, verbose = TRUE)
})
