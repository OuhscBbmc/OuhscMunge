library(testthat)

###########
context("Package Janitor")
###########

test_that("Smoke test w/o update", {
  path <- base::file.path(devtools::inst(name="OuhscMunge"), "package_dependency_list.csv")
  OuhscMunge:::package_janitor(path_package_dependencies=path, update_packages=FALSE)
})
