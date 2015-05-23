library(testthat)

###########
context("Package Janitor")
###########

test_that("Smoke Test", {
  path <- base::file.path(devtools::inst(name="OuhscMunge"), "package_dependency_list.csv")
  OuhscMunge:::package_janitor(path_csv=path)
})
