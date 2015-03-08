library(devtools)
devtools::create(  path=file.path(getwd(), "seed"),check=TRUE, description=list(
  "Title" = "Data manipulation operations",
  "Description" = "Data manipulation operations frequently used in OUHSC BBMC projects.",
  "Date" = "2014-03-08",
  "Author" = "Will Beasley",
  "Maintainer" = "'Will Beasley' <wibeasley@hotmail.com> "
))

use_travis(pkg = ".")
use_testthat(pkg = ".")
use_rstudio(pkg = ".")
use_vignette(name, pkg = ".")
use_rcpp(pkg = ".")
use_appveyor(pkg = ".")
use_package_doc(pkg = ".")
use_revdep(pkg = ".")
use_cran_comments(pkg = ".")