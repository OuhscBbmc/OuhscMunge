#' Download and install dependencies
#'
#' When called in the repo of an R package, its package dependencies are inspected
#' and the obsolete ones are updated.  This function is a thin wrapper around
#' `remotes::update_packages(remotes::dev_package_deps()$package, dependencies=T)`.
#' Unlike the 'Update' button in RStudio's 'Packages' panel,
#' this function will (a) update from CRAN and remote sources like GitHub and
#' (b) not attempt to install local packages that are unrelated to the current package.
#'
#' @note
#' This function only works if run inside a valid package.  It reads the dependencies enumerated in the package's
#' [DESCRIPTION](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file) file.
#'
#' @export
update_packages_addin <- function() {
  # nocov start
  dependency_list <- remotes::dev_package_deps()
  remotes::update_packages(
    packages      = dependency_list$package,
    dependencies  = TRUE
  )
  # nocov end
}
