#' Download and install dependencies
#'
#' When called in the repo of an R package, its package dependencies are inspected
#' and the obsolete ones are updated.  This function is a thin wrapper around
#' \code{stats::update(remotes::dev_package_deps())}.  Unlike the 'Update' button in RStudio's 'Packages' panel,
#' this function will (a) update from CRAN and remote sources like GitHub and
#' (b) not attempt to install local packages that are unrelated to the current package.
#'
#' @export
update_packages_addin <- function() {
  stats::update(remotes::dev_package_deps())
}
