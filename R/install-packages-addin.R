#' Install important packages
#'
#' Installs the [important packages](https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/master/utility/package-dependency-list.csv)
#' typically used by [BBMC](https://github.com/OuhscBbmc) data analysts.
#'
#' @export
install_packages_addin <- function() {
  package_janitor_remote <- NULL   # This will be overwritten by the gist.

  if (!base::requireNamespace("devtools")) utils::install.packages("devtools")
  devtools::source_gist("2c5e7459b88ec28b9e8fa0c695b15ee3", filename = "package-janitor-bbmc.R")
  package_janitor_remote("https://raw.githubusercontent.com/OuhscBbmc/RedcapExamplesAndPatterns/master/utility/package-dependency-list.csv")
}
