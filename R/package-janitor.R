#'  This code checks the user's installed packages against the packages listed in
#'    https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/master/utility/package-dependency-list.csv
#'    These packages are necessary for most of the analyses run by the OUHSC BBMC  (https://github.com/OuhscBbmc).
#'
#'  CRAN packages are installed only if they're not already; then they're updated if available.
#'     GitHub packages are installed regardless if they're already installed.
#'
#'  If anyone encounters a package that should be on there, please add it to
#'   https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/master/utility/package-dependency-list.csv
#'
#'  There are two identical versions of this file.  If in doubt, use the first option.
#'     1. Stand-alone GitHub Gist: https://gist.github.com/wibeasley/2c5e7459b88ec28b9e8fa0c695b15ee3
#'     2. R package on GitHub repo: https://github.com/OuhscBbmc/OuhscMunge/blob/master/R/package-janitor.R
#'
#'  To run this function on your local machine with the following three lines of code:
#'     if( !base::requireNamespace("devtools") ) utils::install.packages("devtools")
#'     devtools::source_gist("2c5e7459b88ec28b9e8fa0c695b15ee3", filename="package-janitor-bbmc.R")
#'     package_janitor_remote("https://raw.githubusercontent.com/OuhscBbmc/RedcapExamplesAndPatterns/master/utility/package-dependency-list.csv")

#' @name package_janitor_remote
#' @export
#'
#' @title checks the user's installed packages against the packages listed in a CSV.
#'
#' @description CRAN packages are installed only if they're not already; then they're updated if available.
#' GitHub packages are installed regardless if they're already installed.
#' These packages are necessary for most of the analyses run by the OUHSC BBMC  (https://github.com/OuhscBbmc).
#'
#' We use https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/master/utility/package-dependency-list.csv.
#' The undecorated version of this csv (which is better for computers, but harder for humans) is
#' https://raw.githubusercontent.com/OuhscBbmc/RedcapExamplesAndPatterns/master/utility/package-dependency-list.csv.
#'
#' @param url_package_dependencies path to a csv containing the packages.  See the description.  Required.
#' @param cran_repo path to a CRAN mirror.
#'
#' @param update_packages should package be updated first.
#' @param dependencies Passed to the `dependencies` parameter of [`utils::install.packages()`]).  Set to `NA` to avoid 'Suggests'.
#' @param check_xml_linux display a message about the xml Linux package.
#' @param check_libcurl_linux display a message about the libcurl Linux package.
#' @param check_openssl_linux display a message about the openssl Linux package.
#' @param verbose print messages to the console (or wherever messages are being directed).
#'
#' @importFrom utils installed.packages
#'
#' @author Will Beasley
#'
#' @examples
#' \dontrun{
#' # This path works if the working directory is the root of the repo:
#' # https://github.com/OuhscBbmc/RedcapExamplesAndPatterns
#' package_janitor_remote("./utility/package-dependency-list.csv")
#'
#' # Internet URLs are also accepted.
#' # Caution, this one takes at least 5 minutes.
#' url <- paste0(
#'   "https://raw.githubusercontent.com/OuhscBbmc/RedcapExamplesAndPatterns/",
#'   "master/utility/package-dependency-list.csv"
#' )
#' package_janitor_remote(url)
#' }


package_janitor_remote <- function(
  url_package_dependencies,
  cran_repo                    = "https://cran.rstudio.com",
  update_packages              = TRUE,
  dependencies                 = TRUE,
  check_xml_linux              = (R.Version()$os == "linux-gnu"),
  check_libcurl_linux          = (R.Version()$os == "linux-gnu"),
  check_openssl_linux          = (R.Version()$os == "linux-gnu"),
  verbose                      = TRUE
) {

  # if( !file.exists(url_package_dependencies) )
  #   base::stop("The path `", url_package_dependencies, "` was not found.  Make sure the working directory is set to the root of the repository.")

  # ---- load-sources ------------------------------------------------------------

  # ---- load-packages -----------------------------------------------------------

  # ---- declare-globals ---------------------------------------------------------
  required_columns <- c("package_name", "on_cran", "install", "github_username", "description")

  # ---- load-data ---------------------------------------------------------------
  if (verbose)
    message("package_janitor is loading the list of package dependencies.")


  if (!base::requireNamespace("checkmate"))
    utils::install.packages("checkmate", repos = cran_repo) # nocov

  checkmate::assert_character(url_package_dependencies, min.chars = 1, len=1)

  ds_packages <- utils::read.csv(
    file = url_package_dependencies,
    stringsAsFactors = FALSE
  )

  rm(url_package_dependencies)


  # ---- tweak-data --------------------------------------------------------------
  missing_columns <- base::setdiff(required_columns, colnames(ds_packages))
  if( length(missing_columns) > 0 )
    stop(paste("The data.frame of the required packages is missing the following columns:", missing_columns))

  ds_install_from_cran    <- ds_packages[ds_packages$install &  ds_packages$on_cran, ]
  ds_install_from_github  <- ds_packages[ds_packages$install & !ds_packages$on_cran & !is.na(ds_packages$github_username) & nchar(ds_packages$github_username)>0, ]

  rm(ds_packages)


  # ---- cran-packages -----------------------------------------------------------
  if (verbose)
    message("package_janitor is updating the existing packages from CRAN.")
  if (update_packages)
    utils::update.packages(ask = FALSE, checkBuilt = TRUE, repos = cran_repo) # nocov


  # ---- install-devtools --------------------------------------------------------
  if (verbose)
    message("package_janitor is installing the the `devtools` and `httr` packages from CRAN if necessary.")

  if (!base::requireNamespace("httr"))
    utils::install.packages("httr", repos = cran_repo) # nocov

  if (!base::requireNamespace("devtools"))
    utils::install.packages("devtools", repos = cran_repo) # nocov


  # ---- install-cran-packages ---------------------------------------------------
  if (verbose)
    message("package_janitor is installing the CRAN packages:")

  for (package_name in ds_install_from_cran$package_name) {
    if (package_name == "devtools") {
      if (verbose) # nocov
        message("\nThe `devtools` package does not need to be in the list of package dependencies.  It's updated automatically.")  # nocov

    } else if (package_name == "httr") {
      if (verbose) # nocov
        message("\nThe `httr` package does not need to be in the list of package dependencies.  It's updated automatically.") # nocov

    } else {
      available <- base::requireNamespace(package_name, quietly = TRUE) # Checks if it's available
      if (!available) {
        if (verbose) message("\nInstalling `", package_name, "` from CRAN, including its dependencies.")
        utils::install.packages(package_name, dependencies = dependencies, repos = cran_repo)

      } else if (update_packages) {
        if (verbose) message("\n`", package_name, "` exists, and verifying it's dependencies are installed too.")

        # Make sure all their dependencies are installed & up-to-date
        need_to_install <- remotes::package_deps(package_name, dependencies = dependencies)$package
        if (verbose)
          message("Package `", package_name, "` has ", length(need_to_install), " dependencies: ", paste(need_to_install, collapse = ", "), ".")

        remotes::update_packages(need_to_install, repos = cran_repo)
      }
      base::rm(available)
    }
  }

  rm(ds_install_from_cran, package_name)
  if (verbose)
    message("\n")

  # ----check-linux-xml ---------------------------------------------------------
  #http://stackoverflow.com/questions/7765429/unable-to-install-r-package-in-ubuntu-11-04

  if (check_xml_linux) {
    xml_results <- length(base::system2("locate", "r-cran-xml", stdout = TRUE))
    xml_missing <- (xml_results == 0L)

    if (xml_missing)
      base::warning(
        "This Linux machine is possibly missing the 'r-cran-xml' library.  ",
        "Consider running `sudo apt-get install r-cran-xml` ",
        "or the equivalent for your distribution."
      )

    base::rm(xml_results, xml_missing)
  }


  # ---- check-linux-libcurl -----------------------------------------------------
  if (check_libcurl_linux) {
    libcurl_results <- length(base::system2("locate", "libcurl4", stdout = TRUE))
    libcurl_missing <- (libcurl_results == 0L)

    if (libcurl_missing)
      base::warning(
        "This Linux machine is possibly missing the 'libcurl' library.  ",
        "Consider running `sudo apt-get install libcurl4-openssl-dev` ",
        "or the equivalent for your distribution."
      )

    base::rm(libcurl_results, libcurl_missing)
  }


  # ---- check-linux-openssl -----------------------------------------------------
  if (check_openssl_linux) {
    openssl_results <- length(base::system2("locate", "libssl-dev", stdout = TRUE))
    openssl_missing <- (openssl_results == 0L)

    if (openssl_missing)
      base::warning(
        "This Linux machine is possibly missing the 'libssl' library.  ",
        "Consider running `sudo apt-get install libssl-dev` ",
        "or the equivalent for your distribution."
      )

    base::rm(openssl_results, openssl_missing)
  }


  #---- install-github-packages -------------------------------------------------
  if (verbose)
    message("\npackage_janitor is installing the GitHub packages:")

  for (i in base::seq_len(base::nrow(ds_install_from_github))) {
    package_name <- ds_install_from_github$package_name[i]
    if (verbose) message("Installing `", package_name, "` from GitHub, (not including its dependencies).")

    username        <- ds_install_from_github$github_username[i]
    repository_name <- paste0(username, "/", package_name)
    devtools::install_github(repo = repository_name)
    base::rm(package_name, username, repository_name)
  }

  base::rm(ds_install_from_github, i)

  # ---- notify-tinytex ---------------------------------------------------------
  if (any(installed.packages() == "tinytex")) {
    # This comparison is copied from tinytex:::is_tinytex().
    if (!(gsub("^[.]", "", tolower(basename(tinytex::tinytex_root()))) == "tinytex"))
      tinytex::install_tinytex() # nocov
      # message("If you haven't already, install the TeX part of tinytex with `tinytex::install_tinytex()`.")
  }

  # ---- return ---------------------------------------------------------

  if (verbose)
    message("package_janitor is complete.")
}
