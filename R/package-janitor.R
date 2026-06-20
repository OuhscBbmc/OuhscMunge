#'  This code checks the user's installed packages against the packages listed in
#'    https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/main/utility/package-dependency-list.csv
#'    These packages are necessary for most of the analyses run by the OUHSC BBMC  (https://github.com/OuhscBbmc).
#'
#'  CRAN packages are installed only if they're not already; then they're updated if available.
#'     GitHub packages are installed regardless if they're already installed.
#'
#'  If anyone encounters a package that should be on there, please add it to
#'   https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/main/utility/package-dependency-list.csv
#'
#'  There are two identical versions of this file.  If in doubt, use the first option.
#'     1. Stand-alone GitHub Gist: https://gist.github.com/wibeasley/2c5e7459b88ec28b9e8fa0c695b15ee3
#'     2. R package on GitHub repo: https://github.com/OuhscBbmc/OuhscMunge/blob/main/R/package-janitor.R
#'
#'  To run this function on your local machine with the following three lines of code:
#'     if( !base::requireNamespace("devtools") ) utils::install.packages("devtools")
#'     devtools::source_gist("https://gist.github.com/wibeasley/2c5e7459b88ec28b9e8fa0c695b15ee3")
#'     package_janitor_remote("https://raw.githubusercontent.com/OuhscBbmc/RedcapExamplesAndPatterns/main/utility/package-dependency-list.csv")

#' @name package_janitor_remote
#' @export
#'
#' @title checks the user's installed packages against the packages listed in a CSV.
#'
#' @description CRAN packages are installed only if they're not already; then they're updated if available.
#' GitHub packages are installed regardless if they're already installed.
#' These packages are necessary for most of the analyses run by the OUHSC BBMC  (https://github.com/OuhscBbmc).
#'
#' We use https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/main/utility/package-dependency-list.csv.
#' The undecorated version of this csv (which is better for computers, but harder for humans) is
#' https://raw.githubusercontent.com/OuhscBbmc/RedcapExamplesAndPatterns/main/utility/package-dependency-list.csv.
#'
#' @param url_package_dependencies path to a csv containing the packages.  See the description.  Required.
#' @param cran_repo path to a CRAN mirror.
#'
#' @param update_packages should package be updated first.
#' @param dependencies Passed to the `dependencies` parameter of [`utils::install.packages()`]).  Set to `NA` to avoid 'Suggests'.
#' @param check_xml_linux display a message about the xml Linux package.
#' @param lib library location passed to [`utils::install.packages()`]). A value of `NULL` defaults to the first element of `.libPaths()`.
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
#'   "main/utility/package-dependency-list.csv"
#' )
#' package_janitor_remote(url)
#' }

package_janitor_remote <- function(
  url_package_dependencies,
  cran_repo                    = "https://cran.rstudio.com",
  update_packages              = TRUE,
  dependencies                 = TRUE,
  lib                          = NULL,
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
    utils::install.packages("checkmate", repos = cran_repo, lib = lib) # nocov

  checkmate::assert_character(url_package_dependencies, min.chars = 1, len=1)

  ds_packages <- utils::read.csv(
    file = url_package_dependencies,
    stringsAsFactors = FALSE
  )

  rm(url_package_dependencies)

  # ---- tweak-data --------------------------------------------------------------
  missing_columns <- base::setdiff(required_columns, colnames(ds_packages))
  if (1L <= length(missing_columns))
    stop(paste("The data.frame of the required packages is missing the following columns:", missing_columns))

  ds_install_from_cran    <- ds_packages[ds_packages$install &  ds_packages$on_cran, ]
  ds_install_from_github  <- ds_packages[ds_packages$install & !ds_packages$on_cran & !is.na(ds_packages$github_username) & nchar(ds_packages$github_username)>0, ]

  rm(ds_packages)

  # ---- cran-packages -----------------------------------------------------------
  if (verbose)
    message("package_janitor is updating the existing packages from CRAN.")
  if (update_packages)
    utils::update.packages(ask = FALSE, checkBuilt = TRUE, repos = cran_repo) # nocov

  # ---- install-pak -------------------------------------------------------------
  if (!base::requireNamespace("pak"))
    utils::install.packages("pak", repos = cran_repo, lib = lib) # nocov

  # ---- install-cran-packages ---------------------------------------------------
  if (verbose)
    message("package_janitor is installing the CRAN packages:")

  if (update_packages) {
    pak::pak(ds_install_from_cran$package_name, lib = lib)
  } else {
    to_install <- ds_install_from_cran$package_name[
      !vapply(ds_install_from_cran$package_name, base::requireNamespace, logical(1), quietly = TRUE)
    ]
    if (1L <= length(to_install))
      pak::pak(to_install, lib = lib)
  }

  rm(ds_install_from_cran)
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

  repository_names <- paste0(ds_install_from_github$github_username, "/", ds_install_from_github$package_name)
  if (1L <= length(repository_names))
    pak::pak(repository_names, lib = lib)

  base::rm(ds_install_from_github, repository_names)

  # ---- notify-tinytex ---------------------------------------------------------
  if (any(installed.packages() == "tinytex")) {
    # This comparison is copied from tinytex:::is_tinytex().
    if (!(gsub("^[.]", "", tolower(basename(tinytex::tinytex_root()))) == "tinytex"))
      tinytex::install_tinytex() # nocov
  }

  # ---- return ---------------------------------------------------------

  if (verbose)
    message("package_janitor is complete.")
}
