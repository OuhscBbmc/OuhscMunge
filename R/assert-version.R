#' @name assert_version
#' @aliases assert_version_r, assert_version_package
#' @title I that the local machine is using an acceptable version.
#'
#' @description Assert that the local machine is using a version that satisfies
#' the minimum version specified.
#'
#' @param minimum A [package_version] or [character] that specifies
#' the version of the software being examined
#' (ie, R, a package, or an ODBC driver).
#'
#' @return An error if the minimum version is not met.
#' If the local machine is using an acceptable version, an `invisible()` `TRUE`
#' is returned.
#'
#' @note These functions help us assert the the local machine has an acceptable
#' version of the software running.
#'
#' For [assert_version_r()], the current default value is "4.2.0" because
#' it introduced the
#' [placeholder](https://davidbudzynski.github.io/general/2022/04/23/r-native-placeholder.html)
#' for the native pipe.  Future versions of OuhscMunge will likely increase
#' the default value to keep pace with important developments to R.
#'
#' @author Will Beasley
#'
#' @examples
#' # Check R
#' assert_version_r("3.1.0")
#' assert_version_r()
#' # Fails: assert_version_r("99.1.0")
#'
#' # Check packages
#' assert_version_package("base", "3.1.0")
#' assert_version_package("OuhscMunge", "0.1.0")
#' # Fails: assert_version_package("base", "99.1.0")
#' # Fails: assert_version_package("OuhscMunge", "9.1.0")
#' # Fails: assert_version_package("OuhscMunge", "9.1.0", installation_code = 'remotes::install_github("OuhscBbmc/OuhscMunge")')
#' # Fails:
#' # Fails: assert_version_package("OuhscMungeee", "9.1.0")
#'
#'
#' @export
assert_version_r <- function(minimum = base::package_version("4.2.1")) {
  checkmate::assert_vector(minimum, len = 1, any.missing = FALSE)
  minimum <-
    if (inherits(minimum, "package_version")) {
      as.character(minimum)
    } else if (inherits(minimum, "character")) {
      # Make sure it can be recognized as a version
      as.character(base::package_version(minimum))
    } else {
      stop("The value passed to `minimum` must inherit either from 'character' or `package_version`.")
    }

  current <- as.character(utils::packageVersion("base"))

  comparison <-
    utils::compareVersion(
      current,
      minimum
    )

  if (comparison < 0 ) {
    "Your R version is too old.  It is %s, but needs to be at least %s.  Update it at <https://cloud.r-project.org>." |>
      sprintf(
        current,
        minimum
      ) |>
      stop()
  } else {
    invisible(TRUE)
  }
}

#' @export
assert_version_package <- function(
  package_name,
  minimum,
  installation_code = ""
) {
  checkmate::assert_character(package_name, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assert_vector(minimum, len = 1, any.missing = FALSE)
  checkmate::assert_character(installation_code, len = 1, min.chars = 0, any.missing = FALSE)

  package_is_installed <- requireNamespace(package_name, quietly = TRUE)

  installation_message <-
    if (1L <= nchar(installation_code)) {
      "  Install the package with `%s`.  Afterwards, please restart the R session." |>
        sprintf(installation_code)
    } else {
      "  Afterwards, please restart the R session."
    }

  if (!package_is_installed) {
    "The package '%s' not installed.%s" |>
      sprintf(package_name, installation_message) |>
      stop()
  }

  minimum <-
    if (inherits(minimum, "package_version")) {
      as.character(minimum)
    } else if (inherits(minimum, "character")) {
      # Make sure it can be recognized as a version
      as.character(base::package_version(minimum))
    } else {
      stop("The value passed to `minimum` must inherit either from 'character' or `package_version`.")
    }

  current <- as.character(utils::packageVersion(package_name))

  comparison <-
    utils::compareVersion(
      current,
      minimum
    )

  if (comparison < 0 ) {
    "Your version of the `%s` package is too old.  It is %s, but needs to be at least %s.%s" |>
      sprintf(
        package_name,
        current,
        minimum,
        installation_message
      ) |>
      stop()
  } else {
    invisible(TRUE)
  }
}
