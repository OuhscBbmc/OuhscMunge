rm(list=ls(all.names=TRUE))
# library(devtools)
import::from("magrittr", "%>%")
deviceType <- ifelse(R.version$os=="linux-gnu", "X11", "windows")
options(device = deviceType) #http://support.rstudio.org/help/discussions/problems/80-error-in-function-only-one-rstudio-graphics-device-is-permitted

spelling::spell_check_package()
# spelling::update_wordlist()
urlchecker::url_check(); urlchecker::url_update()

devtools::document()
devtools::check_man() #Should return NULL
devtools::build_vignettes()
pkgdown::clean_site()
pkgdown::build_site()
# system("R CMD Rd2pdf --no-preview --force --output=./documentation-peek.pdf ." )

lintr::lint_package()
# lintr::lint("R/redcap-metadata-coltypes.R")
checks_to_exclude <- c(
  "covr",
  "cyclocomp",
  "lintr_line_length_linter"
)
gp <-
  goodpractice::all_checks() |>
  purrr::discard(~(. %in% checks_to_exclude)) |>
  {
    \(checks)
    goodpractice::gp(checks = checks)
  }()
goodpractice::results(gp)
gp
# If necessary: tinytex::tlmgr_install("makeindex")
b <- BiocCheck::BiocCheck()
# styler::style_pkg()
# styler::style_file("R/redcap-read-oneshot.R")

devtools::run_examples(); #dev.off() #This overwrites the NAMESPACE file too
# devtools::run_examples(), "redcap_read.Rd")
test_results_checked <- devtools::test()
# test_results_checked <- devtools::test(filter = "read_b.*")
# testthat::test_dir("./tests/")
# test_results_not_checked <- testthat::test_dir("./tests/manual/")

# devtools::check(force_suggests = FALSE)
devtools::check(cran=T)
# devtools::check_rhub(email="wibeasley@hotmail.com")
# devtools::build_win(version="R-devel") #CRAN submission policies encourage the development version
# devtools::revdep_check(pkg="REDCapR", recursive=TRUE)
# devtools::release(check=FALSE) #Careful, the last question ultimately uploads it to CRAN, where you can't delete/reverse your decision.
