#This code checks the user's installed packages against the packages listed in `./utility/package_dependency_list.csv`.
#   These are necessary for the repository's R code to be fully operational.
#   CRAN packages are installed only if they're not already; then they're updated if available.
#   GitHub packages are installed regardless if they're already installed.
#If anyone encounters a package that should be on there, please add it to `./utility/package_dependency_list.csv`

package_janitor <- function(
                            path_package_dependencies, # = './utility/package_dependency_list.csv',
                            cran_repo = "http://cran.rstudio.com",
                            update_packages = TRUE,
                            check_xml_linux = (R.Version()$os=="linux-gnu"),
                            check_libcurl_linux = (R.Version()$os=="linux-gnu"),
                            check_openssl_linux = (R.Version()$os=="linux-gnu"),
                            verbose = TRUE
                          ) {

  if( !file.exists(path_package_dependencies))
    base::stop("The path `", path_package_dependencies, "` was not found.  Make sure the working directory is set to the root of the repository.")
  
  required_columns <- c("package_name", "on_cran", "install", "github_username", "description")
  
  #####################################
  ## load_data
  if( verbose ) message("package_janitor is loading the list of package depencies.")
  ds_packages <- utils::read.csv(file=path_package_dependencies, stringsAsFactors=FALSE)
  
  rm(path_package_dependencies)
  #####################################
  ##  tweak_data
  missing_columns <- base::setdiff(required_columns, colnames(ds_packages))
  if( length(missing_columns) > 0 )
    stop(paste("The data.frame of the required packages is missing the following columns:", missing_columns))
  
  ds_install_from_cran <- ds_packages[ds_packages$install & ds_packages$on_cran, ]
  ds_install_from_github <- ds_packages[ds_packages$install & !ds_packages$on_cran & !is.na(ds_packages$github_username) & nchar(ds_packages$github_username)>0, ]
  
  rm(ds_packages)
  #####################################
  ## update_cran_packages
  if( verbose ) message("package_janitor is updating the existing packages from CRAN.")
  if( update_packages )
    utils::update.packages(ask=FALSE, checkBuilt=TRUE, repos=cran_repo)
  
  #####################################
  ## install_devtools
  if( verbose ) message("package_janitor is installing the the `devtools` and `httr` packages from CRAN if necessary.")
  
  if( !base::requireNamespace("httr") )
    utils::install.packages("httr", repos=cran_repo)
  
  if( !base::requireNamespace("devtools") )
    utils::install.packages("devtools", repos=cran_repo)

  #####################################
  ## install_cran_packages
  if( verbose ) message("package_janitor is installing the CRAN packages:")
  
  for( package_name in ds_install_from_cran$package_name ) {
    if( package_name =="devtools" ) {
      if( verbose ) message("The `devtools` package does not need to be in the list of package dependencies.  It's updated automatically.")
      
    } else if( package_name =="httr" ) {
      if( verbose ) message("The `httr` package does not need to be in the list of package dependencies.  It's updated automatically.")
      
    } else {
        available <- base::requireNamespace(package_name, quietly=TRUE) #Checks if it's available
        if( !available ) {
          if( verbose ) message("Installing `", package_name, "` from CRAN, including its dependencies.")
          utils::install.packages(package_name, dependencies=TRUE, repos=cran_repo)
          #base::requireNamespace( package_name, character.only=TRUE)
        } else if( update_packages ) {
          if( verbose ) message("`", package_name, "` exists, and verifying it's dependencies are installed too.")
          #Make sure all their dependencies are installed & up-to-date
          need_to_install <- devtools::package_deps(package_name, dependencies=TRUE)
          devtools::update_packages(need_to_install, repos=cran_repo)
        }
        base::rm(available)
      }
  }
  
  rm(ds_install_from_cran, package_name)
  #####################################
  ## check_xml_linux
  #http://stackoverflow.com/questions/7765429/unable-to-install-r-package-in-ubuntu-11-04
  
  if( check_xml_linux ) {
    libcurl_results <- base::system("locate r-cran-xml")
    libcurl_missing <- (libcurl_results==0)
    
    if( libcurl_missing )
      base::warning("This Linux machine is possibly missing the 'libxml2-dev' library.  ",
                    "Consider running `sudo apt-get install r-cran-xml`.", 
                    "or the equivalent for your distribution.")
    
    base::rm(libcurl_results, libcurl_missing)
  }
  
  #####################################
  ## check_for_libcurl
  if( check_libcurl_linux ) {
    libcurl_results <- base::system("locate libcurl4")
    libcurl_missing <- (libcurl_results==0)
    
    if( libcurl_missing )
      base::warning("This Linux machine is possibly missing the 'libcurl' library.  ",
                    "Consider running `sudo apt-get install libcurl4-openssl-dev`.", 
                    "or the equivalent for your distribution.")
    
    base::rm(libcurl_results, libcurl_missing)
  }
  
  #####################################
  ## check_openssl_linux  
  if( check_openssl_linux ) {
    openssl_results <- base::system("locate libssl-dev")
    openssl_missing <- (openssl_results==0)
    
    if( openssl_missing )
      base::warning("This Linux machine is possibly missing the 'libssl' library.  ",
                    "Consider running `sudo apt-get install libssl-dev`.", 
                    "or the equivalent for your distribution.")
    
    base::rm(openssl_results, openssl_missing)
  }
  
  #####################################
  ## install_github_packages
  if( verbose ) message("package_janitor is installing the GitHub packages:")
  
  for( i in base::seq_len(base::nrow(ds_install_from_github)) ) {
    package_name <- ds_install_from_github[i, "package_name"]
    if( verbose ) message("Installing `", package_name, "` from GitHub, (not including its dependencies).")
    
    username <- ds_install_from_github[i, "github_username"]
    repository_name <- paste0(username, "/", package_name)
    devtools::install_github(repo=repository_name)
    base::rm(package_name, username, repository_name)
  }
  
  base::rm(ds_install_from_github, i)
  if( verbose ) message("package_janitor is complete.")
}
# OuhscMunge:::package_janitor()
