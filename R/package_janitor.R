#This code checks the user's installed packages against the packages listed in `./utility/package_dependency_list.csv`.
#   These are necessary for the repository's R code to be fully operational.
#   CRAN packages are installed only if they're not already; then they're updated if available.
#   GitHub packages are installed regardless if they're already installed.
#If anyone encounters a package that should be on there, please add it to `./utility/package_dependency_list.csv`

package_janitor <- function(
                            path_csv, # = './utility/package_dependency_list.csv',
                            cran_repo = "http://cran.rstudio.com",
                            update_packages = TRUE,
                            check_libcurl_linux = (R.Version()$os=="linux-gnu")
                          ) {

  if( !file.exists(path_csv))
    base::stop("The path `", path_csv, "` was not found.  Make sure the working directory is set to the root of the repository.")
  
  required_columns <- c("package_name", "on_cran", "install", "github_username", "description")
  
  #####################################
  ## load_data
  ds_packages <- utils::read.csv(file=path_csv, stringsAsFactors=FALSE)
  
  rm(path_csv)
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
  if( update_packages )
    utils::update.packages(ask=FALSE, checkBuilt=TRUE, repos=cran_repo)
  
  #####################################
  ## install_devtools

  if( !base::requireNamespace("devtools") )
    utils::install.packages("devtools", repos=cran_repo)

  #####################################
  ## install_cran_packages
  for( package_name in ds_install_from_cran$package_name ) {
    available <- base::requireNamespace(package_name, quietly=TRUE) #Checks if it's available
    if( !available ) {
      utils::install.packages(package_name, dependencies=TRUE, repos=cran_repo)
      #base::requireNamespace( package_name, character.only=TRUE)
    } else if( update_packages ) {
      #Make sure all their dependencies are installed & up-to-date
      update(devtools::package_deps(package_name, dependencies=TRUE), repos=cran_repo) #devtools:::update.package_deps()
    }
    base::rm(available)
  }
  
  rm(ds_install_from_cran, package_name)
  #####################################
  ## check_for_libcurl
  
  if( check_libcurl_linux ) {
    libcurl_results <- base::system("locate libcurl4")
    libcurl_missing <- (libcurl_results==0)
    
    if( libcurl_missing )
      base::warning("This Linux machine is possibly missing the 'libcurl' library.  ",
                    "Consider running `sudo apt-get install libcurl4-openssl-dev`.")
    
    base::rm(libcurl_results, libcurl_missing)
  }
  
  #####################################
  ## install_github_packages
  
  for( i in base::seq_len(base::nrow(ds_install_from_github)) ) {
    package_name <- ds_install_from_github[i, "package_name"]
    username <- ds_install_from_github[i, "github_username"]
    repository_name <- paste0(username, "/", package_name)
    devtools::install_github(repo=repository_name)
    base::rm(package_name, username, repository_name)
  }
  
  base::rm(ds_install_from_github, i)
}
# OuhscMunge:::package_janitor()
