check_latest_version <- function() {
  description <- readLines("https://raw.githubusercontent.com/LatinR/latinr/master/DESCRIPTION")
  latest_version <- description[grepl("Version:", description)]
  latest_version <- gsub("Version: ", "", latest_version)
  pkg_info <- sessioninfo::package_info("latinr", dependencies = FALSE)
  
  this_version_loaded <- pkg_info$loadedversion
  this_version_disk <- pkg_info$ondiskversion
  
  
  
  if (latest_version != this_version_disk) {
    stop("Your version of latinr (", this_version_disk, ") is not the most recent (", 
         latest_version, "). Please update with `devtools::install_github(\"latinr/latinr\")`",
         "and restart R (In RStudio: Session -> Restart R).")
  } 
  
  if (latest_version != this_version_loaded) {
    stop("Your loaded version of latinr (", this_version_loaded, ") is not the most recent (", 
         latest_version, "). Please restart R (In RStudio: Session -> Restart R).")
    
  }
}


check_submissions_open <- function() {
  submissions <- yaml::read_yaml("https://raw.githubusercontent.com/LatinR/latinr/master/submission_conditions.yaml")
 if (!submissions$open) {
   stop("The latinr package is not ready for submissions yet.")
 }
}
