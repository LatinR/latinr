check_latest_version <- function() {
  description <- readLines("https://raw.githubusercontent.com/LatinR/latinr/master/DESCRIPTION")
  latest_version <- description[grepl("Version:", description)]
  latest_version <- gsub("Version: ", "", latest_version)
  
  this_version <- sessioninfo::package_info("latinr", dependencies = FALSE)$ondiskversion
  
  if (latest_version != this_version) {
    stop("Your version of latinr (", this_version, ") is not the most recent (", 
         latest_version, "). Please update with `devtools::install_github(\"latinr/latinr\")`")
  }
}


check_submissions_open <- function() {
  submissions <- yaml::read_yaml("https://raw.githubusercontent.com/LatinR/latinr/master/submission_conditions.yaml")
 if (!submissions$open) {
   stop("Latinr service is not open for submissions yet.")
 }
}
