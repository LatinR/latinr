
latinr_service <- function() {
  "latinr_easychair"
}

latinr_default_user_set <- function(user = NULL) {
  options("LATINR.DEFAULT.USER" = user)
}

latinr_default_user_get <- function() {
  # First priotiy: global option
  user <- getOption("LATINR.DEFAULT.USER", default = NULL)
  
  if (is.null(user)) {
    # Second priority: check if only one user
    all_keys <- keyring::key_list()
    latinr_keys <- all_keys[all_keys[["service"]] == latinr_service(), ]
    if (nrow(latinr_keys) == 1) {
      user <- latinr_keys[["username"]]
    } else {
      stop("No default user set and multiple users present in keyring.")  
    }
  }
  
  return(user)
}



latinr_url <- function(what) {
  switch(what,
         verify = "https://easychair.org/account/verify",
         login = "https://easychair.org/account/signin",
         latinr = "https://easychair.org/conferences/?conf=latinr2019",
         submit = "https://easychair.org/conferences/submission_new_z.cgi",
         stop("Unrecognised what")
  )
}