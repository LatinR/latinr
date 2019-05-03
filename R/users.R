#' Default users
#' 
#' @param user user to use as the default (`NULL` means no default user). 
#' 
#' @return 
#' `latinr_default_user_get()` returns the default user set with `latinr_default_user_get()`
#' or the user if there's only one user saved with `latinr_password_set()`.
#' 
#' `latinr_default_user_get()` sets the default user (for the current session) 
#' and returns the supplied user invisibly. 
#' 
#' @aliases latinr_default_user_get latinr_default_user_set
#' @name latinr_default_user
NULL

#' @describeIn latinr_default_user Sets the default user. 
#' @export
latinr_default_user_set <- function(user = NULL) {
  options("LATINR.DEFAULT.USER" = user)
  return(invisible(user))
}

#' @describeIn latinr_default_user Gets the default user. 
#' @export
latinr_default_user_get <- function() {
  # First priotiy: global option
  user <- getOption("LATINR.DEFAULT.USER", default = NULL)
  
  if (is.null(user)) {
    # Second priority: check if only one user
    keyring_check()
    all_keys <- keyring::key_list()
    latinr_keys <- all_keys[all_keys[["service"]] == latinr_service(), ]
    
    if (nrow(latinr_keys) == 0) {
      warning("No users saved. Create an account at ", .latinr_url("latinr"), 
           " and (optionally) use 'latinr_password_set()' to save it.")
      user <- NULL
    } else if (nrow(latinr_keys) > 1) {
      stop("Multiple users present in keyring. Use 'latinr_default_user_set()' ", 
           "to set the default user manually.")  
    } else {
      user <- latinr_keys[["username"]]
    }
  }
  
  return(user)
}


keyring_check <- function() {
  has_keyring <- requireNamespace("keyring", quietly = TRUE)
  if (!has_keyring) {
    stop("Credentials management needs the keyring package.\n", 
         'You can install it with `install.packages("keyring")`')
  }
  return(invisible(has_keyring))
}
