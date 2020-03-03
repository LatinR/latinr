#' Password management
#' 
#' Functions to save, get and remove latinr passwords. 
#' 
#' @param user,password username and password. If `NULL`, they will be asked
#' interactively (recommended).
#' @param check_credentials whether to validate credentials before saving them 
#' (recommended).
#' 
#' @details 
#' By default, `latinr_password_set()` will try to validate the credentials to 
#' so that only valid credentials are saved. However, this needs an active 
#' internet conection. Use `check_credentials = FALSE` if you are sure your 
#' credentials are correct but don't have internet. 
#' 
#' The default `NULL` value for user and password is the recommended method for
#' secuirity, as otherwise your credentials will be saved in plain text in the 
#' command history.
#' 
#' @name latinr_password
#' @aliases latinr_password_remove latinr_password_get latinr_password_set
NULL


#' @describeIn latinr_password Set a new user/password combination
#' @export
latinr_password_set <- function(user = NULL, password = NULL, check_credentials = TRUE) {
  keyring_check()
  if (is.null(user) || is.null(password)) {
    user <- readline("User: ")
    if (is.null(user) | user == "") {
      stop("No user supplied.")
    }
    password <- getPass::getPass(msg = "Password: ", noblank = TRUE)
    if (is.null(password)) {
      stop("No password supplied.")
    }
  }
  
  bypass_message <-  " or use `check_credentials = FALSE` to bypass login verification"
  ok_user <- !check_credentials
  if (!ok_user) {
    ok_user <- latinr_password_check(user, password)
  }
  
  if (isTRUE(ok_user[["ok"]]) | !check_credentials) {
    if(keyring::default_backend()$name != "env") {
      keyring::keyring_unlock()
    }
    keyring::key_set_with_value(service = latinr_service(), 
                                username = user,
                                password = password)
  } else {
    stop(ok_user[["message"]], bypass_message)
  }
  return(invisible(user))
}

#' @describeIn latinr_password Get password for an user.
#' @export
latinr_password_get <- function(user) {
  keyring_check()
  if(keyring::default_backend()$name != "env") {
    keyring::keyring_unlock()
  }
  keyring::key_get(service = latinr_service(), 
                   username = user)
}

#' @describeIn latinr_password Remove an user/password combination.
#' @export
latinr_password_remove <- function(user) {
  keyring_check()
  keyring::key_delete(service = latinr_service(), 
                      username = user)
}


latinr_password_check <- function(user, password) {
  url <- .latinr_url(what = "verify")
  response <- try(httr::POST(url, body = list(name = user, 
                                              password = password)),
                  silent = TRUE)
  if (inherits(response, "try-error")) {
    return(list(ok = FALSE,
                message = "Couldn't verify login information. Check your internet connection"))
  }
  response_url <- httr::parse_url(response[["url"]])
  verified <- !is.null(response_url[["query"]][["info"]])
  
  if (!verified) {
    return(list(ok = FALSE,
                message = paste0("Login verification failed for user ", 
                                 user, 
                                 ". Check your credentials")))
  }
  
  return(list(ok = TRUE, 
              cookie = httr::cookies(response)))
}