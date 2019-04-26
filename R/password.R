latinr_password_set <- function(user, password, check_credentials = TRUE) {
  bypass_message <-  " or use `check_credentials = FALSE` to bypass login verification"
  ok_user <- !check_credentials
  if (!ok_user) {
    ok_user <- latinr_password_check(user, password)
  }
  
  if (isTRUE(ok_user[["ok"]]) | !check_credentials) {
    keyring::keyring_unlock()
    keyring::key_set_with_value(service = latinr_service(), 
                                username = user,
                                password = password)
  } else {
    stop(ok_user[["message"]], bypass_message)
  }
  return(invisible(user))
}



latinr_password_get <- function(user) {
  keyring::keyring_unlock()
  keyring::key_get(service = latinr_service(), 
                   username = user)
}

latinr_password_remove <- function(user) {
  keyring::key_delete(service = latinr_service(), 
                      username = user)
}




latinr_password_check <- function(user, password) {
  url <- latinr_url(what = "verify")
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