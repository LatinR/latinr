#' Submit an article to LatinR
#' 
#' @export
latinr_submit <- function(rmd = list.files(getwd(), pattern = ".Rmd"), 
                          user = latinr_default_user_get(), 
                          user_check = TRUE) {
  if (length(rmd) == 0) {
    stop("No Rmd file selected")
  }
  
  if (length(rmd) > 1) {
    stop("Multiple Rmd files selected")
  }
  
  metadata <- rmarkdown::yaml_front_matter(rmd)
  
  if (user_check) {
    print_form_data(metadata)
    cat("Source file:", rmd, "\n\n")
    
    cat("Upload with username:", user, "\n\n")
    
    ok <- readline("Is the above information correct? (y/n) ")  
    if (tolower(ok) != "y") {
      return(invisible(NULL))
    }
  }

  message("Checking metadata")
  latinr_checks(metadata, check_is_error = TRUE)
  
  message("Rendering file")
  pdf_location <- rmarkdown::render(rmd, quiet = TRUE, 
                                    params = list(check_is_error = TRUE))
  
  keep <- c("title", "keywords", "field44396")
  metadata$field44396 <- switch(metadata$type,
                                oral   = "162531",
                                poster = "162532",
                                table  = "162533")
  
  authors <- parse_authors(metadata$authors)
  metadata$keywords <- paste0(metadata$keywords, collapse = "\n")
  topics <- .parse_topics(metadata$topics)
  metadata <- metadata[names(metadata) %in% keep]
  metadata <- c(metadata[keep[-3]], topics, metadata[keep[3]])
  # metadata$upload90642 <- pdf_location
  
  form_data <- c(authors, metadata)
  
  ### Submit form
  password <- latinr_password_get(user)
  
  url <- latinr_url("latinr")
  message("Logging in")
  session <- rvest::html_session(url)
  login_form <- rvest::html_form(session)[[1]]
  login_form <- rvest::set_values(login_form, 
                                  name = user, 
                                  password = password)
  session <- suppressMessages(rvest::submit_form(session, login_form))
  try_session <- try(rvest::follow_link(session, "enter as an author"), silent = TRUE)
  
  if (inherits(try_session, "try-error")) {
    session <- suppressMessages(rvest::follow_link(session, "author"))
    a <- httr::parse_url(session$url)$query$a
    session <- rvest::jump_to(session, paste0("https://easychair.org/conferences/submission_new.cgi", "?a=", a))
  } else {
    session <- try_session
  }
  
  submit_form <- rvest::html_form(session)[[1]]
  form <- submit_form
  submit_form <- form
  message("Submitting")
  # modified set_values
  form_data$form <- submit_form
  submit_form <- do.call(set_values, form_data)
  
  
  if (FALSE) {
    message("Submitting")
    # session <- rvest::submit_form(session, submit_form)
    # 
    # submission <- httr::parse_url(session$url)$query$submission
    # submission <- strsplit(submission, ";", fixed = TRUE)[[1]]
    # a <- strsplit(submission[3], "=", fixed = TRUE)[[1]][2]
    # submission <-  submission[1]
    submit_url <- paste0("https://easychair.org/conferences/submission_upload.cgi?",
           "submission=", submission, ";",
           "track=", .submission_track, ";",
           "a=", a)
    message(paste0("Go to this url to check your submission and upload your file:\n",
                   submit_url))
    # browseURL(paste0("https://easychair.org/conferences/submission?submission=4405891;a=21863896"))
    # 
    # session <- rvest::jump_to(session,
    #                           paste0("https://easychair.org/conferences/submission_upload.cgi?",
    #                                  "submission=", submission, ";",
    #                                  "track=", .submission_track, ";",
    #                                  "a=", a))
    # 
    return(invisible(submit_url))
  }
  message("Submitted")
}
