#' Submit an article to LatinR
#' 
#' @param rmd rmarkdown source file of the submission.
#' @param pdf pdf file for submission. If `NULL`, the source file will be 
#' rendered and used as file (recommended).
#' @param user user used for submission.
#' @param check whether to ask for confirmation (recommended).
#' 
#' @details 
#' It is highly recommended to use the latinr template that comes with 
#' this package and to use `pdf = NULL` as this will ensure that the publication
#' adheres to the correct format and is appropriately anonymised.
#' 
#' It's also very important to check that your submission has gone through 
#' correctly.
#' 
#' @export
latinr_submit <- function(rmd = list.files(getwd(), pattern = ".Rmd"), 
                          pdf = NULL,
                          user = latinr_default_user_get(), 
                          check = TRUE) {
  disclaimer <- "----- IMPORTANT!! ----- \nAutomatic submission is still experimental. \nPlease be sure to manually check your submision at the end of the process and correct it if needed!"
  
  message(disclaimer)
  
  check_latest_version()
  check_submissions_open()
  
  if (length(rmd) == 0) {
    stop("No Rmd file selected")
  }
  
  if (length(rmd) > 1) {
    stop("Multiple Rmd files selected")
  }
  
  metadata <- rmarkdown::yaml_front_matter(rmd)
  
  if (is.null(user)) {
    user <- readline("User: ")
    password <- getPass::getPass(msg = "Password: ", noblank = TRUE)
    if (is.null(password)) {
      stop("No password supplied.")
    }
  } else {
    password <- latinr_password_get(user)
  }
  
  if (isTRUE(check)) {
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
  
  if (is.null(pdf)) {
    message("Rendering file")
    
    output_options <- metadata[["output"]][[1]]
    output_options["submission"] <- TRUE
    
    pdf <- rmarkdown::render(rmd, quiet = TRUE, 
                             output_format = do.call(latinr_article, output_options),
                             params = list(check_is_error = TRUE))
  }
  
  if (isTRUE(check)) {
    pdf_open <- readline("Press (enter) to check your submission file. ")
    file.show(pdf)
    pdf_ok <- readline("Send this file? (y/n) ")
    
    if (tolower(pdf_ok) != "y") {
      return(invisible(NULL))
    }
  }
  
  keep <- c("title", "presenter", "keywords", "field47997")
  metadata$field47997 <- .types_number[.types == metadata$type]
  
  authors <- .parse_authors(metadata$authors)
  metadata$keywords <- paste0(metadata$keywords, collapse = "\n")
  topics <- .parse_topics(metadata$topics)
  metadata <- metadata[names(metadata) %in% keep]
  metadata <- c(metadata[keep[-4]], topics, metadata[keep[4]])
  
  form_data <- c(authors, metadata, list(upload102820 = httr::upload_file(pdf)))
  
  ### Submit form
  url <- .latinr_url("latinr")
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
    session <- rvest::jump_to(session, paste0("https://easychair.org/conferences/submission_new", "?a=", a))
  } else {
    session <- try_session
  }
  
  submit_form <- rvest::html_form(session)[[1]]
  
  message("Submitting")
  form_data$form <- submit_form
  submit_form <- do.call(set_values, form_data)
  
  session <- suppressMessages(submit_form(session, submit_form))
  
  title <- rvest::html_text(rvest::html_nodes(session, "title")[[1]])
  
  if (substr(title, 1, 21) != "LatinR2020 Submission") {
    errors <- rvest::html_text(rvest::html_node(session, "div.subcontent ul"))
    
    msg <- "There was an error with your submission"
    
    if (is.null(errors)) {
      msg <- paste0(msg, ", but I'm still not smart enought to know which :(!\n",
                    "Check your submission details and if you still get this error, submit manually", 
                    paste0(" at ", .latinr_url("latinr")))
    } else {
      msg <- paste0(msg, ":\n", paste(errors, collapse = "\n"))
    }
    
    stop(msg)

  } 
  message(title)
  
  submission <- httr::parse_url(session$url)$query$submission
  submission <- strsplit(submission, ";", fixed = TRUE)[[1]]
  a <- strsplit(submission[3], "=", fixed = TRUE)[[1]][2]
  submission <-  submission[1]
  submit_url <- paste0("https://easychair.org/conferences/submission_upload.cgi?",
                       "submission=", submission, ";",
                       "track=", .submission_track, ";",
                       "a=", a)
  message(paste0("Go to this url to check your submission:\n",
                 submit_url))
  
  return(invisible(submit_url))
  
  message("Submitted")
}
