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
  
  # uses magick to count number of pages because pdftools requires
  # installing a system library and is a pita whereas magick comes
  # preinstalled in most linux distros. 
  n_pages <- length(magick::image_read(pdf))
  if (n_pages > 2) {
    stop("The file has more than the accepted numebr of pages (2).")
  }
  
  
  if (isTRUE(check)) {
    pdf_open <- readline("Press (enter) to check your submission file. ")
    file.show(pdf)
    pdf_ok <- readline("Send this file? (y/n) ")
    
    if (tolower(pdf_ok) != "y") {
      return(invisible(NULL))
    }
  }
  
  n_authors <- length(metadata$authors)
  keep <- c("title", "keywords", "abstract")
  # No more type?
  # metadata$field47997 <- .types_number[.types == metadata$type]
  
  authors <- .parse_authors(metadata$authors)
  metadata$keywords <- paste0(metadata$keywords, collapse = "\n")
  metadata <- metadata[names(metadata) %in% keep]
  names(metadata)[names(metadata) == "abstract"] <- "abstr"  # Name on the form
  
  form_data <- c(authors, metadata)
  
  ### Submit form
  url <- .latinr_url("latinr")
  message("Logging in")
  session <- rvest::session(url)
  login_form <- rvest::html_form(session)[[1]]
  login_form <- rvest::html_form_set(login_form, 
                                     name = user, 
                                     password = password)
  session <- suppressMessages(rvest::session_submit(session, login_form))
  
  
  
  # If previously submitted
  session_try <- try(rvest::session_follow_link(session, "author"), silent = TRUE)
  if (inherits(session_try, "try-error")) {
    session <- rvest::session_follow_link(session, "make a new submission") 
  } else {
    session <- session_try
    menu <- rvest::html_element(session, css = "#menu1")
    
    click <- rvest::html_attr(menu, "onclick")
    
    click <- gsub("Menu.followLink\\('menu1','", "", click)
    click <- sub("'\\)", "", click)
    session <- rvest::session_jump_to(session, paste0("https://easychair.org", click))
  }
  
  submit_form <- rvest::html_form(session)[[1]]
  
  browser()
  message("Submitting")
  submit_form <- add_authors(submit_form, n_authors)
  form_data$form <- submit_form
  submit_form <- do.call(rvest::html_form_set, form_data)
  
  # Workaround because rvest doesn't do file upload correctly.
  submit_form <- rvest:::submission_build(submit_form, NULL)
  submit_form$values$upload114286 <- httr::upload_file(pdf)
  resp <- suppressMessages(rvest:::submission_submit(submit_form, session$config, handle = session$handle))
  session <- rvest:::session_set_response(session, resp)
  
  title <- rvest::html_text(rvest::html_nodes(session, "title")[[1]])
  
  if (substr(title, 1, 21) != "LatinR2021 Submission") {
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
  message("Submission successful! Check your email for confirmation.")
  return(invisible(TRUE))
  
}


add_authors <- function(form, n_authors) {
  new_form <- form
  if (n_authors < 4) {
    return(form)
  }
  
  fields <- c("first_name", "last_name", "email", "country", "url", "Affiliation", "corresponding")
  based_on <- paste0(fields, "1")
  # Agrega autores mayores a 3
  for (i in seq_len(n_authors - 3) + 3) {
    new_fields <- paste0(fields, i)
    
    for (f in seq_along(new_fields)) {
      new_form <- add_field(new_form, new_fields[f], based_on[f])
      
    }
    
  }
  return(new_form)
}


add_field <- function(form, new_name, based_on) {
  new_form <- form
  new_form$fields[[new_name]] <- new_form$fields[[based_on]]
  new_form$fields[[new_name]]$name <- new_name
  
  return(new_form)
}
