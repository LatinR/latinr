latinr_submit <- function(..., user = latinr_default_user_get()) {
  ### Parse YAML
  metadata <- rmarkdown::yaml_front_matter(rmd_location)
  
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
  metadata$upload90642 <- pdf_location
  
  form <- c(authors, metadata)
  
  ### Submit form
  password <- latinr_password_get(user)
  url <- latinr_url("submit")
  
  # ok_user <- latinr_password_check(user, password)
  # 
  # if (!isTRUE(ok_user[["ok"]])) {
  #   stop(ok_user, ".", sep = "")
  # }
  # 
  # cookie <- ok_user[["cookie"]]
  # cookie <- cookie[cookie[["name"]] == "cool1", "value"]
  
  url <- latinr_url("latinr")
  
  session <- rvest::html_session(url)
  login_form <- rvest::html_form(session)[[1]]
  login_form <- rvest::set_values(login_form, 
                                  name = user, 
                                  password = password)
  session <- rvest::submit_form(session, login_form)
  session <- rvest::follow_link(session, "enter as an author")
  
  submit_form <- rvest::html_form(session)[[1]]
  
  # modified set_values
  submit_form <- set_values(submit_form, 
                            first_name1 = "Elio",
                            last_name1 = "CAmpi",
                            email1 = "elio@gmail.com",
                            country1 = "ar",
                            Affiliation1 = "CIMA",
                            corresponding1 = TRUE,
                            speaker = TRUE,
                            title = "TITULO",
                            keywords = "una \n palabra \n clave",
                            topic = "281910",
                            field44396 = "162532",
                            upload90642 = pdf_location
  )
  
  
  session <- rvest::submit_form(session, submit_form)
  
  
  # start <- list(track = .submission_track, 
  #               a = httr::parse_url(session$url)$query$a)
  # end <- list(end = "1", x = "1", button = "submit")
  # form <- c(start, authors, metadata, end)
  # 
  # form$title <- NULL
  # 
  # form <- content_from_list(form)
  # 
  # 
  # cookie <- httr::cookies(session)
  # cookie <- paste0("cool1=", cookie[cookie[["name"]] == "cool1", "value"])
  # 
  # head <- httr::add_headers(
  #   "Accept-Encoding" = "gzip, deflate, br",
  #   "Referer" = session[["url"]],
  #   "User-Agent" = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:67.0) Gecko/20100101 Firefox/67.0",
  #   "Content-Type" =  paste0("multipart/form-data; boundary=", .make_boundary()),
  #   "Content-Length" = nchar(form, "bytes"),
  #   "Connection" = "keep-alive",
  #   "Cookie" = cookie
  # )
  # if (FALSE){
  # after_post <- httr::POST(latinr_url("submit"),
  #                          session$config, 
  #                          head,
  #                          body = form,
  #                          handle = session$handle)
  #  
  # after_post <- rvest:::request_POST(session, 
  #                      latinr_url("submit"),
  #                      
  #                      config(referer = session$url),
  #                      user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.86 Safari/537.36"),
  #                      body = form)
  #  
  # }
}
