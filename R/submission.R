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
  matadta <- c(metadata[keep[-3]], topics, metadata[keep[3]])
  metadata$upload90642 <- pdf_location
  
  start <- list(track = .submission_track, a = .submission_a)
  end <- list(end = "1", x = "1", button = "submit")
  form <- c(start, authors, metadata, end)
  
  form <- content_from_list(form)
  
  
  ### Submit form
  password <- latinr_password_get(user)
  url <- latinr_url("submit")
  
  ok_user <- latinr_password_check(user, password)
  
  if (!isTRUE(ok_user[["ok"]])) {
    stop(ok_user, ".", sep = "")
  }
  
  cookie <- ok_user[["cookie"]]
  cookie <- cookie[cookie[["name"]] == "cool1", "value"]
  
  if (FALSE){
    rt <- httr::POST(url, 
                     httr::add_headers(
                       "Cookie"       = cookie,
                       "Content-Type" = paste0("multipart/form-data; ", .make_boundary())
                     ))
    
  }
}



