print_form_data <- function(metadata) {
  
  cat(metadata$title, "\n")
  
  a <- switch(metadata$type,
          oral = cat("Oral presentation\n"),
          poster = cat("Poster\n"),
          table = cat("Round table\n"))
  cat("\n")

  
  a <- lapply(seq_along(metadata$authors),
         function(n) {
           author <-  metadata$authors[[n]]
           cat("Author #", n, ":\n", sep = "")
           cat("  Name:        ", author$first_name, author$last_name, "\n")
           cat("  Email:       ", author$email, "\n")
           cat("  Affil:       ", author$Affiliation, "\n")
           cat("  Country:     ", rownames(.countries)[.countries$countries == author$country], "\n")
           cat("  Corresonding:", author$corresponding, "\n\n")
         })
  
  cat("Keywords:", paste0(metadata$keywords, collapse = " - "), "\n")
  cat("Topics:\n")
  cat("  * ", paste0(.topics[metadata$topics], collapse = "\n  * "), sep = "")
  cat("\n")
  return(invisible(metadata))
}

