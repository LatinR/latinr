#' R Markdown output format for submissions to LatinR
#' 
#' 
#' @inheritParams rmarkdown::pdf_document
#'
#' @export
latinr_article <- function( ..., keep_tex = FALSE, highlight = "default", citation_package = "none", 
  latex_engine = "xelatex", submission = TRUE) {
  pdf_document_format(
    "latinr_article", 
    submission = submission,
    keep_tex = keep_tex, highlight = highlight,
    citation_package = citation_package, latex_engine = latex_engine, 
    md_extensions = c("-autolink_bare_uris"),...
  )
}

pdf_document_format <- function(format, submission = TRUE, ...) {
  
  if (isTRUE(submission)) {
    template <- find_resource(format, 'template_anom.tex')  
  } else {
    template <- find_resource(format, 'template_name.tex')  
  }
  
  fmt <- rmarkdown::pdf_document(..., template = template)
  fmt$inherits <- "pdf_document"
  fmt
}


find_resource <- function(template, file = 'template.tex') {
  res <- system.file(
    "rmarkdown", "templates", template, "resources", file, package = "latinr"
  )
  if (res == "") stop(
    "Couldn't find template file ", template, "/resources/", file, call. = FALSE
  )
  res
}


#' Checks that metadata is ok
#' 
#' Does basics checks on metadata so that it adheres to submission guidelines. 
#' 
#' @param metadata yaml metadata.
#' @param check_is_error whether to treat fails as errors or only warnings
#' useful during development).
#' 
#' @export
latinr_checks <- function(metadata, check_is_error = TRUE) {
  
  if (!is.null(metadata$speaker)) {
    warning("The 'speaker' field has been renamed to 'presenter'.")
  }
  
  authors <- metadata$authors
  required <- c("last_name", "email", "country", "affiliation")
  
  missing_fields <- lapply(seq_along(authors), function(a) {
    person <- authors[[a]]
    len <- lengths(person[names(person) %in% required])
    nulls <- names(person)[names(person) %in% required][len == 0]
    length_0 <- nchar(person[names(person) %in% required]) == 0
    length_0 <- names(person)[names(person) %in% required][length_0]
    
    missing_fields <- required[ !(required %in% names(person)) ]
    
  
    missing_fields <- c(nulls, missing_fields, length_0)
    
    if (length(missing_fields) != 0) {
      paste0("Author ", a, ": missing ", knitr::combine_words(missing_fields))
    } else {
      ""
    }
  })
  
  errors <- unlist(missing_fields)
  errors <- errors[nchar(errors) != 0]
  
  
  if (is.null(metadata$presenter) || length(metadata$presenter) == 0) {
    errors <- c(errors, "Missing presenter")
  } else {
    if (length(metadata$presenter) > 1) {
      errors <- c(errors, "Only one author can be marked as presenter")
    }
    
    if (metadata$presenter > length(authors)) {
      errors <- c(errors, paste0("presenter cannot be '", metadata$presenter,
                                 "' if there are ", length(authors), " authors"))
    }
  }
 
  
  n_correspondence <- Reduce("+", lapply(authors, function(a) isTRUE(a$corresponding)))
  
  if (n_correspondence == 0) {
    errors <- c(errors, "Missing corresponding author")
  }
  
  if (is.null(metadata$title)) {
    errors <- c(errors, "Missing title")
  }
  
  if (is.null(metadata$topics)) {
    errors <- c(errors, "Missing topics")
  } else {
    if (any(metadata$topics > length(.topics))) {
      errors <- c(errors, paste0("Invalid topic detected, must be a number between 1 and ",
                                 length(.topics)))
    }
  }
  
  keywords <- metadata$keywords
  
  if (sum(lengths(keywords)) < 3) {
    errors <- c(errors, "At least three keywords needed")
  }
  
  
  if (!(metadata$type %in% .types)) {
    errors <- c(errors, paste0("Submission type mus be ", 
                               knitr::combine_words(.types, and = " or ")))
  }
  
  if (length(errors) != 0) {
    text <- paste0("\nFound the following errors:\n * ", paste(errors, collapse = "\n * "))
    if (isTRUE(check_is_error)) {
      stop(text)
    } else {
      cat(text)
    }
  } 
  invisible(errors)
}


