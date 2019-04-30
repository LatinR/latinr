

.parse_authors <- function(authors) {
  authors <- lapply(seq_along(authors), function(n) {
    author <- authors[[n]]
    if (isTRUE(author$corresponding)) {
      author$corresponding <- "on"
    } else {
      authors$corresponding <- "off"
    }
    
    names <- names(author) 
    
    names <- paste0(names, n)
    names(author) <- names
    
    return(author)
  }
  )
  return(unlist(authors, FALSE))
}



.parse_topics <- function(topics) {
  topics  <- .topics_number[topics]
  topics <- as.list(topics)
  names(topics) <- rep("topic", length(topics))
  return(topics)
}

