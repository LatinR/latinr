

parse_authors <- function(authors) {
  authors <- lapply(seq_along(authors), function(n) {
    author <- authors[[n]]
    if (isTRUE(author$corresponding)) {
      author$corresponding <- "on"
    } else {
      authors$corresponding <- NULL
    }
    
    if (isTRUE(author$speaker)) {
      author$speaker <- 1
    } else {
      author$speaker <- NULL
    }
    
    author$country <- country_code(author$country)
    
    names <- names(author) 
    
    names[names != "speaker"] <- paste0(names[names != "speaker"],
                                        n)
    names(author) <- names
    
    return(author)
  }
  )
  return(unlist(authors, FALSE))
}



# to do
country_code <- function(country) {
  country
}



content_from_list <- function(list) {
  
  fields <- lapply(seq_along(list), function(n) {
    content_field(names(list)[n], list[[n]])
  })
  
  return(paste0(c(fields, paste0("--", make_boundary(), "--")), collapse = "\n"))
}


content_field <- function(name, contents, boundary = make_boundary()) {
  head <- paste0("--", boundary)
  
  subhead <- paste0('Content-Disposition: form-data; name="', name, '"')
  
  if (name == "upload90642") {
    subhead <- paste0(subhead, '; filename="', basename(contents),'"\nContent-Type: application/pdf')
    # todo: upload file??
  }
  
  if (missing(contents)) {
    contents <- "\n"
  }
  field <- paste0(head, "\n", 
                  subhead, "\n\n",
                  contents)
  return(field)
}


topics <- seq(281909, 281920)

make_boundary <- function() {
  "---------------------------15446075827322774522002851"
}

.submission_track <- "240492"
.submission_a <- "21838528"

rmd_location <- "~/Documents/latinR2019/metamer.Rmd"
pdf_location <- "~/Documents/latinR2019/metamer.pdf"