


latinr_topics <- function() {
  a <- lapply(seq_along(.topics), 
         function(topic) cat(formatC(topic, width = 2), " - ", .topics[topic], "\n", sep = ""))
  return(invisible(.topics))
}


latinr_countries <- function() {
  .countries
}
