#' R Markdown output format for submissions to LatinR
#'
#' @export
latinr_article <- function(
  ..., keep_tex = TRUE, citation_package = 'natbib',
  highlight = NULL, md_extensions = c("-autolink_bare_uris", "-auto_identifiers")
) {
  pdf_document_format(
    "latinr_article", keep_tex = keep_tex, highlight = highlight,
    citation_package = citation_package, md_extensions = md_extensions, ...
  )
}



pdf_document_format <- function(
  format, template = find_resource(format, 'template.tex'), ...
) {
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
