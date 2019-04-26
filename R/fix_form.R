# from https://github.com/tidyverse/rvest/pull/132/commits/e1abd4b823be25ea5c8f581266c40bbf5cb4367a
set_values <- function(form, ...) {
  new_values <- list(...)
  
  # check for valid names
  no_match <- setdiff(names(new_values), names(form$fields))
  if (length(no_match) > 0) {
    stop("Unknown field names: ", paste(no_match, collapse = ", "),
         call. = FALSE)
  }
  
  for (field in unique(names(new_values))) {
    type <- form$fields[[field]]$type %||% "non-input"
    if (type == "hidden") {
      warning("Setting value of hidden field '", field, "'.", call. = FALSE)
    } else if (type == "submit") {
      stop("Can't change value of submit input '", field, "'.", call. = FALSE)
    }
    
    if (type == "checkbox") {
      # there could be multiple check boxes with same 'name's and different 'value's
      idx <- unlist(names(new_values) == field)
      form <- set_checkbox(form, new_values[idx])
    } else if (type == "radio") {
      # there could be multiple radio buttons with same 'name's and different 'value's
      idx <- unlist(names(new_values) == field)
      form <- set_radio(form, new_values[idx])
    } else {
      form$fields[[field]]$value <- new_values[[field]]
    }
  }
  
  form
  
}

set_checkbox <- function(form, values) {
  idx <- which(unlist(lapply(form$fields, function(x) { x$name %in% names(values) })))
  
  for (i in unname(idx)) {
    if (!is.null(form$fields[[i]]$value) && (form$fields[[i]]$value %in% values))
      form$fields[[i]]$checked <- "true"
  }
  return(form)
}

set_radio <- function(form, values) {
  idx <- which(unlist(lapply(form$fields, function(x) { x$name %in% names(values) })))
  
  for (i in unname(idx)) {
    if (!is.null(form$fields[[i]]$value)) {
      if (form$fields[[i]]$value %in% values)
        form$fields[[i]]$checked <- "true"
      else
        form$fields[[i]]$checked <- "false"
    }
  }
  return(form)
}


submit_request <- function(form, submit = NULL) {
  submits <- Filter(function(x) {
    identical(tolower(x$type), "submit") | identical(tolower(x$type), "image")
  }, form$fields)
  if (is.null(submit)) {
    submit <- names(submits)[[1]]
    message("Submitting with '", submit, "'")
  }
  if (!(submit %in% names(submits))) {
    stop(
      "Unknown submission name '", submit, "'.\n",
      "Possible values: ", paste0(names(submits), collapse = ", "),
      call. = FALSE
    )
  }
  other_submits <- setdiff(names(submits), submit)
  
  # Parameters needed for http request -----------------------------------------
  method <- form$method
  if (!(method %in% c("POST", "GET"))) {
    warning("Invalid method (", method, "), defaulting to GET", call. = FALSE)
    method <- "GET"
  }
  
  url <- form$url
  
  fields <- form$fields
  fields <- Filter(function(x) length(x$value) > 0, fields)
  fields <- Filter(function(x) is.null(x$type) || ((x$type != "radio") && (x$type != "checkbox")) || (!is.null(x$type) && (x$type %in% c("checkbox", "radio")) && !is.null(x$checked) && (x$checked == "true")), fields)
  fields <- fields[setdiff(names(fields), other_submits)]
  
  values <- rvest::pluck(fields, "value")
  names(values) <- names(fields)
  
  list(
    method = method,
    encode = form$enctype,
    url = url,
    values = values
  )
}

"%||%" <- function(a, b) if (length(a) == 0) b else a

vpluck_with_default <- function(xs, i, default) {
  extract <- function(x) {
    if (i %in% names(x)) {
      x[[i]]
    } else {
      default
    }
  }
  vapply(xs, extract, FUN.VALUE = default)
}
