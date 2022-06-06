pkg.env <- new.env()

#' Helper function to get credentials from artugment or pkg.env
get_credentials = function(credentials=NULL) {
  if (is.null(credentials)) {
    if (is.null(pkg.env$current)) stop("Please login() first")
    credentials = pkg.env$current
    message(paste0("Using connection: ",credentials$user, "@", credentials$host))
  }
  credentials
}

#' Helper function to execute a request to this API
request <- function(credentials, url, request_function=httr::GET, ...) {
  credentials = get_credentials(credentials)
  url = paste(c(credentials$host, trimws(url, whitespace="/")), collapse="/")
  r = request_function(
    url=url,
    config=httr::add_headers(Authorization = paste("Bearer", credentials$token)),
    ...)
  if (httr::status_code(r) >= 300) message(httr::content(r, as="parsed"))


  httr::stop_for_status(r)
  httr::content(r, as="parsed")
}

#' Execute a POST request to this AmCAT API, returning the json result
do_post <- function(credentials, url, body, encode="json", ...) {
  request(credentials, url, request_function=httr::POST, body=body, encode=encode, ...)
}

#' Execute a PUT request to this AmCAT API, returning the json result
do_put <- function(credentials, url, body, encode="json", ...) {
  request(credentials, url, request_function=httr::PUT, body=body, encode=encode, ...)
}

#' Execute a GET request to this AmCAT API, returning the json result
do_get <- function(credentials, url, ...) {
  request(credentials, url, request_function=httr::GET, ...)
}

#' Execute a DELETE request to this AmCAT API, returning the json result
do_delete <- function(credentials, url, ...) {
  request(credentials, url, request_function=httr::DELETE, ...)
}
