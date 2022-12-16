pkg.env <- new.env()

#' Helper function to get credentials from argument or pkg.env
#' @noRd
get_credentials = function(credentials=NULL) {

  if (is.null(credentials)) {
    if (!is.null(pkg.env$current)) {
      credentials = pkg.env$current
    } else if (!is.null(pkg.env$current_server)) {
      credentials = amcat_get_token(pkg.env$current_server)
      credentials$host <- pkg.env$current_server
    } else {
      stop("Please login() first")
    }
  }
  credentials
}

#' Helper function to execute a request to this API
#' @noRd
request <- function(credentials, url, request_function=httr::GET, error_on_404=TRUE, ...) {
  credentials = get_credentials(credentials)
  url = paste(c(credentials$host, trimws(url, whitespace="/")), collapse="/")

  token <- credentials$access_token
  if (is.null(token)) token <- credentials$token

  r = request_function(
    url=url,
    config=httr::add_headers(Authorization = paste("Bearer", token)),
    ...)
  if (httr::status_code(r) == 404 & !error_on_404) return(NULL)
  if (httr::status_code(r) >= 300) message(httr::content(r, as="parsed"))
  httr::stop_for_status(r)
  httr::content(r, as="parsed")
}

#' Execute a POST request to this AmCAT API, returning the json result
#' @noRd
do_post <- function(credentials, url, body, encode="json-auto", auto_unbox=TRUE, ...) {
  if (encode == "json-auto") {
    body = jsonlite::toJSON(body, null='null', auto_unbox=auto_unbox)
    encode = "raw"
  }
  request(credentials, url, request_function=httr::POST, body=body, encode=encode, ...)
}

#' Execute a PUT request to this AmCAT API, returning the json result
#' @noRd
do_put <- function(credentials, url, body, encode="json-auto", auto_unbox=TRUE, ...) {
  if (encode == "json-auto") {
    body = jsonlite::toJSON(body, null='null', auto_unbox=auto_unbox)
  }
  request(credentials, url, request_function=httr::PUT, body=body, encode=encode, ...)
}

#' Execute a GET request to this AmCAT API, returning the json result
#' @noRd
do_get <- function(credentials, url, ...) {
  request(credentials, url, request_function=httr::GET, ...)
}

#' Execute a DELETE request to this AmCAT API, returning the json result
#' @noRd
do_delete <- function(credentials, url, ...) {
  request(credentials, url, request_function=httr::DELETE, ...)
}
