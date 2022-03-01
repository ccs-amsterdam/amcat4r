
pkg.env <- new.env()

#' Logon to AmCAT
#' @param host The host including schema i.e. "http://localhost:5000"
#' @param username The username to login with
#' @param password The password to login with. If not given, will prompt interactively
#' @return A named list containing host, username, and token
#' @export
login <- function(host, username, password=NULL) {
  if (is.null(password)) password = rstudioapi::askForPassword(paste("Password for", host))
  r = httr::GET(paste0(host, "/auth/token"),
                httr::authenticate(username, password))

  httr::stop_for_status(r)
  token = httr::content(r)$token
  credentials = list(host=host, username=username, token=token)

  pkg.env$current = credentials
  invisible(credentials)
}

get_credentials = function(credentials=NULL) {
  if (is.null(credentials)) {
    if (is.null(pkg.env$current)) stop("Please login() first")
    credentials = pkg.env$current
    message(paste0("Using connection: ",credentials$user, "@", credentials$host))
  }
  credentials
}

request <- function(url, request_function=httr::GET, credentials=NULL, ...) {
  credentials = get_credentials(credentials)
  r = request_function(paste0(credentials$host, url),
                httr::add_headers(Authorization = paste("Bearer", credentials$token)), ...)
  httr::stop_for_status(r)
  invisible(httr::content(r, as="parsed"))
}

#' List the indexes on this server
#'
#' @param credentials The credentials to use. If not given, use last login information
#' @return a tibble with index names and role
#' @export
list_indexes <- function(credentials=NULL) {
  request("/index", credentials=credentials) |> dplyr::bind_rows()
}

#' Delete an index
#'
#' @param credentials The credentials to use. If not given, use last login information
#' @export
delete_index <- function(index, credentials=NULL) {
  request(paste0("/index/", index), request_function = httr::DELETE, credentials = credentials)
}

#' Create an index
#'
#' @param index Name of the index to create
#' @param credentials The credentials to use. If not given, use last login information
#' @export
create_index <- function(index, credentials=NULL) {
  body = list(name=index)
  request("/index/", request_function = httr::POST, body=body, encode="json", credentials = credentials)
}

#' Upload documents
#'
#' @param index The index name to create
#' @param documents A data frame with columns title, text, date, and optional other columns
#' @param columns An optional list with data types, e.g. list(author="keyword")
#' @export
upload_documents <- function(index, documents, columns=NULL, credentials=NULL) {
  body = list(documents=documents)
  if (!is.null(columns)) body$columns = columns
  body = jsonlite::toJSON(body, null='null')
  request(paste0("/index/", index, "/documents"), request_function=httr::POST, body=body, encode="raw", credentials=credentials)
}

#' Post a codingjob to the annotation backend
#' @param codingjob The job as a (json) list
#' @param credentials The credentials to use. If not given, use last login information
#' @export
post_codingjob <- function(codingjob, credentials=NULL) {
  credentials = get_credentials(credentials)
  r <- httr::POST(paste0(credentials$host, "/codingjob"),
                  httr::add_headers(Authorization = paste("Bearer", credentials$token)),
                  body = codingjob, encode = "json")
  httr::stop_for_status(r)
  httr::content(r)$id
}

#' Retrieve a codingjob (including results)
#' @param id the job ID
#' @param credentials The credentials to use. If not given, use last login information
#' @export
get_codingjob <- function(id, credentials=NULL) {
  credentials <- get_credentials(credentials)
  r <- httr::GET(paste0(credentials$host, "/codingjob/", id),
                 httr::add_headers(Authorization = paste("Bearer", credentials$token)))
  httr::stop_for_status(r)
  httr::content(r)
}
