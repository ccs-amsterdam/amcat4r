
pkg.env <- new.env()

#' Logon to AmCAT
#' @param host The host including schema i.e. "http://localhost:5000"
#' @param username The username to login with
#' @param password The password to login with. If not given, will prompt interactively
#' @return A named list containing host, username, and token
#' @export
login <- function(host, username, password=NULL) {
  if (is.null(password)) password = rstudioapi::askForPassword(paste("Password for", host))
  r = httr::GET(paste0(host, "/auth/token/"),
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

#' List the indexes on this server
#'
#' @param credentials The credentials to use. If not given, use last login information
#' @export
list_indexes <- function(credentials=NULL) {
 credentials = get_credentials(credentials)
  httr::GET(paste0(credentials$host, "/index"),
            httr::add_headers(Authorization = paste("Bearer", credentials$token)))
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
