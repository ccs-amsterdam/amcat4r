#' Logon to AmCAT
#' @param host The host including schema i.e. "http://localhost:5000"
#' @param username The username to login with
#' @param password The password to login with. If not given, will prompt interactively
#' @return A named list containing host, username, and token
#' @export
login <- function(host, username, password=NULL) {

  # Display as soon as the new auth is implemented everywhere
  # .Deprecated("amcat_auth", paste("The old AmCAT authentication method is currently phased out.",
  #                                 "You can still use this way of authenticating as long as you ",
  #                                 "do not update your amcat4 server."))
  #

  if (is.null(password)) {
    if (rstudioapi::isAvailable()) {
      password = rstudioapi::askForPassword(paste("Password for", host))
    } else {
      stop("password is missing")
    }
  }

  r = httr::POST(paste0(host, "/auth/token"),
                 body=list(username=username, password=password))

  httr::stop_for_status(r)
  token = httr::content(r)$access_token
  credentials = list(host=host, username=username, token=token)
  if (is.null(token)) stop("Login request unsuccessful. Did you enter the correct host address?")

  class(credentials) <- c("amcat.credentials", class(credentials))
  pkg.env$current = credentials
  invisible(credentials)
}

#' Test amcat4r credentials object
#'
#' Test amcat4r credentials object as returned by \code{login()}
#'
#' @param credentials amcat4r credentials object
#'
#' @export
test_credentials <- function(credentials) {
  if (methods::is(credentials, "amcat.credentials") &
      length(credentials) == 3L) {
    message("token is valid for host ", credentials$host)
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}
