#' Logon to AmCAT
#' @param host The host including schema i.e. "http://localhost:5000"
#' @param username The username to login with
#' @param password The password to login with. If not given, will prompt interactively
#' @return A named list containing host, username, and token
#' @export
login <- function(host, username, password=NULL) {
  if (is.null(password)) password = rstudioapi::askForPassword(paste("Password for", host))
  r = httr::POST(paste0(host, "/auth/token"),
                 body=list(username=username, password=password))

  httr::stop_for_status(r)
  token = httr::content(r)$access_token
  credentials = list(host=host, username=username, token=token)
  if (is.null(token)) stop("Login request unsuccessful. Did you enter the correct host address?")

  pkg.env$current = credentials
  invisible(credentials)
}

#' List users
#'
#' @export
list_users <- function(credentials=NULL) {
  do_get(credentials, c("users")) |> dplyr::bind_rows()
}


#' Modify an existing user
#'
#' @param email email of the user to modify
#' @param new_password new password for the user
#' @export
modify_user <- function(email, new_password, credentials=NULL) {
  body = list(password=new_password)
  invisible(do_put(credentials, c("users", email), body))
}

#' Create a new user
#'
#' @param email email of the user to modify
#' @param password new password for the user
#' @param global_role global role of the user ("writer" or "admin")
#' @param index_access index to grant access to for the new user
#' @export
create_user <- function(
    email, password, global_role=NULL, index_access=NULL,
    credentials=NULL) {
  body = list(email=email, password=password, global_role=global_role,
              index_access=index_access)
  invisible(do_post(credentials, c("users"), body))
}
