#' List users
#'
#' @param credentials The credentials to use. If not given, uses cached login
#'   information.
#'
#' @export
list_users <- function(credentials = NULL) {
  request(credentials, c("users")) |> dplyr::bind_rows()
}


#' Modify an existing user
#'
#' @param email email of the user to modify.
#' @param role global role of the user ("metareader", "reader", "writer" or
#'   "admin").
#' @param credentials The credentials to use. If not given, uses cached login
#'   information.
#'
#' @export
modify_user <- function(email,
                        role = "writer",
                        credentials = NULL) {
  if (!is.null(role)) role <- toupper(role)
  body = list(
    role = role
  )
  invisible(request(credentials, c("users", email), "PUT", body))
}


#' Create a new user
#'
#' @param email email of the user to add.
#' @param role global role of the user ("metareader", "reader", "writer" or
#'   "admin").
#' @param index_access index to grant access to for the new user.
#' @param credentials The credentials to use. If not given, uses cached login
#'   information.
#'
#' @export
create_user <- function(email,
                        role = "writer",
                        index_access = NULL,
                        credentials = NULL) {
  if (!is.null(role)) role <- toupper(role)
  body <- list(
    email = email, role = role,
    index_access = index_access
  )
  invisible(request(credentials, "users/", "POST", body))
}

#' Delete new user
#'
#' @param email email of the user to remove.
#' @param credentials The credentials to use. If not given, uses cached login
#'   information.
#'
#' @export
delete_user <- function(email,
                        credentials = NULL) {
  invisible(request(credentials, c("users", email), "DELETE"))
}

