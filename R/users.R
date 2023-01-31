#' List users
#'
#' @param credentials The credentials to use. If not given, uses cached login
#'   information.
#'
#' @export
list_users <- function(credentials=NULL) {
  request(credentials, c("users")) |> dplyr::bind_rows()
}


#' Modify an existing user
#'
#' @param email email of the user to modify.
#' @param global_role global role of the user ("writer" or "admin").
#' @param new_password new password for the user.
#' @param credentials The credentials to use. If not given, uses cached login
#'   information.
#'
#' @export
modify_user <- function(email,
                        global_role = "writer",
                        new_password = NULL,
                        credentials = NULL) {
  body = list(
    password = new_password,
    global_role = global_role
  )
  invisible(request(credentials, c("users", email), "PUT", body))
}


#' Create a new user
#'
#' @param email email of the user to add.
#' @param global_role global role of the user ("writer" or "admin").
#' @param password new password for the user.
#' @param index_access index to grant access to for the new user.
#' @param credentials The credentials to use. If not given, uses cached login
#'   information.
#'
#' @export
create_user <- function(email,
                        password = NULL,
                        global_role = "writer",
                        index_access = NULL,
                        credentials = NULL) {
  body <- list(
    email = email, password = password, global_role = global_role,
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

