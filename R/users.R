#' List users
#'
#' @param credentials The credentials to use. If not given, uses last login information
#'
#' @export
list_users <- function(credentials=NULL) {
  do_get(credentials, c("users")) |> dplyr::bind_rows()
}


#' Modify an existing user
#'
#' @param email email of the user to modify
#' @param new_password new password for the user
#' @param credentials The credentials to use. If not given, uses last login information
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
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
create_user <- function(email,
                        password,
                        global_role = NULL,
                        index_access = NULL,
                        credentials = NULL) {
  body <- list(
    email = email, password = password, global_role = global_role,
    index_access = index_access
  )
  invisible(do_post(credentials, c("users", ""), body))
}
