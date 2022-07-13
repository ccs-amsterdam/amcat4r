

#' List the indexes on this server
#'
#' @param credentials The credentials to use. If not given, use last login information
#' @return a tibble with index names and role
#' @export
list_indexes <- function(credentials=NULL) {
  do_get(credentials, "index") |> dplyr::bind_rows()
}

#' Delete an index
#'
#' @param credentials The credentials to use. If not given, use last login information
#' @export
delete_index <- function(index, credentials=NULL) {
  invisible(do_delete(credentials, c("/index/", index)))
}

#' Create an index
#'
#' @param index Name of the index to create
#' @param credentials The credentials to use. If not given, use last login information
#' @export
create_index <- function(index, guest_role=NULL, credentials=NULL) {
  body = list(name=index, guest_role=guest_role)
  invisible(do_post(credentials, "/index/",  body=body))
}

#' Upload documents
#'
#' @param index The index name to create
#' @param documents A data frame with columns title, text, date, and optional other columns
#' @param columns An optional list with data types, e.g. list(author="keyword")
#' @export
upload_documents <- function(index, documents, columns=NULL, credentials=NULL) {
  body = list(documents=documents)
  if (!is.null(columns)) body$columns = lapply(columns, jsonlite::unbox)
  do_post(credentials, c("index", index, "documents"), body, auto_unbox=FALSE) |>
    invisible()
}

#' List index users
#'
#' @param index The index to list
#' @export
list_index_users <- function(index, credentials=NULL) {
  do_get(credentials, c("index", index, "users"))  |> dplyr::bind_rows()
}

#' Add index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param role The role of the user (METAREADER, READER, WRITER, ADMIN)
#' @export
add_index_user <- function(index, email, role, credentials=NULL) {
  body = list(email=email, role=role)
  invisible(do_post(credentials, c("index", index, "users"), body=body))
}

#' Delete index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @export
delete_index_user <- function(index, email, credentials=NULL) {
  invisible(do_delete(credentials, c("index", index, "users", email)))
}

#' Modify index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param role The role of the user (METAREADER, READER, WRITER, ADMIN)
#' @export
modify_index_user <- function(index, email, role, credentials=NULL) {
  body = list(role=role)
  invisible(do_put(credentials, c("index", index, "users", email), body=body))
}


#' Refresh an index
#'
#' @param index The index to refresh
#' @export
refresh_index <- function(index, credentials=NULL) {
  invisible(do_get(credentials, c("index", index, "refresh")))
}

#' Set fields
#'
#' @param index The index to set fields for
#' @param fields A list with fields and data types, e.g. list(author="keyword")
#' @export
set_fields <- function(index, fields, credentials=NULL) {
  invisible(do_post(credentials, c("index", index, "fields"), body=fields))
}
