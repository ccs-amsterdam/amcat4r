#' List the indexes on this server
#'
#' @param credentials The credentials to use. If not given, uses last login information
#' @return a tibble with index names and role
#' @export
list_indexes <- function(credentials=NULL) {
  do_get(credentials, c("index", "")) |> dplyr::bind_rows()
}

#' Delete an index
#'
#' @param index name of the index on this server
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
delete_index <- function(index, credentials=NULL) {
  invisible(do_delete(credentials, c("/index/", index)))
}

#' Create an index
#'
#' @param index Name of the index to create
#' @param guest_role Role for unauthorized users. Options are "admin", "writer",
#'   "reader" and "metareader".
#' @param credentials The credentials to use. If not given, uses last login
#'   information
#' @export
create_index <- function(index, guest_role=NULL, credentials=NULL) {
  body = list(name=index, guest_role=guest_role)
  invisible(do_post(credentials, c("index", ""),  body=body))
}

#' Upload documents
#'
#' @param index The index name to create.
#' @param documents A data frame with columns title, text, date, and optional
#'   other columns.
#' @param columns An optional list with data types, e.g. list(author = "keyword").
#' @param chunk_size Uploads are broken into chunks to prevent errors. Smaller
#'   chunks are less error-prone, but this also makes the upload slower.
#' @param verbose Should a progress bar be printed during upload.
#' @param credentials The credentials to use. If not given, uses last login
#'   information.
#' @export
upload_documents <- function(index,
                             documents,
                             columns = NULL,
                             chunk_size = 10000L,
                             verbose = TRUE,
                             credentials = NULL) {
  req_fields <- c("title", "date", "text") # hard coded, might change later
  if (any(sapply(req_fields, function(c) any(is.na(documents[[c]])))) |
      !all(req_fields %in% names(documents))) {
    req_fields[length(req_fields)] <- paste("and", req_fields[length(req_fields)])
    stop("The fields ", paste(req_fields, collapse = ", "), " are required and can never be NA")
  }
  # chunk uploads
  rows <- seq_len(nrow(documents))
  chunks <- split(rows, ceiling(seq_along(rows) / chunk_size))
  if (verbose & length(chunks) > 1L) pb <- progress::progress_bar$new(total = length(chunks))
  for (r in chunks) {
    if (verbose) pb$tick()
    body <- list(documents = documents[r, ])
    if (!is.null(columns)) body$columns <- lapply(columns, jsonlite::unbox)
    do_post(credentials, c("index", index, "documents"), body, auto_unbox = FALSE) |>
      invisible()
  }
}

#' List index users
#'
#' @param index The index to list
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
list_index_users <- function(index, credentials=NULL) {
  do_get(credentials, c("index", index, "users"))  |> dplyr::bind_rows()
}

#' Add index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param role The role of the user (METAREADER, READER, WRITER, ADMIN)
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
add_index_user <- function(index, email, role, credentials=NULL) {
  body = list(email=email, role=role)
  invisible(do_post(credentials, c("index", index, "users"), body=body))
}

#' Delete index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
delete_index_user <- function(index, email, credentials=NULL) {
  invisible(do_delete(credentials, c("index", index, "users", email)))
}

#' Modify index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param role The role of the user (METAREADER, READER, WRITER, ADMIN)
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
modify_index_user <- function(index, email, role, credentials=NULL) {
  body = list(role=role)
  invisible(do_put(credentials, c("index", index, "users", email), body=body))
}


#' Refresh an index
#'
#' @param index The index to refresh
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
refresh_index <- function(index, credentials=NULL) {
  invisible(do_get(credentials, c("index", index, "refresh")))
}

#' Set fields
#'
#' @param index The index to set fields for
#' @param fields A list with fields and data types, e.g. list(author="keyword")
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
set_fields <- function(index, fields, credentials=NULL) {
  invisible(do_post(credentials, c("index", index, "fields"), body=fields))
}

#' Get fields
#'
#' @param index The index to get fields for
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
get_fields <- function(index, credentials=NULL) {
  do_get(credentials, c("index", index, "fields")) |>
    purrr::map_df(function(t) tibble::tibble(name=t$name, type=t$type))
}
