#' List the indexes on this server
#'
#' @param credentials The credentials to use. If not given, uses last login
#'   information.
#'
#' @return a tibble with index names and guest_role
#'
#' @examples \dontrun{
#' list_indexes()
#' }
#' @export
list_indexes <- function(credentials = NULL) {
  request(credentials, "index/") |> dplyr::bind_rows()
}


#' Delete an index
#'
#' @param index name of the index on this server
#' @param credentials The credentials to use. If not given, uses last login information
#'
#' @examples \dontrun{
#' delete_index("test_index")
#' }
#'
#' @export
delete_index <- function(index, credentials = NULL) {
  invisible(request(credentials, c("index", index), method = "DELETE"))
}


#' Create an index
#'
#' @param index short name of the index to create (follows naming conventions of
#'   Elasticsearch, see details).
#' @param name optional more descriptive name of the index to create (all
#'   characters are allowed here)
#' @param description optional description of the index to create
#' @param guest_role Role for unauthorized users. Options are "admin", "writer",
#'   "reader" and "metareader".
#' @param credentials The credentials to use. If not given, uses last login
#'   information.
#'
#' @details The short name for the new index (index argument) must meet these
#'   criteria:
#'
#' - Lowercase only
#' - Cannot include `\`, `/`, `*`, `?`, `"`, `<`, `>`, `|`, `:`, ` `(space), `,` (comma), `#`
#' - Cannot start with -, _, +
#' - Cannot be `.` or `..`
#' - Cannot be longer than 255 character (note that some symbols like emojis take up tw characters)
#' - If names start with ., the index will be hidden and non accesible
#' @md
#'
#' @examples \dontrun{
#' create_index("test_index")
#' }
#'
#' @export
create_index <- function(index, name=index, description = NULL, guest_role = NULL, credentials = NULL) {
  if (!is.null(guest_role)) guest_role <- tolower(guest_role)
  body <- list(id=index, name = name, description = description, guest_role = guest_role)
  invisible(request(credentials, "index/", body = body, "POST"))
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
                             chunk_size = 100L,
                             verbose = TRUE,
                             credentials = NULL) {
  req_fields <- c("title", "date", "text") # hard coded, might change later
  if (any(sapply(req_fields, function(c) any(is.na(documents[[c]])))) |
    !all(req_fields %in% names(documents))) {
    req_fields[length(req_fields)] <- paste("and", req_fields[length(req_fields)])
    stop("The fields ", paste(req_fields, collapse = ", "), " are required and can never be NA")
  }
  if (".id" %in% colnames(documents)) colnames(documents) <- gsub(".id", "_id", colnames(documents), fixed = TRUE)
  # chunk uploads
  rows <- seq_len(nrow(documents))
  chunks <- split(rows, ceiling(seq_along(rows) / chunk_size))
  if (verbose & length(chunks) > 1L) pb <- progress::progress_bar$new(total = length(chunks))
  for (r in chunks) {
    if (verbose & length(chunks) > 1L) pb$tick()
    body <- list(documents = documents[r, ])
    if (!is.null(columns)) body$columns <- lapply(columns, jsonlite::unbox)
    request(credentials, c("index", index, "documents"), "POST", body, auto_unbox = FALSE) |>
      invisible()
  }
}


#' Update documents
#'
#' @param index The index name to create.
#' @param id The ID (.id) of the document to update description (if NULL, the
#'   .id column from document will be used).
#' @param documents A data frame with columns to update.
#' @param credentials The credentials to use. If not given, uses last login
#'   information.
#' @export
update_documents <- function(index,
                             id = NULL,
                             documents,
                             credentials = NULL) {
  if (is.null(id) && ".id" %in% colnames(documents)) id <- documents[[".id"]]
  if (is.null(id)) stop("id is required either in the id or documents argument")
  documents$.id <- NULL
  bodies <- as.list(documents)
  for (i in seq_along(bodies)) {
    request(credentials, c("index", index, "documents", id), "PUT", body = bodies[1]) |>
      invisible()
  }
}


#' Delete documents from index
#'
#' @param index The index name in which documents should be deleted.
#' @param docid the .ids of the documents that should be deleted.
#' @param credentials The credentials to use. If not given, uses last login
#'   information.
#'
#' @export
delete_documents <- function(index,
                             docid,
                             credentials = NULL) {
  invisible(lapply(docid, function(id) request(credentials, c("index", index, "documents", id), "DELETE")))
}


#' List index users
#'
#' @param index The index to list
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
list_index_users <- function(index, credentials = NULL) {
  request(credentials, c("index", index, "users")) |> dplyr::bind_rows()
}


#' Add index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param role The role of the user (METAREADER, READER, WRITER, ADMIN)
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
add_index_user <- function(index, email, role, credentials = NULL) {
  body <- list(email = email, role = toupper(role))
  invisible(request(credentials, c("index", index, "users"), "POST", body = body))
}


#' Delete index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
delete_index_user <- function(index, email, credentials = NULL) {
  invisible(request(credentials, c("index", index, "users", email), "DELETE"))
}


#' Modify index user
#'
#' @param index The index to list
#' @param email The email of an (existing) user
#' @param role The role of the user (METAREADER, READER, WRITER, ADMIN)
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
modify_index_user <- function(index, email, role, credentials = NULL) {
  body <- list(role = toupper(role))
  invisible(request(credentials, c("index", index, "users", email), "PUT", body = body))
}


#' Refresh an index
#'
#' @param index The index to refresh
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
refresh_index <- function(index, credentials = NULL) {
  invisible(request(credentials, c("index", index, "refresh")))
}


#' Set fields
#'
#' @param index The index to set fields for
#' @param fields A list with fields and data types, e.g. list(author="keyword")
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
set_fields <- function(index, fields, credentials = NULL) {
  invisible(request(credentials, c("index", index, "fields"), "POST", body = fields))
}


#' Get fields
#'
#' @param index The index to get fields for
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
get_fields <- function(index, credentials = NULL) {
  request(credentials, c("index", index, "fields")) |>
    purrr::map_df(function(t) tibble::tibble(name = t$name, type = t$type))
}

