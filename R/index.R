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
#' Create or modify an index.
#'
#' @param index short name of the index to create (follows naming conventions of
#'   Elasticsearch, see details).
#' @param name optional more descriptive name of the index to create (all
#'   characters are allowed here)
#' @param description optional description of the index to create
#' @param create_fields create fields in the new index.
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
create_index <- function(index,
                         name = index,
                         description = NULL,
                         create_fields = list(title = "text", date = "date", text = "text"),
                         guest_role = NULL,
                         credentials = NULL) {
  if (!is.null(guest_role)) guest_role <- toupper(guest_role)
  body <- list(id = index, name = name, description = description, guest_role = guest_role)
  resp <- request(credentials, "index/", body = body, "POST")
  if (!is.null(create_fields)) {
    set_fields(index, create_fields)
  }
  invisible(resp)
}


#' @describeIn create_index Modify an index
#' @export
modify_index <- function(index, name = index, description = NULL, guest_role = NULL, credentials = NULL) {
  if (!is.null(guest_role)) guest_role <- toupper(guest_role)
  body <- list(name = name, description = description, guest_role = guest_role)
  invisible(request(credentials, c("index/", index), body = body, "PUT"))
}


#' Upload documents
#'
#' @param index The name of the index documents should be added to.
#' @param documents A data frame with columns title, text, date, and optional
#'   other columns.
#' @param columns An optional list with data types, e.g. list(author =
#'   "keyword").
#' @param chunk_size Uploads are broken into chunks to prevent errors. Smaller
#'   chunks are less error-prone, but this also makes the upload slower.
#' @param max_tries In case something goes wrong, how often should the function
#'   retry to send the documents?
#' @param verbose Should a progress bar be printed during upload.
#' @param credentials The credentials to use. If not given, uses last login
#'   information.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' amcat_login("http://localhost/amcat")
#' docs <- data.frame(
#'   date = "2024-01-01",
#'   title = "This is a title",
#'   text = "This is some text"
#' )
#' create_index(index = "new_index")
#' upload_documents(index = "new_index",
#'                  documents = docs)
#' }
upload_documents <- function(index,
                             documents,
                             columns = NULL,
                             chunk_size = 100L,
                             max_tries = 5L,
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
  if (verbose & length(chunks) > 1L) cli::cli_progress_bar("Uploading", total = length(chunks))
  for (r in chunks) {
    if (verbose & length(chunks) > 1L) cli::cli_progress_update()
    body <- list(documents = documents[r, ])
    if (!is.null(columns)) body$columns <- lapply(columns, jsonlite::unbox)
    request(credentials, c("index", index, "documents"), "POST", body, max_tries = max_tries, auto_unbox = FALSE) |>
      invisible()
  }
  if (verbose & length(chunks) > 1L) cli::cli_progress_done()
}


#' Update documents
#'
#' @param index The index name to create.
#' @param ids The IDs (.id) of the document to update description (if NULL, the
#'   .id column from documents will be used).
#' @param documents A data frame with columns to update.
#' @param credentials The credentials to use. If not given, uses last login
#'   information.
#' @export
update_documents <- function(index,
                             ids = NULL,
                             documents,
                             credentials = NULL) {
  if (is.null(ids) && ".id" %in% colnames(documents)) ids <- documents[[".id"]]
  if (is.null(ids)) stop("id is required either in the id or documents argument")
  documents <- documents[,colnames(documents) != ".id", drop = FALSE]
  for (i in seq_along(ids)) {
    request(credentials, c("index", index, "documents", ids[i]), "PUT", body = as.list(documents[i, , drop = FALSE])) |>
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
  invisible(request(credentials, c("index", index, "fields"), "POST", body = as.list(fields)))
}


#' Get fields
#'
#' @param index The index to get fields for
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
get_fields <- function(index, credentials = NULL) {
  res <- request(credentials, c("index", index, "fields"))
  purrr::map(names(res), function(f) {
    tibble::tibble(name = f,
                   type = purrr::pluck(res[[f]], "type"),
                   elastic_type = purrr::pluck(res[[f]], "elastic_type"),
                   identifier = purrr::pluck(res[[f]], "identifier"),
                   metareader = list(purrr::pluck(res[[f]], "metareader")),
                   client_settings = list(purrr::pluck(res[[f]], "client_settings")))
  }) |>
    purrr::list_rbind()
}

