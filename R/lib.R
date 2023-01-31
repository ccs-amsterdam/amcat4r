pkg.env <- new.env()

#' Helper function to get credentials from argument or pkg.env
#' @noRd
get_credentials = function(credentials=NULL) {

  if (is.null(credentials)) {
    if (!is.null(pkg.env$current_server)) {
      credentials = amcat_get_token(pkg.env$current_server)
      credentials$host <- pkg.env$current_server
    } else {
      stop("Please use amcat_login() first")
    }
  }
  credentials
}

#' Helper function to execute a request to this API
#' @noRd
request <- function(credentials,
                    url,
                    method = "GET",
                    body = NULL,
                    error_on_404 = TRUE,
                    ...) {

  # current httr2 version has a bug in req_url_path that can't handle objects of
  # length != 1, already fixed on gh
  if (utils::packageVersion("httr2") <= "0.2.2") url <- make_path(url)

  credentials <- get_credentials(credentials)

  req <- httr2::request(credentials$host) |>
    httr2::req_url_path_append(url) |>
    httr2::req_method(method) |>
    httr2::req_error(
      is_error = function(resp) ifelse(httr2::resp_status(resp) == 404L,
                                       error_on_404 ,
                                       httr2::resp_status(resp) >= 300),
      body = error_body
    )

  if (!is.null(body)) {
    req <- req |>
      httr2::req_body_json(body)
  }

  if (credentials$authorization != "no_auth") {
    req <- req |>
      httr2::req_auth_bearer_token(credentials$access_token)
  }

  resp <- httr2::req_perform(req)
  if (length(resp[["body"]]) > 0) {
    return(httr2::resp_body_json(resp))
  } else {
    invisible(NULL)
  }
}


#' Custom error message for requests
#' @noRd
make_path <- function(...) {
  path <- paste(c(...), collapse = "/")
  # Ensure we don't add duplicate /s
  if (path != "" && !grepl("^/", path)) {
    path <- paste0("/", path)
  }

  path
}


#' Custom error message for requests
#' @noRd
error_body <- function(resp) {
  if (httr2::resp_content_type(resp) == "json") {
    error <- httr2::resp_body_json(resp)$error
  } else {
    error <- glue::glue("HTTP {httr2::resp_status(resp)} {httr2::resp_status_desc(resp)}."                        )
  }

  if (httr2::resp_status(resp) == 401)
    error <- glue::glue(error, " (hint: see ?amcat_login on how to get a fresh token)")
  cli::cli_abort(error)
}
