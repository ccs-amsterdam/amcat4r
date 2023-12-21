pkg.env <- new.env()

#' Helper function to get credentials from argument or pkg.env
#' @noRd
get_credentials = function(credentials = NULL) {

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
request_response <- function(credentials,
                             url,
                             method = "GET",
                             body = NULL,
                             error_on_404 = TRUE,
                             max_tries = NULL,
                             auto_unbox = TRUE,
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
                                       httr2::resp_status(resp) >= 400),
      body = amcat_error_body
    ) |>
    # for uploads, we sometimes get 500/502 when elastic is processing new documents
    # in these cases amcat4 reports a server error because the connection times out.
    # It makes sense to wait a little and retry
    httr2::req_retry(max_tries = max_tries,
                     is_transient = function(x) httr2::resp_status(x) %in% c(429, 500, 502, 503))

  if (!is.null(body)) {
    req <- req |>
      httr2::req_body_json(body, auto_unbox = auto_unbox)
  }

  if (credentials$authorization != "no_auth") {
    req <- req |>
      httr2::req_auth_bearer_token(credentials$access_token)
  }

  httr2::req_perform(req)
}

#' Make a request and return the body
#' @noRd
request <- function(...) {
  resp = request_response(...)
  if (length(resp[["body"]]) > 0) {
    return(httr2::resp_body_json(resp))
  } else {
    invisible(NULL)
  }
}

#' Hanlde path for httr2 0.2.2 and below
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
amcat_error_body <- function(resp) {

  if (grepl("json", httr2::resp_content_type(resp), fixed = TRUE)) {
    ebody <- httr2::resp_body_json(resp)

    if (is.list(ebody$detail$body$error)) {
      error <- purrr::map_chr(names(ebody$detail$body$error), function(n) {
        paste0(tools::toTitleCase(n), ": ", ebody$detail$body$error[[n]])
      })
    } else {
      # TODO: find a cleaner way to parse this
      msg <- try(ebody[["detail"]][[1]][["msg"]], silent = TRUE)
      if (methods::is(msg, "try-error")) msg <- NULL
      detail <- try(toString(ebody[["detail"]][[1]][["loc"]]), silent = TRUE)
      if (methods::is(detail, "try-error")) detail <- toString(ebody[["detail"]])
      error <- paste0(msg, detail, .sep = ": ")
    }

  } else {
    # if no further information is returned, revert to httr2 default by
    # returning NULL
    error <- NULL
  }

  if (httr2::resp_status(resp) == 401)
    error <- glue::glue(error, " (hint: see ?amcat_login on how to get a fresh token)")

  return(error)
}


#' Helper function to convert date columns in date format
#' @noRd
convert_datecols <- function(df, index) {
  type <- NULL
  datecols <- dplyr::filter(get_fields(index), type == "date")$name

  for (date_col in intersect(colnames(df), datecols)) {
    # AmCAT / elastic does not standardize date input/output, so try different formats
    # (and maybe complain to whoever is in charge of AmCAT?)
    df[[date_col]] <- lubridate::parse_date_time(df[[date_col]], orders=c("ymdHMSz", "ymdHMS", "ymdHM", "ymd"))
  }
  df
}


#' Truncate id columns when printing
#'
#' @param x id column in a data.frame with amcat4 data.
#' @inheritParams rlang::args_dots_used
#'
#' @export
#' @importFrom pillar pillar_shaft
#' @method pillar_shaft id_col
pillar_shaft.id_col <- function(x, ...) {
  x <- ifelse(
    nchar(x) > 7,
    paste0(substr(x, 1, 3), '\u2026', substr(x, nchar(x)-2, nchar(x))),
    x
  )
  pillar::pillar_shaft(x)
}


#' @title Check if an amcat instance is reachable
#'
#' @description Check if a server is reachable by sending a request to its
#'   config endpoint.
#'
#' @param server A character string of the server URL. If missing the server for
#'   the logged in session is tried.
#'
#' @return A logical value indicating if the server is reachable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ping("http://localhost/amcat")
#' }
ping <- function(server) {
  if (missing(server)) server <- pkg.env$current_server
  tryCatch({
    httr2::request(server) |>
      httr2::req_url_path_append("config") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform() |>
      (\(resp) !is.null(httr2::resp_body_json(resp)$resource))()
  }, error = function(resp) FALSE)
}


#' Helper function to safely turn results into a tibble without unnesting list fields
#' @noRd
safe_bind_rows <- function(l) {
  purrr::map(l, function(tbl) {
    purrr::map(tbl, function(c) {
      if (is.list(c) & length(c) > 1) {
        return(list(c))
      } else {
        return(c)
      }
    }) |>
      tibble::as_tibble()
  }) |>
    dplyr::bind_rows()
}

