#' Post a codingjob to the annotation backend
#' @param codingjob The job as a (json) list
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
post_codingjob <- function(codingjob, credentials=NULL) {
  credentials = get_credentials(credentials)
  r <- httr::POST(paste0(credentials$host, "/codingjob"),
                  httr::add_headers(Authorization = paste("Bearer", credentials$token)),
                  body = codingjob, encode = "json")
  httr::stop_for_status(r)
  httr::content(r)$id
}

#' Retrieve a codingjob (including results)
#' @param id the job ID
#' @param credentials The credentials to use. If not given, uses last login information
#' @export
get_codingjob <- function(id, credentials=NULL) {
  credentials <- get_credentials(credentials)
  r <- httr::GET(paste0(credentials$host, "/codingjob/", id),
                 httr::add_headers(Authorization = paste("Bearer", credentials$token)))
  httr::stop_for_status(r)
  httr::content(r)
}
