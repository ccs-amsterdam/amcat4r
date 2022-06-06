#' Conduct a query and return the resulting documents
#'
#' @param index The index to query
#' @param queries An optional vector of queries to run (implicit OR)
#' @param fields An optional vector of fields to return
#' @param filters An optional list of filters, e.g. list(publisher='A', date=list(gte='2022-01-01'))
#' @param scroll Time to keep scrolling cursor alive
#' @param per_page Number of results per page
#' @param max_pages Stop after getting this many pages. Set to 0 to retrieve all.
#' @param credentials The credentials to use. If not given, use last login information
#' @export
query_documents <- function(
    index, queries=NULL, fields=c("date", "title"), filters=NULL,
    scroll="5m", per_page=1000, credentials=NULL,
    max_pages=1) {
  #TODO: convert dates into Date? <- could check field types. OTOH, maybe return table in more sensible format?
  body <- list(
    queries=queries, fields=fields, filters=filters,
    scroll=scroll, per_page=per_page)
  #TODO: I think there's a better way to create a result set from pages, right Kasper?
  results = list()
  while (T) {
    r = do_post(credentials, c("index", index, "query"), body=body, error_on_404=FALSE)
    if (is.null(r)) break
    new_results = dplyr::bind_rows(r$results)
    results <- append(results, list(new_results))
    message(paste0("Retrieved ", nrow(new_results), " results in ", length(results), " pages"))
    if (max_pages > 0 & length(results) >= max_pages) break
    body$scroll_id <- r$meta$scroll_id
  }
  d = dplyr::bind_rows(results)
  if ("_id" %in% colnames(d)) d <- rename(d, .id=`_id`)

  d
}

#' Conduct a query and return the resulting documents
#'
#' @param index The index to query
#' @param axes The aggregation axes, e.g. list(list(field="publisher", list(field="date", interval="year")))
#' @param queries An optional vector of queries to run (implicit OR)
#' @param filters An optional list of filters, e.g. list(publisher='A', date=list(gte='2022-01-01'))
#' @param credentials The credentials to use. If not given, use last login information
#' @export
query_aggregate <- function(index, axes, queries=NULL, filters=NULL, credentials=NULL) {
  #TODO: convert dates into Date? <- could check field types. OTOH, maybe return table in more sensible format?
  body = list(axes=axes, queries=queries, filters=filters)
  r = do_post(credentials, c("index", index, "aggregate"), body=body)
  d = dplyr::bind_rows(r$data)
  if ("_query" %in% colnames(d)) d <- rename(d, .query=`_query`)
  d
}

