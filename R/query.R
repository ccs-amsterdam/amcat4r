#' Conduct a query and return the resulting documents
#'
#' @param index The index to query
#' @param queries An optional vector of queries to run (implicit OR)
#' @param fields An optional vector of fields to return
#' @param filters An optional list of filters, e.g. list(publisher='A', date=list(gte='2022-01-01'))
#' @param scroll Time to keep scrolling cursor alive
#' @param per_page Number of results per page
#' @param max_pages Stop after getting this many pages. Set to 0 to retrieve all.
#' @export
query_documents <- function(
    index, queries=NULL, fields=c("date", "title"), filters=NULL,
    scroll="5m", per_page=1000, credentials=NULL,
    max_pages=1) {
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
  dplyr::bind_rows(results)
}
