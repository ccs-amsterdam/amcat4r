#' Conduct a query and return the resulting documents
#'
#' This function queries the database and retrieves documents that fit the
#' query.
#'
#' @param index The index to query.
#' @param queries An optional vector of queries to run (implicit OR).
#' @param fields An optional vector of fields to return (returns all fields if
#'   NULL).
#' @param filters An optional list of filters, e.g. \code{list(publisher='A',
#'   date=list(gte='2022-01-01')}).
#' @param per_page Number of results per page.
#' @param max_pages Stop after getting this many pages. Set to \code{Inf} to
#'   retrieve all.
#' @param page Request a specific page (is ignored when \code{scroll} is set).
#' @param scroll Instead of scrolling indefinitely until max_pages is reached,
#'   you can set a time here that amcat4r keeps retrieving new pages before it
#'   stops (see examples).
#' @param merge_tags Character to merge tag fields with, default ';'. Set to
#'   NULL to prevent merging.
#' @param credentials The credentials to use. If not given, uses last login
#'   information
#'
#'
#' @details This function queries the database and retrieves documents that fit the
#' query. The results can be further narrowed down using filters. If there are
#' many results, they are divided into pages to keep the data that is sent from
#' the amcat instance small. You can use the function to iterate over these
#' pages to retrieve many or all or just a specific one (if you want to batch
#' process an index and only work on, e.g., 100 documents at a time).
#'
#' AmCAT uses the Elasticsearch query language. Find the documentation here:
#' \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-query-notes}.
#'
#' @examples
#' \dontrun{
#' # retrieve all fields from all documents
#' query_documents("state_of_the_union", queries = NULL, fields = NULL)
#'
#' # query "migration" and select text field
#' query_documents("state_of_the_union", queries = "migration", fields = "text")
#'
#' # note that by default, the query searches all text fields (see ?get_fields for field types)
#' query_documents("state_of_the_union", queries = "1908", fields = "text")
#'
#' # to narrow a search to the title field use
#' query_documents("state_of_the_union", queries = "title:1908", fields = "text")
#'
#' # searches support wild cards
#' query_documents("state_of_the_union", queries = "migra*", fields = NULL)
#'
#' # if you query more than one term, you can use OR or leave it out since it is
#' # used implicitly anyway. So these two do the same
#' query_documents("state_of_the_union", queries = "migra* OR refug*")
#' query_documents("state_of_the_union", queries = "migra* refug*")
#'
#' # you can search for literal matches using double quotes
#' query_documents("state_of_the_union", queries = '"migration laws"')
#'
#' # and you can chain several boolean operators together
#' query_documents("state_of_the_union", queries = "(migra* OR refug*) AND illegal NOT legal")
#'
#' # get only the first result
#' query_documents("state_of_the_union", queries = "migra*", per_page = 1, page = 1, fields = NULL)
#'
#' # get the 81st resutl
#' query_documents("state_of_the_union", queries = "migra*", per_page = 80, page = 2, fields = NULL)
#'
#' # If you want to retrieve many pages/documents at once, you should use the
#' scroll API by setting a scroll value. E.g., to scroll for 5 seconds before
#' collecting results use:
#' query_documents("state_of_the_union", scroll = "5s", per_page = 1, max_pages = Inf)
#' # or scroll for 5 minutes
#' query_documents("state_of_the_union", scroll = "5m", per_page = 1, max_pages = Inf)
#' }
#' @export
query_documents <- function(index,
                            queries = NULL,
                            fields = c("date", "title"),
                            filters = NULL,
                            per_page = 200,
                            max_pages = 1,
                            page = NULL,
                            merge_tags = ";",
                            scroll = NULL,
                            verbose = TRUE,
                            credentials = NULL) {
  types <- get_fields(index)
  convert_tags <- function(row) {
    for (tag_col in intersect(names(row), types$name[types$type == "tag"])) {
      row[tag_col] <- paste(row[[tag_col]], collapse = merge_tags)
    }
    row
  }

  if (!is.null(page)) {
    page <- page - 1 # to get to 1-based numbering
    if (per_page * max_pages > 10000) {
      max_pages_old <- max_pages
      max_pages <- 10000 %/% per_page
      cli::cli_alert_warning(
        c("You requested more than 10 000 results {per_page} * {max_pages_old} ",
          "(per_page * max_pages) = {per_page * max_pages}, which will not ",
          "work. If you want more than 10 000 documents, you need to use the ",
          "{.emph scroll API}, e.g., by setting {.code scroll=\"5m\"}. For now, ",
          "you will only ge the first {max_pages} pages.")
      )
    }
  }
  if (length(fields) == 1) fields <- list(fields)
  body <- list(
    queries = queries, fields = fields, filters = filters,
    scroll = scroll, per_page = per_page, page = page
  ) |> Filter(f=Negate(is.null))
  if (verbose) {
    new_results <- results <- numeric()
    cli::cli_progress_step("Retrieved {nrow(new_results)} results from page {length(results)}",
                           spinner = TRUE)
  }

  results <- list()
  while (TRUE) {

    resp <- request_response(credentials, c("index", index, "query"),
                             method = "POST", body = body, error_on_404 = FALSE)

    if (resp$status_code == 404) break
    r <- httr2::resp_body_json(resp)
    new_results <- r$results
    if (!is.null(merge_tags)) new_results <- purrr::map(new_results, convert_tags)
    new_results <- safe_bind_rows(new_results)
    results <- append(results, list(new_results))

    if (verbose)
      cli::cli_progress_update()

    if (length(results) >= max_pages) break

    # there are two ways of scrolling through pages: using scroll(_id) or
    # requesting a specific page. scroll takes precedence in the API, hence
    # when scroll != NULL, page is ignored
    if (is.null(scroll)) {
      r <<- r
      body$page <- body$page + 1
      # for when user sets page = NULL
      if (length(body$page) == 0) {
        body$page <- 1L
      }
      if (isTRUE(body$page >= r$meta$page_count)) break
    } else {
      body$scroll_id <- r$meta$scroll_id
    }
  }

  d <- dplyr::bind_rows(results)
  if ("_id" %in% colnames(d)) {
    d <- dplyr::rename(d, .id = "_id")
    class(d$.id) <- c("id_col", class(d$.id))
  }
  convert_datecols(d, index)
}


#' Conduct a query and return the resulting documents
#'
#' @param index The index to query
#' @param axes The aggregation axes, e.g. list(list(field="publisher", list(field="date", interval="year")))
#' @param queries An optional vector of queries to run (implicit OR)
#' @param filters An optional list of filters, e.g. list(publisher='A', date=list(gte='2022-01-01'))
#' @param credentials The credentials to use. If not given, uses last login information
#'
#' @examples
#' \dontrun{
#' query_aggregate("state_of_the_union",
#'                 axes = list(list(field="party", list(field="date", interval="year"))),
#'                 queries = c("war", "peace"),
#'                 filters = list(party = c("Democratic", "Republican"),
#'                                date = list(gte = "1900-01-01")))
#' }
#' @export
query_aggregate <- function(index, axes=NULL,
                            queries = NULL,
                            filters = NULL,
                            credentials = NULL) {

  name <- NULL
  # axes must be keyword or date #9: check
  ax_fields <- unlist(axes)[names(unlist(axes)) == "field"]
  if (!all(dplyr::filter(get_fields(index), name %in%
                        ax_fields)$type %in% c("date", "keyword"))) {
    cli::cli_abort(paste(
      "Aggregation axes need to be either date or keyword fields.",
      "Check the field types with {.fn get_field}"
    ))
  }
  body <- list(axes = axes, queries = queries, filters = filters)
  r <- request(credentials, c("index", index, "aggregate"), method = "POST", body = body)
  d <- dplyr::bind_rows(r$data)
  if ("_query" %in% colnames(d)) d <- dplyr::rename(d, .query = "_query")
  convert_datecols(d, index)
}


#' Add or remove tags to/from documents by query or ID
#'
#' @param index The index to query
#' @param field The tag field name
#' @param tag The tag to add or remove
#' @param action 'add' or 'remove' the tags
#' @param queries An optional vector of queries to run (implicit OR)
#' @param filters An optional list of filters, e.g. list(publisher='A', date=list(gte='2022-01-01'))
#' @param ids A vector of ids to add/remove tags from
#' @param credentials The credentials to use. If not given, uses last login information
#'
#' @examples
#' \dontrun{
#' set_fields("state_of_the_union", list(test = "tag"))
#' update_tags(
#'   index = "state_of_the_union",
#'   action = "add",
#'   field = "test",
#'   tag = "test",
#'   filters = list(party = "Republican",
#'                  date = list(gte = "2000-01-01"))
#' )
#' }
#' @export
update_tags <- function(index, action, field, tag, ids = NULL, queries = NULL, filters = NULL, credentials = NULL) {
  body <- list(field = field, action = action, tag = tag, ids = ids, queries = queries, filters = filters)
  request(credentials, c("index", index, "tags_update"), method = "POST", body = body)
  invisible(TRUE)
}


#' Delete documents by query
#'
#' @param index The index to query
#' @param queries An optional vector of queries to run (implicit OR)
#' @param filters An optional list of filters, e.g. list(publisher='A', date=list(gte='2022-01-01'))
#' @param ids A optional vector of ids to add/remove tags from
#' @examples
#' \dontrun{
#'  delete_by_query("my_index", filters=list(publisher='NY Times'))
#'  delete_by_query("my_index", ids=c(42, 69))
#'  delete_by_query("my_index", queries="advertisement")
#' }
#' @export
delete_by_query <- function(index, ids = NULL, queries = NULL, filters = NULL, credentials = NULL) {
  body <- list(queries = queries, filters = filters)
  if (!is.null(ids)) {
    body$ids = purrr::map(as.list(ids), as.character)
  }
  res <- request(credentials, c("index", index, "delete_by_query"), method="POST", body=body)
  if (length(res$failures) > 0) {
    warning(res$failures)
    warning("Some documents failed to delete! See message above or the function result for details")
  }
  message(str_glue("Deleted {res$deleted} documents from index {index}"))
  invisible(res)
}

#' Update documents by query
#'
#' @param index The index to query
#' @param field The field name to update
#' @param value The new value for the field
#' @param queries An optional vector of queries to run (implicit OR)
#' @param filters An optional list of filters, e.g. list(publisher='A', date=list(gte='2022-01-01'))
#' @param ids A optional vector of ids to add/remove tags from
#' @examples
#' \dontrun{
#'    update_by_query("my_index", "publisher", "NYT", filters=list(publisher='New York Times'))
#'    update_by_query("my_index", "sentiment", -1, ids=c(3, 7, 9, 11))
#' }
#' @export
update_by_query <- function(index, field, value, ids = NULL, queries = NULL, filters = NULL, credentials = NULL) {
  body <- list(field=field, value=value, queries = queries, filters = filters)
  if (!is.null(ids)) {
    body$ids = purrr::map(as.list(ids), as.character)
  }
  res <- request(credentials, c("index", index, "update_by_query"), method="POST", body=body)
  if (length(res$failures) > 0) {
    warning(res$failures)
    warning("Some documents failed to update! See message above or the function result for details")
  }
  message(str_glue("Updated {res$updated} documents from index {index}"))
  invisible(res)
}


