if (!as.logical(Sys.getenv("amcat_offline")))
  amcat_login("http://localhost/amcat", cache = 2L)

test_that("query", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  create_index("amcat4r-test")
  set_fields("amcat4r-test", list(keyword = "keyword",
                                  cats = "keyword"))
  test_doc <- data.frame(
    .id = 1:10,
    title = "test",
    text = "test",
    date = c("2023-01-01T00:00:00", "2022-01-01T00:00:00"),
    cats = c("cute", "cute2"),
    keyword = "test"
  )
  upload_documents("amcat4r-test", documents = test_doc)
  Sys.sleep(2) # seems to take a second to work
  expect_equal(
    dim(query_documents("amcat4r-test", queries = NULL, fields = NULL)),
    c(10, 6)
  )
  expect_equal(
    dim(query_documents("amcat4r-test", queries = NULL, fields = "date")),
    c(10, 2)
  )

  expect_false(isTRUE(all.equal(
    query_documents(index = "amcat4r-test", queries = NULL, per_page = 1, page = 1, max_pages = 1),
    query_documents("amcat4r-test", queries = NULL, per_page = 1, page = 2, max_pages = 2)
  )))

  expect_length(
    query_documents("amcat4r-test", queries = NULL, per_page = 1, max_pages = 10)$.id,
    10L
  )

  expect_equal(
    colnames(
      query_aggregate("amcat4r-test",
                      axes = list(list(field="keyword",
                                       list(field="date", interval="day"))),
                      queries = "test")
    ),
    c("keyword", "n")
  )

  expect_error(
      query_aggregate("amcat4r-test",
                      axes = list(list(field="text",
                                       list(field="date", interval="day"))),
                      queries = "test",
                      filters = list(cats = "cute",
                                     date = list(gte = "1900-01-01"))),
    "Aggregation axes need to be either date or keyword"
  )

  expect_equal(
    query_aggregate("amcat4r-test",
                    axes = list(list(field="keyword", list(field="date", interval="day"))),
                    queries = "test",
                    filters = list(cats = "cute",
                                   date = list(gte = "1900-01-01")))$n,
    5
  )

  test_doc$date[1] <- "2022-01-01T00:00:00"
  upload_documents("amcat4r-test", documents = test_doc)
  Sys.sleep(2) # seems to take a second to work
  expect_equal(
    query_aggregate("amcat4r-test",
                    axes = list(list(field="keyword", list(field="date", interval="day"))),
                    queries = "test",
                    filters = list(cats = "cute",
                                   date = list(gte = "2023-01-01")))$n,
    5L
  )

  expect_equal({
    set_fields("amcat4r-test", list(test = "tag"))
    # TODO: remove, admin should have automatic access
    add_index_user("amcat4r-test", email = "_admin", role = "ADMIN")
    update_tags(
      index = "amcat4r-test",
      action = "add",
      field = "test",
      tag = "test",
      filters = list(cats = "cute",
                     date = list(gte = "2023-01-01"))
    )
    Sys.sleep(2) # seems to take a second to work
    sum(is.na(query_documents("amcat4r-test", queries = NULL, fields = c("test", "title"), scroll = "1m")))},
    5L
  )

  expect_equal({
    update_tags(
      index = "amcat4r-test",
      action = "remove",
      field = "test",
      tag = "test",
      filters = list(cats = "cute",
                     date = list(gte = "2023-01-01"))
    )
    Sys.sleep(2) # seems to take a second to work
    # column test should not be returned as all empty
    sum(is.na(query_documents("amcat4r-test", queries = NULL, fields = c("test", "title"))))},
    0L
  )

  delete_index("amcat4r-test")
})
