test_that("query", {
  setup_test()
  test_doc <- data.frame(
    .id = 1:10,
    title = "test",
    text = "test",
    date = c("2023-01-01T00:00:00", "2022-01-01T00:00:00"),
    cats = c("cute", "cute2"),
    keyword = "test"
  )
  upload_documents(amcat_test_index, documents = test_doc)
  refresh_index(amcat_test_index)
  expect_equal(
    dim(query_documents(amcat_test_index, queries = NULL, fields = NULL)),
    c(10, 6)
  )
  expect_equal(
    dim(query_documents(amcat_test_index, queries = NULL, fields = "date")),
    c(10, 2)
  )

  expect_false(isTRUE(all.equal(
    query_documents(index = amcat_test_index, queries = NULL, per_page = 1, page = 1, max_pages = 1),
    query_documents(amcat_test_index, queries = NULL, per_page = 1, page = 2, max_pages = 2)
  )))

  expect_length(
    query_documents(amcat_test_index, queries = NULL, per_page = 1, max_pages = 10)$.id,
    10L
  )

  expect_equal(
    colnames(
      query_aggregate(amcat_test_index,
                      axes = list(list(field="keyword",
                                       list(field="date", interval="day"))),
                      queries = "test")
    ),
    c("keyword", "n")
  )

  expect_error(
      query_aggregate(amcat_test_index,
                      axes = list(list(field="text",
                                       list(field="date", interval="day"))),
                      queries = "test",
                      filters = list(cats = "cute",
                                     date = list(gte = "1900-01-01"))),
    "Aggregation axes need to be either date or keyword"
  )

  expect_equal(
    query_aggregate(amcat_test_index,
                    axes = list(list(field="keyword", list(field="date", interval="day"))),
                    queries = "test",
                    filters = list(cats = "cute",
                                   date = list(gte = "1900-01-01")))$n,
    5
  )

  test_doc$date[1] <- "2022-01-01T00:00:00"
  upload_documents(amcat_test_index, documents = test_doc)
  refresh_index(amcat_test_index)
  expect_equal(
    query_aggregate(amcat_test_index,
                    axes = list(list(field="keyword", list(field="date", interval="day"))),
                    queries = "test",
                    filters = list(cats = "cute",
                                   date = list(gte = "2023-01-01")))$n,
    4L
  )

  expect_equal({
    update_tags(
      index = amcat_test_index,
      action = "remove",
      field = "test",
      tag = "test",
      filters = list(cats = "cute",
                     date = list(gte = "2023-01-01"))
    )
    refresh_index(amcat_test_index)
    sum(is.na(query_documents(amcat_test_index, queries = NULL, fields = c("test", "title"))))},
    0L
  )
})
