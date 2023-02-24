amcat_login("http://localhost/amcat", cache = 2L)

test_that("query", {
  skip_if(Sys.getenv("amcat_offline"))

  expect_equal(
    dim(query_documents("state_of_the_union", queries = NULL, fields = NULL)),
    c(232, 7),
    tolerance = 1L
  )

  expect_equal(
    colnames(query_aggregate("state_of_the_union",
                             axes = list(list(field="party", list(field="date", interval="year"))),
                             queries = c("war", "peace"),
                             filters = list(party = c("Democratic", "Republican"),
                                            date = list(gte = "1900-01-01")))),
    c("party", "n")
  )

  expect_equal({
    set_fields("state_of_the_union", list(test = "tag"))
    update_tags(
      index = "state_of_the_union",
      action = "add",
      field = "test",
      tag = "test",
      filters = list(party = "Republican",
                     date = list(gte = "2000-01-01"))
    )
    Sys.sleep(2) # seems to take a second to work
    sum(is.na(query_documents("state_of_the_union", queries = NULL, fields = c("test", "title"))))},
    221L
  )

  expect_equal({
    update_tags(
      index = "state_of_the_union",
      action = "remove",
      field = "test",
      tag = "test",
      filters = list(party = "Republican",
                     date = list(gte = "2008-01-01"))
    )
    Sys.sleep(2) # seems to take a second to work
    sum(is.na(query_documents("state_of_the_union", queries = NULL, fields = c("test", "title"))))},
    224L
  )

})
