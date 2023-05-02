test_that("Create Index", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  expect_no_error(
    create_index("amcat4r-test")
  )
  expect_true(
    "amcat4r-test" %in% list_indexes()
  )
})

test_that("documents", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  test_doc <- data.frame(
    .id = "1",
    title = "test",
    text = "test",
    date = "2022-01-01T00:00:00"
  )

  expect_equivalent({
    upload_documents("amcat4r-test", documents = test_doc)
    Sys.sleep(2) # seems to take a second
    out <- query_documents("amcat4r-test", queries = NULL, fields = NULL)
    c(out$title, out$text)
  }, c(test_doc$title, test_doc$text))

  expect_equivalent({
    update_documents("amcat4r-test", documents = data.frame(.id = "1", title = "test-update"))
    Sys.sleep(2) # seems to take a second
    out <- query_documents("amcat4r-test", queries = NULL, fields = NULL)
    c(out$title, out$text)
  }, c("test-update", test_doc$text))

  expect_equal({
    delete_documents("amcat4r-test", "1")
    Sys.sleep(2) # seems to take a second
    dim(query_documents("amcat4r-test", queries = NULL, fields = NULL))
  }, c(0, 0))

})

test_that("users", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))

  expect_false(
    "test" %in% list_index_users("amcat4r-test")$email
  )

  expect_true({
    add_index_user("amcat4r-test", email = "test", role = "READER")
    "test" %in% list_index_users("amcat4r-test")$email
  })

  expect_equal(
    modify_index_user("amcat4r-test", email = "test", role = "ADMIN")$role,
    "ADMIN"
  )

  expect_false({
    delete_index_user("amcat4r-test", email = "test")
    "test" %in% list_index_users("amcat4r-test")$email
  })

})

test_that("fields", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))

  expect_equal(
    dim(get_fields("amcat4r-test")),
    c(4, 2)
  )

  expect_equal({
    set_fields("amcat4r-test", list(test = "keyword"))
    out <- get_fields("amcat4r-test")
    c(dim(out), out[out$name == "test", "type"])
  }, list(5L, 2L, type = "keyword"))

})

test_that("index 2", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))

  # run this last to not leave the index in the database
  expect_false({
    delete_index("amcat4r-test")
    "amcat4r-test" %in% list_indexes()
  })

})
