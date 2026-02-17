library(testthat)

# Note - these tests assume that the global variables from setup.R are created
# testthat::test_file("tests/testthat/test-index.R")

test_that("Create Index", {
  setup_test(create_index=FALSE)
  expect_no_error(
    create_index(amcat_test_index)
  )
  expect_true(
    amcat_test_index %in% list_indexes()$name
  )
})

test_that("documents", {
  setup_test()
  test_doc <- data.frame(
    .id = "1",
    title = "test",
    text = "test",
    date = "2022-01-01T00:00:00"
  )

  expect_equivalent({
    upload_documents(amcat_test_index, documents = test_doc)
    refresh_index(amcat_test_index)
    out <- query_documents(amcat_test_index, queries = NULL, fields = NULL)
    c(out$title, out$text)
  }, c(test_doc$title, test_doc$text))

  expect_equivalent({
    update_documents(amcat_test_index, documents = data.frame(.id = "1", title = "test-update"))
    refresh_index(amcat_test_index)
    out <- query_documents(amcat_test_index, queries = NULL, fields = NULL)
    c(out$title, out$text)
  }, c("test-update", test_doc$text))

  expect_equal({
    delete_documents(amcat_test_index, "1")
    refresh_index(amcat_test_index)
    dim(query_documents(amcat_test_index, queries = NULL, fields = NULL))
  }, c(0, 0))

})

test_that("date conversion", {
  setup_test()
  test_doc <- data.frame(
    .id = "1",
    title = "test",
    text = "test",
    date = "2022-01-01T00:00:00"
  )
  upload_documents(amcat_test_index, documents = test_doc)
  refresh_index(amcat_test_index)
  expect_equivalent(
    as.character(query_documents(amcat_test_index, queries = NULL, fields = NULL)$date),
    "2022-01-01"
  )

  update_documents(amcat_test_index, ids = "1", documents = data.frame(date = "2022-01-01T00:00:01"))
  refresh_index(amcat_test_index)
  expect_equivalent(
    as.character(query_documents(amcat_test_index, queries = NULL, fields = NULL)$date),
    "2022-01-01 00:00:01"
  )
})

test_that("users", {
  setup_test()

  expect_false(
    "test" %in% purrr::pluck(list_index_users(amcat_test_index), "email")
  )

  expect_true({
    add_index_user(amcat_test_index, email = "test@amcat.nl", role = "READER")
    "test@amcat.nl" %in% list_index_users(amcat_test_index)$email
  })

  expect_equal(
    modify_index_user(amcat_test_index, email = "test@amcat.nl", role = "ADMIN")$role,
    "ADMIN"
  )

  expect_false({
    delete_index_user(amcat_test_index, email = "test@amcat.nl")
    "test" %in% purrr::pluck(list_index_users(amcat_test_index), "email")
  })

})

test_that("fields", {
  setup_test(create_index = FALSE)
  create_index(amcat_test_index)
  set_fields(amcat_test_index, list(title="text", date="date", text="text"))

  expect_equal(
    dim(get_fields(amcat_test_index)),
    c(3, 6)
  )

  expect_equal({
    set_fields(amcat_test_index, list(test = "keyword"))
    out <- get_fields(amcat_test_index)
    c(dim(out), out[out$name == "test", "type"])
  }, list(4L, 6L, type = "keyword"))

})
