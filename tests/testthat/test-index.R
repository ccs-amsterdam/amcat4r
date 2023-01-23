amcat_login("http://localhost/amcat", cache = 2L)

test_that("index 1", {
  skip_if(Sys.getenv("amcat_offline"))

  expect_equal(dim(list_indexes()),
               c(1, 1))

  expect_equal({
    create_index("test")
    dim(list_indexes())
  }, c(2, 1))

})

test_that("documents", {
  skip_if(Sys.getenv("amcat_offline"))

  expect_equal({
    upload_documents("test", documents = data.frame(
      data.frame(
        .id = "1",
        title = "test",
        text = "test",
        date = "2022-01-01")
    ))
    Sys.sleep(2) # seems to take a second
    dim(query_documents("test", queries = NULL, fields = NULL))
  }, c(1, 4))

  expect_equal({
    delete_documents("test", "1")
    Sys.sleep(2) # seems to take a second
    dim(query_documents("test", queries = NULL, fields = NULL))
  }, c(0, 0))

})

test_that("users", {
  skip_if(Sys.getenv("amcat_offline"))

  expect_equal(
    dim(list_index_users("test")),
    c(1, 2)
  )

  expect_equal({
    add_index_user("test", email = "test", role = "READER")
    dim(list_index_users("test"))
  }, c(2, 2))

  expect_equal(
    modify_index_user("test", email = "test", role = "ADMIN")$role,
    "ADMIN"
  )

  expect_equal({
    delete_index_user("test", email = "test")
    dim(list_index_users("test"))
  }, c(1, 2))

})

test_that("fields", {
  skip_if(Sys.getenv("amcat_offline"))

  expect_equal(
    dim(get_fields("test")),
    c(4, 2)
  )

  expect_equal({
    set_fields("test", list(test = "keyword"))
    dim(get_fields("test"))
  }, c(5, 2))

})

test_that("index 2", {
  skip_if(Sys.getenv("amcat_offline"))

  # run this last to not leave the index in the database
  expect_equal({
    delete_index("test")
    dim(list_indexes())
  }, c(1, 1))

})
