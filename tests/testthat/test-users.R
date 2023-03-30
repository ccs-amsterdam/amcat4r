# we have to log in each time it seems as the cache is destroyed
if (!as.logical(Sys.getenv("amcat_offline")))
  amcat_login("http://localhost/amcat", cache = 2L)

test_that("query", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))

  expect_false("test@example.com" %in% list_users()$email)

  expect_true({
    create_user("test@example.com", role = "WRITER")
    "test@example.com" %in% list_users()$email
  })

  expect_equal({
    list_users()$role
  }, "WRITER")

  expect_equal({
    modify_user("test@example.com", role = "admin")
    list_users()$role
  }, "ADMIN")

  expect_equal({
    modify_user("test@example.com", role = "reader")
    list_users()$role
  }, "READER")

  expect_false({
    delete_user("test@example.com")
    "test@example.com" %in% list_users()
  })
})
