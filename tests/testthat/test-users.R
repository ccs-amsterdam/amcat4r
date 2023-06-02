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
    users <- list_users()
    users[users$email == "test@example.com", ]$role
  }, "WRITER")

  expect_equal({
    modify_user("test@example.com", role = "admin")
    users <- list_users()
    users[users$email == "test@example.com", ]$role
  }, "ADMIN")

  expect_equal({
    modify_user("test@example.com", role = "reader")
    users <- list_users()
    users[users$email == "test@example.com", ]$role
  }, "READER")

  expect_false({
    delete_user("test@example.com")
    "test@example.com" %in% list_users()
  })
})
