amcat_login("http://localhost/amcat", cache = 2L)

test_that("query", {
  skip_if(Sys.getenv("amcat_offline"))

  expect_equal(dim(list_users()),
               c(0, 0))

  expect_equal({
    create_user("test@example.com", global_role = "writer")
    dim(list_users())
  }, c(1, 2))

  expect_equal({
    modify_user("test@example.com", global_role = "admin")
    list_users()$global_role
  }, "ADMIN")

  expect_equal({
    delete_user("test@example.com")
    dim(list_users())
  }, c(0, 0))

})
