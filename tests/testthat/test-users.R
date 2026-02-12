test_that("query", {
  setup_test()

  expect_false(amcat_test_user %in% suppressWarnings(list_users()$email))

  expect_true({
    create_user(amcat_test_user, role = "WRITER")
    amcat_test_user %in% list_users()$email
  })

  expect_equal({
    users <- list_users()
    users[users$email == amcat_test_user, ]$role
  }, "WRITER")

  expect_equal({
    modify_user(amcat_test_user, role = "admin")
    users <- list_users()
    users[users$email == amcat_test_user, ]$role
  }, "ADMIN")

  expect_false({
    delete_user(amcat_test_user)
    amcat_test_user %in% list_users()
  })
})
