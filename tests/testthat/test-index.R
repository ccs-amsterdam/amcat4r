amcat_login("http://localhost/amcat", username = "admin", password = "supergeheim", force_refresh = TRUE, cache = 2L)

test_that("index", {
  skip_if(Sys.getenv("amcat_offline"))
  expect_equal(dim(list_indexes()),
               c(1, 1))
  expect_equal({
    create_index("test")
    dim(list_indexes())
  }, c(2, 1))
  expect_equal({
    delete_index("test")
    dim(list_indexes())
  }, c(1, 1))
})
