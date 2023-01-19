test_that("docker", {
  skip_if_not_installed("dockr")
  expect_no_error(stop_amcat_docker(filters = "ngincat"))
  expect_no_error(run_amcat_docker())
})
