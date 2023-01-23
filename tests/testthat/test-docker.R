test_that("docker", {
  skip_if_not_installed("dockr")
  # only test ngincat to not interfere with other tests
  expect_no_error(stop_amcat_docker(filters = "ngincat"))
  expect_no_error(run_amcat_docker())
  Sys.sleep(5) # wait a few seconds until back running
})
