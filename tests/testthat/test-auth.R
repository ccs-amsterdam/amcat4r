server <- "http://localhost/amcat"

test_that("No token found", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  expect_error(amcat4r:::amcat_get_token("example.com"),
               "No authentication found.")
})

test_that("Login", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  expect_s3_class(amcat_login(server, cache = 2),
                  "amcat4_token")
  expect_s3_class(amcat4r:::amcat_get_token(server),
                  "amcat4_token")
})

test_that("No Browser found", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  withr::with_options(
    list("browser" = ""),
    expect_error(amcat4r:::get_middlecat_token(server),
                 "Authentication needs access")
  )
})

test_that("cache tokens on disk and retrieve", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  token <- amcat4r:::amcat_get_token(server)
  tmp <- tempfile()
  withr::with_options(
    list(amcat4r_token_cache = tmp),
    {
      amcat4r:::tokens_cache(token, server, cache = 1)
      expect_true(file.exists(tmp))
    }
  )
})

test_that("get config", {
  skip_if(as.logical(Sys.getenv("amcat_offline")))
  expect_match(amcat4r:::get_config(server)$authorization,
               "no_auth")
})
