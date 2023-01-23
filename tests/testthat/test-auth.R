server <- "http://localhost/amcat"

test_that("No token found",
          expect_error(amcat4r:::amcat_get_token(server),
                       "No authentication found.")
)

test_that("Login",
          expect_s3_class(amcat_login(server, cache = 2),
                          "amcat4_token")
)

test_that("No Browser found", {
  tmp <- getOption("browser")
  options("browser" = "")
  expect_error(amcat4r:::get_middlecat_token(server),
               "Authentication needs access")
  options("browser" = tmp)
})

test_that("cache tokens on disk and retrieve", {
  tmp <- tempfile()
  options(amcat4r_token_cache = tmp)
  tokens <- amcat4r:::amcat_get_token(server)
  attr(tokens, "cache_choice") <- 1L
  amcat4r:::tokens_cache(tokens, server)
  expect_true(file.exists(tmp))
  tok <- amcat4r:::amcat_get_token(server)
  expect_match(tok$token_type, "no_auth")
  unlink(tmp)
  .Options$amcat4r_token_cache <- NULL
})

test_that("get config", {
  expect_match(amcat4r:::get_config(server)$authorization,
               "no_auth")
})
