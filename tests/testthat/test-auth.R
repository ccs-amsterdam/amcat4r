server <- "test"
tokens <- list(access_token = "test",
               expires_at = as.numeric(Sys.time() + 60),
               refresh_mode = "static")

test_that("No token found",
          expect_error(amcat4r:::amcat_get_token(server),
                       "No authentication found."))

test_that("Not interactive",
          expect_error(
            amcat_login("https://middlecat.up.railway.app/api/demo_resource", force_refresh = TRUE),
            "OAuth 2\\.0 authorization code flow requires an interactive session"
          )
)

test_that("No Browser found", {
  tmp <- getOption("browser")
  options("browser" = "")
  expect_error(amcat_login("https://middlecat.up.railway.app/api/demo_resource", force_refresh = TRUE),
               "Authentication needs access to a browser.")
  options("browser" = tmp)
})

test_that("cache tokens on disk and retrieve", {
  tmp <- tempfile()
  options(amcat4r_token_cache = tmp)
  attr(tokens, "cache_choice") <- 1L
  amcat4r:::tokens_cache(tokens, server)
  expect_true(file.exists(tmp))
  tok <- amcat4r:::amcat_get_token(server)
  expect_match(tok$access_token, "test")
  unlink(tmp)
  .Options$amcat4r_token_cache <- NULL
})

test_that("cache tokens in env and retrieve", {
  attr(tokens, "cache_choice") <- 2L
  amcat4r:::tokens_cache(tokens, server)
  tok2 <- amcat4r:::amcat_get_token(server)
  expect_match(tok2$access_token, "test")
})

test_that("get config", {
  mock_config <- function(req) {
    httr2::response(
      method = "GET",
      url = "test/middlecat",
      status_code = 200L,
      headers = list(`content-type` = "application/json; charset=utf-8"),
      body = as.raw(c(0x7b, 0x22, 0x6d, 0x69, 0x64, 0x64, 0x6c, 0x65, 0x63,
                      0x61, 0x74, 0x5f, 0x75, 0x72, 0x6c, 0x22, 0x3a, 0x22, 0x74, 0x65,
                      0x73, 0x74, 0x22, 0x7d))
    )
  }
  config <- httr2::with_mock(mock_config, amcat4r:::get_config("test"))
  expect_match(config$middlecat_url, "test")
})
