# Internal functions for dealing with AmCAT 4.0 auth system\
auth_4_0 <- function(tokens, server, config, token_refresh) {
  if (config[["authorization"]] == "no_auth") {
    cli::cli_inform(c("v" = "Authentication at {server} successful!"))
    tokens$authorization <- "no_auth"
    # use "httr2_token" class for a unified printing
    class(tokens) <- c("amcat4_token", "httr2_token")
  } else if (is.null(tokens)) {
    tokens <- get_middlecat_token(server = server,
                                  token_refresh = token_refresh,
                                  middlecat = config[["middlecat_url"]])
  }
  tokens
}

# internal function to get a token from middlecat
get_middlecat_token <- function(server,
                                token_refresh = FALSE,
                                middlecat) {

  if (methods::is(getOption("browser"), "character")) {
    if (getOption("browser") == "") {
      cli::cli_abort(c("!" = "Authentication needs access to a browser. See ?amcat_login."))
    }
  }
  cli::cli_progress_bar()
  cli::cli_progress_step("Requesting a token at {server} using a {middlecat}")
  client <- httr2::oauth_client(
    id = "amcat4r",
    token_url = glue::glue("{middlecat}/api/token")
  )

  tokens <- httr2::oauth_flow_auth_code(
    client = client,
    auth_url = glue::glue("{middlecat}/authorize"),
    pkce = TRUE,
    auth_params = list(
      resource = server,
      refresh_mode = ifelse(token_refresh, "refresh", "static"),
      session_type = "api_key"
    )
  )

  class(tokens) <- c("amcat4_token", class(tokens))
  cli::cli_progress_step("Authentication at {server} successful!")
  cli::cli_progress_done()
  tokens
}


# internal function to refresh tokens
amcat_token_refresh <- function(tokens, server) {

  cache_choice <- attr(tokens, "cache_choice")
  authorization <- tokens$authorization
  middlecat <- get_config(server)[["middlecat_url"]]
  client <- httr2::oauth_client(
    id = "amcat4r",
    token_url = glue::glue("{middlecat}/api/token")
  )

  # Using the unexported function until
  # https://github.com/r-lib/httr2/issues/186 is resolved.
  tokens <- httr2:::token_refresh(
    client,
    refresh_token = tokens$refresh_token,
    scope = NULL,
    token_params = list(
      resource = server,
      refresh_mode = tokens$refresh_mode,
      session_type = "api_key"
    )
  )

  class(tokens) <- c("amcat4_token", class(tokens))
  attr(tokens, "cache_choice") <- cache_choice
  tokens$authorization <- authorization
  tokens <- tokens_cache(tokens, cache_choice)

  return(tokens)
}
