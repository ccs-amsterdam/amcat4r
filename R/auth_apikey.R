# Internal functions for dealing with AmCAT 4.1 auth

auth_4_1 <- function(tokens, server, config, api_key) {
  if (is.null(tokens$api_version)) {
    # This is from an older client, so cannot re-use token
    message("Discarding pre-4.1 authentication tokens")
    tokens <- NULL
  }

  if (config$authorization == "no_auth") {
    # Nothing to do really
  } else if (!is.null(api_key) && !is.na(api_key) && api_key != "") {
    tokens$api_key = api_key
  } else if (is.null(tokens$api_key) || is.na(tokens$api_key) || tokens$api_key != "") {
    stop("Please provide an API token! In the graphical interface, select 'API tokens' in the account menu (top right) after logging in. You can also use the server CLI to obtain a token for a headless server")
  }
  tokens
}
