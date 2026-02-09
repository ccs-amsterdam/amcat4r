# Internal functions for dealing with AmCAT 4.1 auth

auth_40 <- function(server, config, api_token) {
  tokens <- amcat_get_token(server, warn = FALSE)

  if (is.null(tokens$api_version)) {
    # This is from an older client, so cannot re-use token
    message("Discarding pre-4.1 authentication tokens")
    tokens <- NULL
  }

  if (config$authorization == "no_auth") {
    # Nothing to do really
  } else if (!is.null(api_token) && !is.na(api_token) && api_token != "") {
    config$api_token = api_token
  } else if (is.null(config$api_token) || is.na(config$api_token) || config$api_token != "") {
    stop("Please provide an API token! In the graphical interface, select 'API tokens' in the account menu (top right) after logging in. You can also use the server CLI to obtain a token for a headless server")
  }



  tokens
}
