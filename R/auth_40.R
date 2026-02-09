# Internal functions for dealing with AmCAT 4.0 auth system\
auth_40 <- function(server, token_refresh=token_refresh, force_refresh=force_refresh) {
  if (force_refresh) {
    tokens <- NULL
  } else {
    tokens <- amcat_get_token(server, warn = FALSE)
  }

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
}
