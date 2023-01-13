#' Authenticate to an AmCAT instance
#'
#' @param server URL of the AmCAT instance
#' @param token_refresh Whether to enable refresh token rotation (see details).
#' @param force_refresh Overwrite existing cached authentications.
#' @param username,password log in with username and password directly (not
#'   supported for all users).
#' @param cache select where tokens should be cached to suppress the user menu.
#'   1 means to store on disk, 2 means to store only in memory.
#'
#' @details Enabling refresh token rotation ensures added security as leaked
#'   refresh tokens also become invalidated after a short while. It is currently
#'   disabled by default as it is not fully supported by the underlying httr2
#'   package.
#'
#'   If you select to store your tokens on disk in the interactive menu, they
#'   are stored in the location indicated by
#'   \code{rappdirs::user_cache_dir("httr2")}.
#'
#'   The function needs to open a browser, which will usually only work in an
#'   interactive session. However, you can save the returned object in an rds
#'   file (with \code{saveRDS()}) and tell amcat4r where to look for it:
#'   \code{options(amcat4r_token_cache = "path/to/location/tokens.rds")}. If you
#'   still have issues in an interactive session, check \link[utils]{browseURL}
#'   to see if you can set a browser manually.
#'
#' @return an amcat4_token object
#' @export
#'
#' @examples
#' \dontrun{
#'   amcat_auth("https://middlecat.up.railway.app/api/demo_resource")
#' }
amcat_auth <- function(server,
                       token_refresh = FALSE,
                       force_refresh = FALSE,
                       username = NULL,
                       password = NULL,
                       cache = NULL) {

  if (force_refresh) {
    tokens <- NULL
  } else {
    tokens <- amcat_get_token(server, warn = FALSE)
  }

  if (is.null(tokens)) {
    if (is.null(username)) {
      tokens <- get_middlecat_token(server = server, token_refresh = token_refresh)
    } else {
      tokens <- get_password_token(server = server, username = username, password = password)
    }
  }

  attr(tokens, "cache_choice") <- cache
  tokens <- tokens_cache(tokens, server)
  invisible(tokens)
}

# internal function to get a token from amcat directly using password auth
get_password_token <- function(server, username, password) {
  if (is.null(password)) {
    if (rstudioapi::isAvailable()) {
      password = rstudioapi::askForPassword(paste("Password for", server))
    } else {
      stop("password is missing")
    }
  }
  cli::cli_progress_bar()
  cli::cli_progress_step("Requesting a token at {server} for username {username} using a password")
  r = httr::POST(paste0(server, "/auth/token"),
                 body=list(username=username, password=password))

  httr::stop_for_status(r)
  cli::cli_progress_step("Authentication at {server} successful")
  cli::cli_progress_done()
  httr::content(r)
}

# internal function to get a token from middlecat
get_middlecat_token <- function(server, token_refresh=FALSE) {
  middlecat <- get_config(server)[["middlecat_url"]]

  if (methods::is(getOption("browser"), "character")) {
    if (getOption("browser") == "") {
      cli::cli_abort(c("!" = "Authentication needs access to a browser. See ?amcat_auth."))
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
  cli::cli_progress_step("Authentication at {server} successful")
  cli::cli_progress_done()
  tokens
}

# internal function to cache tokens
tokens_cache <- function(tokens, server) {

  cache_choice <- attr(tokens, "cache_choice")
  if (is.null(cache_choice) & interactive()) {
    cache_choice <- utils::menu(
      c("Store on disk (less secure)",
        "Store only in memory (less convenient)",
        "Authenticate each time"),
      title = glue::glue("Should the amcat token be stored only in memory (more secure) or ",
                         " encrypted on disk (more convenient as authentication does ",
                         "not need to be repeated in a new R session)?")
    )
    attr(tokens, "cache_choice") <- cache_choice
  }

  if (cache_choice < 3L) {

    pkg.env$current_server <- server

    if (cache_choice == 1L) {

      cache <- getOption("amcat4r_token_cache")
      if (is.null(cache)) {
        cache <- file.path(rappdirs::user_cache_dir("httr2"),
                           paste0(rlang::hash(server), "-token.rds.enc"))
        dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
      }
      httr2::secret_write_rds(tokens, path = cache, key = I(rlang::hash(server)))

    } else if (cache_choice == 2L) {

      rlang::env_poke(pkg.env, nm = rlang::hash(server), tokens)

    }
  }
  return(tokens)
}

# internal function to check tokens
amcat_token_check <- function(tokens, server) {
  if (!is.null(tokens$expires_at) && tokens$expires_at < as.numeric(Sys.time() + 10)) {
    tokens <- amcat_token_refresh(tokens, server)
  }
  return(tokens)
}

# internal function to refresh tokens
amcat_token_refresh <- function(tokens, server) {

  cache_choice <- attr(tokens, "cache_choice")
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
  tokens <- tokens_cache(tokens, server)

  return(tokens)
}

# internal function to retrieve config from an amcat server
get_config <- function(server) {
  httr2::request(server) |>
    httr2::req_url_path_append("middlecat") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# internal function to retrieve token
amcat_get_token <- function(server = NULL, warn = TRUE) {

  if (is.null(server)) {
    server <- pkg.env$current_server
  }
  tokens <- NULL

  # check memory cache first
  if (rlang::env_has(pkg.env, rlang::hash(server))) {
    tokens <- rlang::env_get(pkg.env, rlang::hash(server))
  } else if (!is.null(getOption("amcat4r_token_cache"))) { # check option next
    getOption("amcat4r_token_cache")
    tokens <- httr2::secret_read_rds(path = getOption("amcat4r_token_cache"),
                                     key = I(rlang::hash(server)))
  } else { # lastly check disk cache
    disk_cache <- file.path(rappdirs::user_cache_dir("httr2"),
                            paste0(rlang::hash(server), "-token.rds.enc"))
    if (file.exists(disk_cache)) {
      tokens <- httr2::secret_read_rds(path = disk_cache,
                                       key = I(rlang::hash(server)))
    } else if (warn) {
      cli::cli_abort(c("!" = "No authentication found. Did you run amcat_auth already?"))
    }
  }

  return(amcat_token_check(tokens, server))
}
