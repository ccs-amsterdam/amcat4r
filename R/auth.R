#' Authenticate to an AmCAT instance
#'
#' @param server URL of the AmCAT instance
#' @param api_key The API Key to use for authentication (API version 4.1+)
#' @param token_refresh Whether to enable refresh token rotation (see details; for API version 4.0).
#' @param force_refresh Overwrite existing cached authentication
#' @param cache select where tokens should be cached to suppress the user menu.
#'   1 means to store on disk, 2 means to store only in memory.
#' @param test_login If TRUE (default), fetch /users/me to test succesful login.
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
#'   It returns an amcat4_token object, which contains a number of standard fields
#'   (host, api_version, authorization) and fields depending on the authentication
#'   method, currently api_token (for 4.1) and the httr2_token fields for 4.0
#'
#' @return an \code{amcat4_token} object, which besides the token itself will contain:
#' \itemize{
#'   \item \strong{host}: The base URL of the AmCAT server.
#'   \item \strong{api_version}: Character string of the API version.
#'   \item \strong{authorization}: The authorization configuration
#' }
#' @export
#'
#' @examples
#' \dontrun{
#'   amcat_login("https://middlecat.up.railway.app/api/demo_resource")
#' }
amcat_login <- function(server,
                        api_key = NULL,
                        token_refresh = FALSE,
                        force_refresh = FALSE,
                        cache = NULL,
                        test_login = TRUE) {
  config <- get_config(server)

  tokens <- if (force_refresh) NULL else amcat4r:::amcat_get_token(server, warn = FALSE)


  # Deal with API version-dependent authentication
  if (is.null(config$api_version)) {
    stop(paste0("The server at ", server, " has API version < 4.0.11, ",
                "please upgrade the server or use an older client to connect"))
  } else if (package_version(config$api_version) >= "4.1") {
    tokens <- auth_4_1(tokens, server, config=config, api_key=api_key)
  } else {
    tokens <- auth_4_0(tokens, server, config=config, token_refresh=token_refresh)
  }

  # Cache tokens and configuration
  tokens$authorization <- config$authorization
  tokens$api_version <- config$api_version
  tokens$host <- server

  if (is.null(attr(tokens, "cache_choice"))) attr(tokens, "cache_choice") <- cache
  tokens <- tokens_cache(tokens, cache)

  if (test_login) {
    u <- get_user("me", credentials=tokens)
    cli::cli_alert_success("Logged on to {.url {tokens$host}} as {.email {u$email}} ({.strong {u$role}})")
  }

  invisible(tokens)
}





# internal function to cache tokens
tokens_cache <- function(tokens, cache) {

  cache_choice <- cache
  if (is.null(cache_choice)) cache_choice <- attr(tokens, "cache_choice")
  if (is.null(cache_choice) & !interactive())
    stop("If you want to run this in a non-interactive environment, ",
         "you have to set a cache mode in amcat_login")
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

    pkg.env$current_server <- tokens$host
    pkg.env$authorization <- tokens$authorization

    if (cache_choice == 1L) {
      # Write token tok disk
      cache <- getOption("amcat4r_token_cache")
      if (is.null(cache)) {
        cache <- file.path(rappdirs::user_cache_dir("httr2"),
                           paste0(rlang::hash(tokens$host), "-token.rds.enc"))
        dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
      }
      httr2::secret_write_rds(tokens, path = cache, key = I(rlang::hash(tokens$host)))
    } else if (cache_choice == 2L) {
      # Cache token in memory
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


# internal function to retrieve config from an amcat server
get_config <- function(server) {
  out <- httr2::request(server) |>
    httr2::req_url_path_append("config") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  for (w in out$warnings) if (!is.null(w)) cli::cli_alert_warning(w)
  return(out)
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
  } else if (!is.null(getOption("amcat4r_
                                token_cache"))) { # check option next
    tokens <- httr2::secret_read_rds(path = getOption("amcat4r_token_cache"),
                                     key = I(rlang::hash(server)))
  } else { # lastly check disk cache
    disk_cache <- file.path(rappdirs::user_cache_dir("httr2"),
                            paste0(rlang::hash(server), "-token.rds.enc"))
    if (file.exists(disk_cache)) {
      tokens <- httr2::secret_read_rds(path = disk_cache,
                                       key = I(rlang::hash(server)))
    } else if (warn) {
      cli::cli_abort(c("!" = "No authentication found. Did you run amcat_login already?"))
    }
  }

  return(amcat_token_check(tokens, server))
}
