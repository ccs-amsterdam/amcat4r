#' Authenticate to an AmCAT instance
#'
#' @param server URL of the AmCAT instance
#' @param refresh Whether to enable refresh token rotation (see details).
#'
#' @details Enabling refresh token rotation ensures added security as leaked
#'   refresh tokens also become invalidated after a short while. It is currently
#'   disabled by default as it is not fully supported by the underlying httr2
#'   package.
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
                       refresh = FALSE) {

  middlecat <- get_config(server)[["middlecat_url"]]

  if (methods::is(getOption("browser"), "character")) {
    if (getOption("browser") == "") {
      cli::cli_abort(c("!" = "Authentication needs access to a browser. See ?amcat_auth."))
    }
  }

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
      refresh = ifelse(refresh, "refresh", "static")
    )
  )

  class(tokens) <- c("amcat4_token", class(tokens))

  cache_choice <- utils::menu(
    c("Store on disk (less secure)",
      "Store only in memory (less convenient)"),
    title = glue::glue("Should the amcat token be stored only in memory (more secure) or ",
                       " encrypted on disk (more convenient as authentication does ",
                       "not need to be repeated in a new R session)?")
  )

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

  cli::cli_inform(c("i" = "autentication at {server} complete"))
  invisible(tokens)
}

amcat_token_check <- function(tokens, server) {
  if (tokens$expires_at < as.numeric(Sys.time() + 10)) {
    tokens <- amcat_token_refresh(tokens, server)
  }
  return(tokens)
}

# internal function to refresh tokens
amcat_token_refresh <- function(tokens, server) {
  middlecat <- get_config(server)[["middlecat_url"]]
  client <- httr2::oauth_client(
    id = "amcat4r",
    token_url = glue::glue("{middlecat}/api/token")
  )
  tokens <- httr2::oauth_flow_refresh(client, tokens$refresh_token)
  class(tokens) <- c("amcat4_token", class(tokens))
  rlang::env_poke(pkg.env, nm = rlang::hash(server), tokens)
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
amcat_get_token <- function(server) {
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
    } else {
      cli::cli_abort(c("!" = "No authentication found. Did you run amcat_auth already?"))
    }
  }
  return(tokens)
}
