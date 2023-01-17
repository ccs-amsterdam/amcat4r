#' Run docker containers with AmCAT modules
#'
#' Automatically sets up services defined in a Docker Compose file. Only options
#' relevant to AmCAT are implemented.
#'
#' @param compose Path to a Docker Compose file. Uses
#'   https://github.com/JBGruber/amcat4docker/blob/main/docker-compose.yml by
#'   default.
#' @param force_install If TRUE, removes all containers and re-creates them from
#'   the compose file. If 2, the images are also re-downloaded. Danger: this
#'   will destroy the indexes in your containers!
#'
#' @export
run_amcat_docker <- function(compose = NULL, force_install = FALSE) {

  rlang::check_installed("dockr", action = function(...) {
    rlang::check_installed("remotes")
    remotes::install_github("JBGruber/dockr")
  })

  # get settings
  if (is.null(compose)) {
    compose <- url("https://raw.githubusercontent.com/JBGruber/amcat4docker/main/docker-compose.yml")
  }

  dockr::compose_up(compose, force_install = force_install)
}


#' Stop docker containers with AmCAT modules
#'
#' @param filters Names of containers or named values for other filters. (See
#'   [https://docs.docker.com/engine/api/v1.41/#tag/Container/operation/ContainerList])
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # stop AmCAT modules
#' stop_amcat_docker()
#'
#' # stop container by id
#' stop_amcat_docker(filters = c(id = "a6cbe4787227"))
#'
#' # stop all containers
#' stop_amcat_docker(filters = "")
#' }
stop_amcat_docker <- function(filters = c("elastic", "amcat")) {

  if (is.null(names(filters)))  {
    filters <- list(filters)
    names(filters) <- "name"
  } else {
    filters <- as.list(filters)
  }
  do.call(dockr::docker_lc, filters)$name |>
    lapply(dockr::docker_stop) |>
    invisible()

}


