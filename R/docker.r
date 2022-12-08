#' Run docker containers with AmCAT modules
#'
#' Automatically sets up services defined in a Docker Compose file. Only options
#' relevant to AmCAT are implemented.
#'
#' @param compose Path to a Docker Compose file. Uses
#'   https://github.com/JBGruber/amcat4docker/blob/main/docker-compose.yml by
#'   default.
#' @param force If TRUE, removes all containers and images mentioned in the
#'   compose file and re-downloads them.
#'
#' @export
run_amcat_docker <- function(compose = NULL, force = FALSE) {

  docker_ping()

  # get settings
  if (is.null(compose)) {
    compose <- url("https://raw.githubusercontent.com/JBGruber/amcat4docker/main/docker-compose.yml")
  }
  config <- parse_yml(x = compose)

  # set up networks
  for (n in names(config$networks)) {
    if (!n %in% docker_ln()$name) {
      docker_create_network(n, verbosity = 1)
    }
  }

  # check missing images and containers
  conf_imgs <- s_pull(config$services, "Image")
  conf_cont <- s_pull(config$services, "container_name")
  imgs <- docker_li(reference = conf_imgs)$tags
  containers <- docker_lc(name = conf_cont, all = TRUE)$name

  if (force) {
    lapply(containers, function(c) {
      docker_stop(c)
      docker_rmc(c, force = TRUE)
    })
    lapply(imgs, function(i) docker_rmi(i, force = TRUE))
    imgs <- containers <- character()
  }

  # create images
  lapply(config$services, function(service)
    if(!service$Image %in% imgs)
      docker_create_image(image = service$Image))

  # create containers
  lapply(config$services, function(service)
    if (!service$container_name %in% containers)
      docker_create_container(name = service$container_name, body = service))

  # start containers
  lapply(config$services, function(service)
    docker_start(service$container_name))

  if (all(conf_cont %in% docker_lc(name = conf_cont)$name)) {
    message("all done!")
  }
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
  do.call(docker_lc, filters)$name |>
    lapply(docker_stop) |>
    invisible()
}


# utils
parse_yml <- function(x) {

  compose <- yaml::read_yaml(x)

  compose$services <- lapply(compose$services, function(s) {

    # rename
    pattern <- c("image", "networks", "environment", "command", "ports")
    replacement <- c("Image", "NetworkMode", "Env", "Cmd", "PortBindings")
    for(i in seq_along(pattern)) names(s) <- gsub(pattern[i], replacement[i], names(s), fixed = TRUE)

    # reformat some special arguments
    if ("PortBindings" %in% names(s)) {
      local <- gsub(":.+$", "", s$PortBindings)
      container <- gsub("^.+?:", "", s$PortBindings)
      s$PortBindings <- list(data.frame(HostPort = local))
      names(s$PortBindings) <- paste0(container, "/tcp")
    }
    if ("Env" %in% names(s)) {
      if (!is.null(names(s$Env))) {
        env <- paste0(names(s$Env), "=", s$Env)
        s$Env <- as.list(env)
      }
    }

    return(s)
  })

  compose$version <- NULL
  compose
}


docker_base_req <- function(verbose = TRUE) {
  if (R.Version()$os == "mingw32") {
    req <- httr2::request("http://localhost:2375")
  } else {
    req <- httr2::request("http://localhost:80") |>
      httr2::req_options(UNIX_SOCKET_PATH = "/var/run/docker.sock")
  }
  if(verbose) req <- httr2::req_options(req, debugfunction = return_status, verbose = TRUE)
  return(req)
}


return_status <- function(type, sts) {
  if (type == 3) {
    sts <- readBin(sts, character())
    lines <- unlist(strsplit(sts, "\r?\n", useBytes = TRUE))
    msgs <- jsonlite::stream_in(textConnection(grep("^\\{", lines, value = TRUE)), verbose = FALSE)
    msgs <- unlist(msgs[!is.na(msgs)], recursive = TRUE)
    # glob_msgs <<- c(glob_msgs, list(msgs))
    for (msg in msgs) {
      message(msg)
    }
  }
}


# check functions
#' Ping Docker daemon
#'
#' Fails if Docker daemon is not reachable
#'
#' @export
docker_ping <- function() {
  res <- try(
    docker_base_req() |>
      httr2::req_url_path_append("_ping") |>
      httr2::req_method("get") |>
      httr2::req_perform()
  )
  if (methods::is(res, "try-error")) {
    stop("The Docker daemon is not reachble")
  }
  invisible(!methods::is(res, "try-error"))
}

s_pull <- function(x, ...) {
  vapply(x, function(x) ifelse(length(x[[...]]) == 0, "", toString(x[[...]])), character(1))
}

#' List docker containers
#'
#' @param ... named values are used as filters.
#' @param all include stopped containers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_lc(name = c("amcat", "elastic"))
#' }
docker_lc <- function(..., all = FALSE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("/containers/json") |>
    httr2::req_method("get") |>
    httr2::req_url_query(all = tolower(all))

  dots <- list(...)
  if (length(dots) > 0) req <- httr2::req_url_query(req, filters = jsonlite::toJSON(dots))

  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::tibble(
    name = s_pull(res, "Names"),
    image = s_pull(res, "Image"),
    status = s_pull(res, "Status"),
    id = s_pull(res, "Id"),
    ports = s_pull(res, "Ports")
  ) |>
    dplyr::mutate(name = gsub("^/", "", name))

}


#' List docker images
#'
#' @param ... named values are used as filters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_li(reference = "amcat4docker")
#' }
docker_li <- function(...) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("/images/json") |>
    httr2::req_method("get")

  dots <- list(...)
  if (length(dots) > 0) req <- httr2::req_url_query(req, filters = jsonlite::toJSON(dots))

  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::tibble(
    labels = s_pull(res, "Labels"),
    tags = s_pull(res, "RepoTags"),
    id = s_pull(res, "Id")
  )
}

#' List docker networks
#'
#' @param ... named values are used as filters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_ln()
#' }
docker_ln <- function(...) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("/networks") |>
    httr2::req_method("get")

  dots <- list(...)
  if (length(dots) > 0) req <- httr2::req_url_query(req, filters = jsonlite::toJSON(dots))

  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::tibble(
    name = sapply(res, function(x) x[["Name"]]),
    labels = sapply(res, function(x) toString(x[["Labels"]])),
    driver = sapply(res, function(x) x[["Driver"]]),
    id = sapply(res, function(x) x[["Id"]])
  )
}


# mainly used to translate json options from running containers
docker_inspect_container <- function(id, as_json = TRUE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers", id, "json") |>
    httr2::req_method("get") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (as_json) {
    return(httr2::resp_body_string(res))
  } else {
    return(httr2::resp_body_json(res))
  }

}

docker_inspect_network <- function(id, as_json = TRUE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("networks", id) |>
    httr2::req_method("get") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (as_json) {
    return(httr2::resp_body_string(res))
  } else {
    return(httr2::resp_body_json(res))
  }

}


# create functions
docker_create_image <- function(image, verbose = TRUE) {

  if (is.null(image)) stop("name can not be NULL")

  # create image
  req <- docker_base_req(verbose = verbose) |>
    httr2::req_url_path_append("images/create") |>
    httr2::req_url_query(fromImage = image) |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (!is.null(res$body)) res <- httr2::resp_body_raw(res)
  return_status(3L, res)
}

docker_create_container <- function(name, body, verbosity = 3) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers/create") |>
    httr2::req_url_query(name = name) |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(body)

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  if (length(res$Warnings) > 0) warning(res$Warnings)
  invisible(res$Id)
}


docker_create_network <- function(name, verbosity = 3) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("networks/create") |>
    httr2::req_url_query(name = name) |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(list(Name = name))

  res <- httr2::req_perform(req, verbosity = verbosity) |>
    httr2::resp_body_json()

  if (nchar(res$Warning) > 0) warning(res$Warnings)
  if (length(res$Id) > 0 & verbosity > 0) message("Network ", name, " created. ID: ", res$Id)
  invisible(res$Id)
}


# remove functions
## remove images
docker_rmi <- function(image, force = FALSE, noprune = FALSE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("images", image) |>
    httr2::req_method("delete") |>
    httr2::req_url_query(force = tolower(force),
                         noprune = tolower(noprune)) |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  if (length(res$Warnings) > 0) warning(res$Warnings)
  invisible(res$Id)

}

## remove containers
docker_rmc <- function(container, force = FALSE, link = FALSE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers/", container) |>
    httr2::req_method("delete") |>
    httr2::req_url_query(force = tolower(force),
                         link = tolower(link)) |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (length(res$body) > 0) res <- httr2::resp_body_json(res)
  if (length(res$Warnings) > 0) warning(res$Warnings)
  if (length(res$message) > 0) message(res$message)
  invisible(res$Id)
}


# control functions
docker_start <- function(id) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers", id, "start") |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  message(
    switch(as.character(httr2::resp_status(res)),
           "204" = paste(id, "started"),
           "304" = "container already started",
           "404" = "no such container",
           "500" = "server error")
  )

}


docker_stop <- function(id, kill_after = 60L) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers", id, "stop") |>
    httr2::req_method("post") |>
    httr2::req_url_query(t = kill_after) |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  message(
    switch(as.character(httr2::resp_status(res)),
           "204" = paste(id, "stopped"),
           "304" = "container already stopped",
           "404" = "no such container",
           "500" = "server error")
  )
}


#' Execute commands inside a running container
#'
#' @param id ID or name of container.
#' @param cmd command to be run.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_exec(id = "amcat4",
#'             cmd = "amcat4 create-admin --username admin --password supergeheim")
#'
#' docker_exec(id = "amcat4",
#'             cmd = "amcat4 create-test-index")
#' }
docker_exec <- function(id, cmd) {

  # make exec instance
  req <- docker_base_req(verbose = FALSE) |>
    httr2::req_url_path_append("containers", id, "exec") |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(list(
        "AttachStdin" =  TRUE,
        "AttachStdout" =  TRUE,
        "AttachStderr" =  TRUE,
        "Cmd" = as.list(unlist(strsplit(cmd, split = " ", fixed = TRUE)))
    ))

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  # run instance
  req2 <- docker_base_req(verbose = TRUE) |>
    httr2::req_url_path_append("exec", res$Id, "start") |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(list(
      "Detach" =  FALSE,
      "Tty" = FALSE
    ))

  res2 <- httr2::req_perform(req2)
  if (length(res2$body) > 0) {
    message(httr2::resp_body_string(res2))
  }

}



