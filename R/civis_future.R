#' @importFrom future run resolved result
NULL

#' Evaluate an expression in Civis Platform
#'
#' This is used as with the \code{\link{future}} API as an argument to \code{\link{plan}}.
#'
#' @param ... Arguments to \code{\link{CivisFuture}} and then \code{\link{scripts_post_containers}}
#' @return The result of evaluating \code{expr}.
#' @examples \dontrun{
#'
#'  plan(civis_platform)
#'
#'  # Specify required resources, image, and tag.
#'  fut <- future({2 + 2},
#'    required_resources = list(cpu = 1024, memory = 2048),
#'    docker_image_name = "civisanalytics/datascience-r",
#'    docker_image_tag = "2.2.0")
#'
#'  # Evaluate the future later
#'  fut <- future({2 + 2}, lazy = TRUE)
#'  run(fut)
#'
#'  # check if a future has resolved
#'  resolved(fut)
#'
#'  # block until the future resolves, and return the value or throw error
#'  value(fut)
#'
#'  # cancel the job
#'  cancel(fut)
#'
#'  # grab the run logs
#'  fetch_logs(fut)
#'
#'  # handle errors
#'  fut <- future({stop("Error!")})
#'  e <- tryCatch(value(fut), error = function(e) e)
#'  get_error(e)
#' }
#'
#' @export
civis_platform <- function(...) {
  future <- CivisFuture(...)
  if (!future$lazy) future <- run(future)
  future
}
class(civis_platform) <- c("CivisFuture", "future", "function")

#' Evaluate an expression in Civis Platform
#' @inheritParams future::Future
#' @param required_resources resources, see \code{\link{scripts_post_containers}}
#' @param docker_image_name the image for the container script.
#' @param docker_image_tag the tag for the Docker image.
#' @param ... arguments to \code{\link{scripts_post_containers}}
#'
#' @return A \code{CivisFuture} inheriting from \code{\link{Future}} that evaluates \code{expr} on the given container.
#'
#' @export
CivisFuture <- function(expr = NULL,
                        envir = parent.frame(),
                        substitute = FALSE,
                        globals = TRUE,
                        packages = NULL,
                        lazy = FALSE,
                        local = TRUE,
                        gc = FALSE,
                        earlySignal = FALSE,
                        label = NULL,
                        required_resources = list(cpu = 1024, memory = 2048, diskSpace = 4),
                        docker_image_name = "civisanalytics/datascience-r",
                        docker_image_tag = "2",
                         ...) {

  gp <- future::getGlobalsAndPackages(expr, envir = envir, globals = globals)

  ## if there are globals, assign them in envir
  env <- new.env()
  if (length(gp) > 0) {
    env <- list2env(gp$globals)

  }

  future <- future::Future(expr = expr,
                           envir = env,
                           substitute = substitute,
                           globals = gp$globals,
                           packages = unique(c(packages, gp$packages)),
                           lazy = lazy,
                           local = local,
                           gc = gc,
                           earlySignal = earlySignal,
                           label = label,
                           version = "1.8",  # see: https://github.com/civisanalytics/civis-r/issues/168

                           # extra args to scripts_post_containers
                           required_resources = required_resources,
                           docker_image_name = docker_image_name,
                           docker_image_tag = docker_image_tag, ...)
  future$.callResult <- TRUE
  structure(future, class = c("CivisFuture", class(future)))
}

#' @export
#' @param future CivisFuture object.
#' @describeIn CivisFuture Run a CivisFuture
run.CivisFuture <- function(future, ...) {
  if (is.null(future$job$containerId)) {
    cargo <- c(expr = future$expr, envir = future$envir,
               packages = list(future$packages))
    task_file_id <- write_civis_file(cargo)
    runner_file_id <- upload_runner_script()
    cmd <- make_docker_cmd(task_file_id, runner_file_id)
    future$job <- scripts_post_containers(future$required_resources,
                                          docker_command = cmd,
                                          docker_image_name = future$docker_image_name,
                                          docker_image_tag = future$docker_image_tag, ...)
    future$job <- scripts_post_containers_runs(future$job$id)
    future$state <- c("running")
  }
  return(future)
}

#' @export
#' @describeIn CivisFuture Return the value of a CivisFuture
result.CivisFuture <- function(future, ...) {
  if (future$state == "created") {
    future <- run(future)
  }

  # if the result isn't collected, try to collect it.
  if (is.null(future$result)) {
    tryCatch({
      future$run <- await(scripts_get_containers_runs, id = future$job$containerId,
                          run_id = future$job$id)
      future$state <- future$run$state
      value <- read_civis(civis_script(future$job$containerId),
                          using = readRDS)[[1]]
      future$result <- future::FutureResult(value=value)
      future$logs  <- fetch_logs(future$run)
    }, civis_error = function(e) {
      future$state <- "failed"
      e$message <- paste0(c(e$message, fetch_logs(e)), collapse = "\n")
      stop(e)
    }, error = function(e) stop(e))
  }
  future$result
}

#' Cancel the evaluation of a CivisFuture.
#' @param future CivisFuture object.
#' @param ... unused for CivisFuture.
#' @export
cancel <- function(future, ...) {
  UseMethod("cancel")
}

#' @export
#' @describeIn CivisFuture Cancel a CivisFuture
cancel.CivisFuture <- function(future, ...) {
  scripts_delete_containers_runs(id = future$job$containerId, run_id = future$job$id)
  future$state <- "cancelled"
}

#' @export
#' @describeIn CivisFuture Check if a CivisFutre has resolved
resolved.CivisFuture <- function(future, ...){
  if (!is.null(future$job$containerId)) {
    future$state <- scripts_get_containers_runs(id = future$job$containerId,
                                              run_id = future$job$id)$state
  }
  future$state %in% c("succeeded", "failed", "cancelled")
}

#' @export
#' @param object CivisFuture
#' @describeIn CivisFuture Fetch logs from a CivisFuture
fetch_logs.CivisFuture <- function(object, ...){
  object$logs
}

make_docker_cmd <- function(task_file_id, run_script_file_id) {
  # this loads the same initial packages in the same order as default R.
  cmd <- "Rscript -e \"civis::download_civis(${run_script_file_id}, 'run.R')\" && \
    Rscript --default-packages=methods,datasets,utils,grDevices,graphics,stats run.R ${task_file_id}"
  stringr::str_interp(cmd)
}

upload_runner_script <- function() {
  path <- system.file("scripts", "r_remote_eval.R", package = "civis")
  write_civis_file(path, name = "r_remote_eval.R")
}
