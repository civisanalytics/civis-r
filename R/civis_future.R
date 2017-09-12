#' Evaluate an expression in Civis Platform
#'
#' @param ... Arguments to \code{\link{CivisFuture}}
#' @return The result of evaluating \code{expr}.
#'
#' @examples \dontrun{
#'  eval_civis(quote(sqrt(10)))
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
#' @param required_resources resources
#' @param docker_image_name the image for the container script.
#' @param docker_image_tag the tag for the Docker image.
#' @param ... arguments to \code{\link{scripts_post_containers}}
#'
#' @return A future evaluating \code{expr} on the given container.
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
                        docker_image_tag = "2.0.0",
                         ...) {

  # import_future imports the function from future without :::, which triggers R CMD CHECK
  getGlobalsAndPackages <- import_future("getGlobalsAndPackages")
  gp <- getGlobalsAndPackages(expr, envir = envir, globals = globals)

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

                           # extra args to scripts_post_containers
                           required_resources = required_resources,
                           docker_image_name = docker_image_name,
                           docker_image_tag = docker_image_tag, ...)
  structure(future, class = c("CivisFuture", class(future)))
}

#' @importFrom future run value resolved
NULL

#' @export
run.CivisFuture <- function(future, ...) {
  cargo <- c(expr = future$expr, envir = future$envir, packages = list(future$packages))
  task_file_id <- write_civis_file(cargo)
  runner_file_id <- upload_runner_script()
  cmd <- make_docker_cmd(task_file_id, runner_file_id)
  future$job <- scripts_post_containers(future$required_resources,
                                        docker_command = cmd,
                                        docker_image_name = future$docker_image_name,
                                        docker_image_tag = future$docker_image_tag, ...)
  future$job <- scripts_post_containers_runs(future$job$id)
  future$state <- c("running")
  future
}

#' @export
value.CivisFuture <- function(future, ...) {
  if (future$state == "created") {
    future <- run(future)
  }
  if (!future$state %in% c("succeeded", "failed", "cancelled")) {
    # get the run from the future object
    tryCatch({
      runs <- await(scripts_get_containers_runs, id = future$job$containerId, run_id = future$job$id)
      future$state <- runs$state
      future$run <- runs
      future$value <- fetch_output(runs)
    }, error = function(e) {
      future$state <- "failed"
      stop(e)
    })
  }
  future$value
}

#' Cancel the evaluation of a CivisFuture.
#' @param future CivisFuture object to be cancelled.
#' @param ... unused for CivisFuture.
#' @export
cancel <- function(future, ...) {
  UseMethod("cancel")
}

#' @export
cancel.CivisFuture <- function(future, ...) {
  scripts_delete_containers_runs(id = future$job$containerId, run_id = future$job$id)
  future$state <- "cancelled"
}

#' @export
resolved.CivisFuture <- function(future, ...){
  future$state %in% c("succeeded", "failed", "cancelled")
}

#' @export
fetch_logs.CivisFuture <- function(object, limit, ...){
  logs <- scripts_list_containers_runs_logs(object$job$containerId, run_id = object$job$id, limit = limit)
  format_scripts_logs(logs)
}

fetch_output <- function(run) {
  outputs <- scripts_list_containers_runs_outputs(run$containerId, run$id)
  # We only save one object in the run script.
  output_file_id <- outputs[[1]][["objectId"]]
  read_civis(output_file_id)
}

make_docker_cmd <- function(task_file_id, run_script_file_id) {
  cmd <- "Rscript -e \"civis::download_civis(${run_script_file_id}, 'run.R')\" && Rscript run.R ${task_file_id}"
  stringr::str_interp(cmd)
}

upload_runner_script <- function() {
  path <- system.file("scripts", "r_remote_eval.R", package = "civis")
  write_civis_file(path, name = "r_remote_eval.R")
}

# This is to get around R CMD check
import_from <- function(name, default = NULL, package) {
  ns <- getNamespace(package)
  if (exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    get(name, mode = "function", envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such '%s' function: %s()", package, name))
  }
}

import_future <- function(name, default = NULL) {
  import_from(name, default = default, package = "future")
}

