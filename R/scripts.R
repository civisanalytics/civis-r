#' Civis Script
#'
#' Create a civis_script object from a job and run id.
#' @param id integer, job id of the script.
#' @param run_id integer, id of the run. If \code{NULL} (default), the most recent run will be used.
#' @details
#' A \code{civis_script} can be any custom, container, R, Python, SQL, or Javacript platform script.
#' @return a \code{civis_script} object.
#' @export
#' @seealso \code{\link{read_civis}}
#' @family script_utils
civis_script <- function(id, run_id = NULL) {
  if (length(id) != 1) stop("id must be length 1")
  if (!is.null(run_id) && length(run_id) != 1) stop("run_id must be length 1 or NULL")
  obj <- list(id = id, run_id = run_id)
  class(obj) <- "civis_script"
  return(obj)
}

#' @return A named list of run output file ids with names matching \code{regex}.
#' @param x civis_script
#' @param regex string, regex used to match the run output name.
#' @export
#' @family script_utils
#' @details If the script has no outputs, the results are a list of length is 0.
#' @examples
#' \dontrun{
#' out <- fetch_output(civis_script(1234))
#'
#' # Filter output with regex, then read into memory:
#' ids <- fetch_output_file_ids(civis_script(1234), regex = '.csv')
#' vals <- lapply(ids, read_civis, using = read.csv)
#' }
fetch_output_file_ids <- function(x, regex = NULL) {
  output <- fetch_output(x, regex)
  names <- lapply(output, function(o) o$name)
  out <- stats::setNames(lapply(output, function(o) o$objectId), names)
  return(out)
}

#' Return output from a civis_script.
#' @describeIn fetch_output_file_ids Return output of \code{scripts_list_*_runs_outputs} matching \code{regex}.
#' @inheritParams fetch_output_file_ids
#' @family script_utils
#' @export
fetch_output <- function(x, regex = NULL) {
  job <- jobs_get(x$id)
  run_id <- if (is.null(x$run_id)) job$lastRun$id else x$run_id
  get_output <- get_script_fun(job, 'list', 'outputs')
  output <- get_output(x$id, run_id)
  names <- sapply(output, function(o) o$name)
  if (!is.null(regex)) {
    match <- grep(regex, names)
    output <- output[match]
  }
  return(output)
}

#' Evaluate an R expression in a Civis Platform container
#' @param expr code to evaluate
#' @param ... arguments to \code{\link{CivisFuture}}
#' @details
#' \code{run_civis} blocks until completion. For non-blocking calls,
#' use futures directly with \code{\link{civis_platform}}.
#' Attempts are made at detecting and installing necessary packages
#' within the container, and detecting global variables required in \code{expr}.
#'
#' @family script_utils
#' @examples
#' \dontrun{
#' run_civis(2+2)
#'
#' # specify required resources, and a specific image
#' run_civis(2+2,
#'   required_resources = list(cpu = 1024, memory = 2048),
#'   docker_image_name='image',
#'   docker_image_tag = 'latest')
#'
#' }
#' @export
run_civis <- function(expr, ...) {
  future::plan(civis_platform)
  on.exit(future::plan("default"))
  fut <- future::future({expr}, ...)
  return(future::value(fut))
}

#' Run a template script
#' @param id id of the template script.
#' @param arguments list of arguments to the script.
#' @param JSONValue bool (default FALSE) If true, returns the 
#'                  JSON values instead of the file_ids
#' @param ... additional arguments to \code{scripts_post_custom}
#' @return If JSONValue is FALSE, File ids of any run outputs are returned.
#'         If JSONValue is TRUE, JSON values of first JSON run output is returned.
#'           If there are no JSON outputs, nothing is returned
#'           If there are more than 1 JSON outputs, error message is printed 
#'             and nothing is returned.
#'                              
#' @export
#' @family script_utils
#' @examples
#' \dontrun{
#' # Try a search for the template id
#' search_list('template name', type = 'template_script')
#'
#' # Run the template
#' run_template(id, arguments = list(arg1 = 1, arg2 = 2), ...)
#'
#' # Run the template and return JSON value outputs
#' run_template(id, arguments = list(arg1 = 1, arg2 = 2), JSONValue=TRUE...)
#' }
run_template <- function(id, arguments, JSONValue=FALSE, ...) {
  job <- scripts_post_custom(id, arguments = arguments, ...)
  run <- scripts_post_custom_runs(job$id)
  await(scripts_get_custom_runs, id = job$id, run_id = run$id)
  if (JSONValue) {

     output <- fetch_output(civis_script(job$id, run$id))
     json_output <- output[sapply(output, function(o) o$objectType=="JSONValue")]

     if (length(json_output) == 0) {
        warning("Error in returning JSON outputs of template run -- no JSON output")
	return()
     }
     if (length(json_output) > 1) {
        warning("More than 1 JSON output for template. Returning only the first one")
     }
     return(json_output[[1]]$value)

  }
  else {
     file_ids = fetch_output_file_ids(civis_script(job$id, run$id))
     return(file_ids)
  }
}

#' Add a file as a run output if called from a container job
#'
#' @param filename string, name of the file to add as a run output
#'   \code{civis::scripts_post_*_runs_outputs}.
#' @return Returns the filename if not running on platform.
#' @details Only posts if running on Civis Platform.
#'
#' @export
write_job_output <- function(filename) {
  job_id <- Sys.getenv("CIVIS_JOB_ID")
  run_id <- Sys.getenv("CIVIS_RUN_ID")
  if (job_id != "" & run_id != "") {
    name <- basename(filename)
    file_id <- civis::write_civis_file(filename, name = name, expires_at = NULL)
    job <- jobs_get(job_id)
    post_output <- get_script_fun(job, verb = "post", fun_type = 'outputs')
    post_output(id = job_id,
                run_id = run_id,
                object_type = 'File',
                object_id = file_id)
  }
}

#' Get a script function matching a job type.
#' @param job output of \code{jobs_get}
#' @param verb one of \code{"list"} or \code{"post"}
#' @param fun_type one of \code{"logs"} or \code{"outputs"}
#' @return The correct output or log fetching function
#' (e.g. \code{\link{scripts_list_containers_runs_logs}}) based on the job type.
#' @details container and custom scripts both have the same job type, but can be distinguished
#' by a non-null \code{fromTemplateId}.
get_script_fun <- function(job, verb = c("list", "post"), fun_type = c("outputs", "logs")) {
  fun_type <- match.arg(fun_type)
  verb <- match.arg(verb)
  job_type <- job$type
  if (!is.null(job$fromTemplateId)) {
    name <- "custom"
  } else {
    name <- SCRIPT_MAPPING$name[which(SCRIPT_MAPPING$job_type == job_type)]
  }
  fname <- paste0("scripts_", verb, "_", name, "_runs_", fun_type)
  get(fname)
}

# containers and custom scripts both have type 'ContainerDocker'
# sql is of type SqlRunner
# javascript is of type ScriptedSql
SCRIPT_MAPPING <- data.frame(
  job_type = paste0("JobTypes::", c('ContainerDocker', 'PythonDocker', 'RDocker', 'SqlRunner', 'ScriptedSql')),
  name = c('containers', 'python3', 'r', 'sql', 'javascript')
)


