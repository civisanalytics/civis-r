#' Call a function repeatedly until a status is reached.
#'
#' @description
#' \code{await} repeatedly calls a Civis API endpoint such as \code{scripts_get_sql_runs}
#' that monitors the status of a script, job, import, or model. It blocks until the function
#' returns a result with a successful or error status.
#' If the script, job, import or model results in an error state,
#' \code{await} throws an error with useful debugging information.
#'
#' \code{await_all} is a vectorized version of \code{await}. It repeatedly calls a Civis API endpoint for
#' all values of a vector, e.g. a vector of script, job, import, run, or model ids. It blocks until
#' all calls have returned a result with a given status, and silently captures jobs that return
#' errors.
#'
#' @param f function to be called repeatedly until a status is reached.
#' @param ... arguments to \code{f}
#' @param .status_key The name of the element of the list returned by \code{f} containing the status.
#' For most Civis API endpoints, this is the default, \code{"state"}.
#' @param .success_states list of states indicating remote work has completed successfully.
#' For most Civis API endpoints, this set of states is the default, \code{c("succeeded", "success")}.
#' @param .error_states list of states indicating remote work is in an error state. For most Civis
#' API endpoints, this set of states is the default, \code{c("failed", "cancelled")}.
#' @param .timeout Number of seconds after which to timeout.
#' @param .interval The interval for retries (in seconds). If \code{NULL} (default), use exponentially increasing
#' intervals with jitter (see 'Details')
#' @param .verbose Print the status of \code{f} at a given retry with the retry time (default \code{FALSE})
#' @examples
#' \dontrun{
#'
#'    # Executing a query
#'    q_id <- queries_post(db_id, query, n_rows, cred_id)[["id"]]
#'    r <- await(queries_get, id = q_id)
#'    get_status(r)
#'
#'    r <- tryCatch(await(queries_get, id = q_id), error = function(e) e)
#'    get_error(r)
#'
#'    r <- try(await(queries_get, id = q_id))
#'    get_error(r)
#'
#' }
#' @export
#' @details
#'
#' \code{await} and \code{await_all} can wrap Civis API endpoints in \code{generated_client.R}.
#' The default values for \code{.status_key}, \code{.success_states}, and \code{.error_states}
#' are suitable for most endpoints. The final status of \code{f} can be obtained using
#' \code{\link{get_status}}.
#'
#' If an error state is reached, \code{await} throws a \code{civis_await_error}.
#' \code{await_all} silently captures and returns a \code{civis_await_error} for any job
#' reaching an error state as an element in the list of results.
#'
#' If \code{.timeout} is specified and the job fails to reach a success state
#' within the time limit, \code{await} throws a \code{civis_timeout_error}.
#' Likewise, \code{await_all} throws a \code{civis_timeout_error} if all jobs fail to
#' reach a success state within the time limit.
#'
#' These errors can be caught using \code{try} or \code{tryCatch}.
#' Useful debugging information can be returned using \code{\link{get_error}} and \code{\link{fetch_logs}}.
#'
#' The set of possible states for jobs on Civis platform are:
#' \code{"succeeded"}, \code{"success"}, \code{"failed"}, \code{"queued"}, \code{"running"},
#' and \code{"cancelled"}.
#'
#' Unless \code{.interval} is specified, retries are attempted with exponentially increasing intervals using
#' \code{.25 * (1.2^i)) + runif(1, 0, .2)}, where \code{i} is the index of the current retry.
#' Approximate intervals for a given number of retries are as follows:
#' \itemize{
#' \item{1-5: .5s}
#' \item{6-10: 1-5s}
#' \item{11-19: 5-10s}
#' \item{20-29: 10s - 1m}
#' }
#' @seealso \code{\link{get_status}, \link{get_error}, \link{fetch_logs}}
await <- function(f, ...,
                  .status_key = "state",
                  .success_states = c("succeeded", "success"),
                  .error_states = c("failed", "cancelled"),
                  .timeout = NULL, .interval = NULL,
                  .verbose = FALSE) {
  start <- Sys.time()
  i <- 1
  fname = as.character(substitute(f))
  repeat {
    r <- call_once(f, ..., .status_key = .status_key,
                   .success_states = .success_states,
                   .error_states = .error_states,
                   fname = fname)

    if (r$called) return(r$response)
    status <- get_status(r$response)

    if (!is.null(.timeout)) {
      running_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))
      if (running_time > .timeout) stop(civis_timeout_error(fname, list(...), status))
    }

    interval <- if (is.null(.interval)) interval_jitter(i) else .interval
    if (.verbose) {
      pretty_time <- formatC(interval, digits = 3, format = "fg")
      msg <- paste0("Status: ", status, " @ ", Sys.time(),
                    ". Retry ", i, " in ", pretty_time, " seconds")
      message(msg)
    }
    Sys.sleep(interval)
    i <- i + 1
  }
}

#' @param .x a vector of values to be passed to \code{f}
#' @param .y a vector of values to be passed to \code{f} (default \code{NULL})
#' @export
#' @describeIn await Call a function repeatedly for all values of a vector until all have reached a completed status
await_all <- function(f, .x, .y = NULL, ...,
                      .status_key = "state",
                      .success_states = c("succeeded", "success"),
                      .error_states = c("failed", "cancelled"),
                      .timeout = NULL, .interval = NULL,
                      .verbose = FALSE) {


  responses <- vector(mode = "list", length = length(.x))
  called <- rep(FALSE, length(.x))
  i <- 1
  start <- Sys.time()
  fname <- as.character(substitute(f))

  if (!is.null(.y) & (length(.x) != length(.y))) {
    error <- c("Lengths of input parameters (.x and .y) are not equal!")
    stop(error)
  }

  zipped_parameters <- if (is.null(.y)) .x else mapply(c, .x, .y, SIMPLIFY=FALSE)

  repeat {
    responses[!called] <- lapply(zipped_parameters[!called], safe_call_once,
                                 f = f, ..., .status_key = .status_key,
                                 .success_states = .success_states,
                                 .error_states = .error_states,
                                 fname = fname)

    called <- unlist(lapply(responses, function(x) x$called))

    if (all(called)) {
      return(lapply(responses, maybe_response))
    }

    if (!is.null(.timeout)) {
      running_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))
      if (running_time > .timeout) {
        args <- c(list(zipped_parameters), list(...))
        names(args)[1] <- names(formals(f))[1]
        status <- unlist(lapply(responses, function(x) get_status(x$response)))
        stop(civis_timeout_error(fname, args, status))
      }
    }

    interval <- if (is.null(.interval)) interval_jitter(i) else .interval

    if (.verbose) {
      pretty_time <- formatC(interval, digits = 3, format = "fg")
      make_msg <- function(x) {
        msg <- paste0("Task: ", x, " Status: ", responses[[x]]$response[[.status_key]],
                      " @ ", Sys.time(),
                      ". Retry ", i, " in ", pretty_time, " seconds")
        message(msg)
      }
      lapply(seq_along(zipped_parameters), make_msg)
    }
    Sys.sleep(interval)
    i <- i + 1
  }
}

# This is just so we can throw civis_errors and not try-errors in await_all
safe_call_once <- function(...) {
  tryCatch(call_once(...), error = function(e) e)
}

# .id is the first argument to f.
call_once <- function(f, ..., .id = NULL, .status_key = "state",
                      .success_states = c("succeeded"),
                      .error_states = c("failed", "cancelled"), fname) {
  response <- do.call(f, c(.id, list(...)))
  status <- response[[.status_key]]
  if (is.null(status)) stop("Cannot find status")

  called <- any(status %in% .success_states)

  if (any(status %in% .error_states)) {
    args <- c(.id, list(...))
    names(args)[1] <- names(formals(f))[1]
    # queries_post uses response$exception for errors
    error <- response$error %||% response$exception
    stop(civis_await_error(fname, args, status = status, error = error))
  }
  response <- structure(response,
                        status = status,
                        fname = fname,
                        args = list(...))
  return(list(response = response, called = called))
}

#' Get the status from results of \code{await}
#' @param response the results from \code{await}
#' @export
#' @seealso await await_all
get_status <- function(response) {
  attr(response, "status")
}

#' Get error data from civis_errors
#' @param x The error, usually from \code{tryCatch}
#' @return A list containing
#'  \item{f}{The function}
#'  \item{args}{A list of named arguments \code{f} was called with}
#'  \item{error}{The error message from platform (if any)}
#'  \item{status}{The status}
#' @export
#' @examples
#' \dontrun{
#'  q_id <- queries_post(db_id, query, n_rows, cred_id)[["id"]]
#'  r <- tryCatch(await(queries_get, id = q_id), error = function(e) e)
#'  print(r)
#'  get_error(r)
#'
#'  r <- try(await(queries_get, id = q_id))
#'  get_error(r)
#'  }
get_error <- function(x) {
  UseMethod("get_error")
}

#' @export
get_error.civis_error <- function(x) {
  as.list(attributes(x))
}

# will work with try-error
#' @export
get_error.default <- function(x) {
  as.list(attributes(attr(x, "condition")))
}

#' @export
print.civis_await_error <- function(x, ...) {
  cat("<civis_await_error>", fill = T)
  cat(x$message)
}

#' @export
print.civis_timeout_error <- function(x, ...) {
  cat("<civis_timeout_error>", fill = T)
  cat(x$message)
}

interval_jitter <- function(i) {
  #  i  < 5      : .5s
  #  5  < i < 10 : 1-5s
  #  10 < i < 20 : 5-10s
  #  i  > 20     : 10s-1min
  val <- .25 * 1.2 ^ i
  jitter <- stats::runif(1, 0, .2)
  val + jitter
}

civis_await_error <- function(fname, args, msg, error, ...) {
  msg <- await_err_msg(fname, args, error)
  condition(c("civis_await_error", "civis_error", "error"), msg, call = NULL,
            f = fname, args = args, error = error, ...)
}

civis_timeout_error <- function(fname, args, status, ...) {
  msg <- paste0("Timeout exceeded. Current status: ",
                paste0(status, collapse = ", "))
  condition(c("civis_timeout_error", "civis_error", "error"), msg, call = NULL,
            f = fname, args = args, ..., status = status)
}

maybe_response <- function(x) {
  if (is(x, "civis_error")) return(x)
  x$response
}

await_err_msg <- function(fname, args = NULL, error = NULL) {
  arg_str <- ""
  arg_str  <- if (!is.null(args)) paste0(names(args), " = ", args, collapse = ", ")
  paste0(fname, "(", arg_str, "): ", error)
}
