#' Fetch job logs from the Civis Platform
#'
#' @param object A \code{civis_api}, \code{civis_error},  or \code{civis_ml} object.
#' @param limit The number of log lines to fetch, the default is 100.
#' @param \dots Extra parameters passed to methods, currently unused.
#'
#' @return A list of log messages with class \code{civis_logs}.
#'
#' @examples \dontrun{
#' m <- civis_ml_fetch_existing(123)
#' fetch_logs(m)
#'
#' import <- write_civis(iris, "scratch.mytest")
#' fetch_logs(import)
#'
#' query_run <- query_civis(123)
#' fetch_logs(query_run)
#'
#' e <- tryCatch(read_civis("asdf;"), error = function(e) e)
#' fetch_logs(e)
#' }
#'
#' @export
fetch_logs <- function(object, limit = 100, ...) {
  UseMethod("fetch_logs", object)
}

#' @export
fetch_logs.civis_api <- function(object, limit = 100, ...) {
  f <- find_log_fetcher(name = attr(object, "fname"))
  args <- attr(object, "args")
  logs <- do.call(f, c(args, limit = limit))
  format_scripts_logs(logs)
}

#' @export
fetch_logs.civis_error <- function(object, limit = 100, ...) {
  f <- find_log_fetcher(name = attr(object, "f"))
  args <- attr(object, "args")
  logs <- do.call(f, c(args, limit = limit))
  format_scripts_logs(logs)
}

find_log_fetcher <- function(name) {

  # This captures slightly more than necessary, but includes all candidates.
  available <- grep("*_logs$", ls(getNamespace("civis")), value = TRUE)

  # The only exception this rule is models_list_builds_logs, which we don't use.
  candidate <- paste0(gsub("get", "list", name), "_logs")

  fetcher <- grep(candidate, available, value = TRUE)
  if (length(fetcher) == 0) stop("No function to fetch logs.")
  get(fetcher)
}

#' @export
print.civis_logs <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}

format_scripts_logs <- function(logs) {
  messages <- purrr::map_chr(logs, format_scripts_message)
  # The messages come in reverse order, flip them so the user can read from
  # top down.
  messages <- rev(messages)
  structure(messages, class = "civis_logs")
}

format_scripts_message <- function(m) {
  # Messages from scripts_list_*_runs_logs are a list containing a `message`
  # and a `createdAt` timestamp.
  ts <- strftime(lubridate::ymd_hms(m$createdAt),
                 format = "%Y-%m-%d %H:%M:%S %p %Z")
  paste(ts, m$message)
}
