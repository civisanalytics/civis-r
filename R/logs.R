#' Fetch job logs from the Civis Platform
#'
#' @param object An api response, CivisML model, or other object from the Civis
#'   Platform.
#' @param limit The number of log lines to fetch, the default is 100.
#' @param \dots Extra parameters passed to methods, currently unused.
#'
#' @return A list of log messages with class \code{civis_logs}.
#'
#' @examples \dontrun{
#' m <- civis_ml_fetch_existing(123)
#' fetch_logs(m)
#' }
#'
#' @export
fetch_logs <- function(object, limit = 100, ...) {
  UseMethod("fetch_logs", object)
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
