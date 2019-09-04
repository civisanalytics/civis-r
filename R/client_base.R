#' Print results from a Civis API call
#'
#' @param x A \code{civis_api} response.
#' @param ... Further arguments passed to \code{str}
#'
#' @examples
#' \dontrun{
#' query_civis("SELECT * FROM schema.my_table", "database", preview_rows = 100)
#' csv_to_civis("file.csv", "my_database", "schema.my_table")
#' print(databases_list(), max = 5)
#' }
#' @return The `civis_api` object, invisibly.
#' @export
print.civis_api <- function(x, ...) {
  cat("<civis_api>\n")
  resp <- x
  nullify <- !(names(attributes(x)) %in% c("class", "names"))
  attributes(x)[nullify] <- NULL
  utils::str(x, ...)
  invisible(resp)
}

call_api <- function(verb, path, path_params, query_params, body_params) {
    url <- build_url(path, path_params)
    auth <- httr::authenticate(api_key(), "")

    # this works when call_api is used to download spec before package is installed
    pkg_version <- tryCatch(version(), error = function(e) "")

    user_str <- sprintf("civis-r/%s %s %s", as.character(pkg_version),
                        R.version$version.string, utils::sessionInfo()$platform)
    user_agent <- httr::user_agent(user_str)

    request_args <- list(verb, url, auth, user_agent, query = query_params)
    if (length(body_params) > 0) {
      body_json <- jsonlite::toJSON(body_params, auto_unbox = TRUE, null = "null")
      request_args <- c(request_args, list(body = body_json, httr::content_type_json()))
    }
    if (tolower(verb) %in% c("get", "put")) {
      # Retry get and put for these error codes, in addition to 429.
      retry_on <- c(413, 429, 502, 503, 504)
      terminate_on <- setdiff(200:527, retry_on)
      request <- httr::RETRY
      request_args <- c(request_args, list(terminate_on = terminate_on,
                                           pause_cap = 600,
                                           times = 10))
    } else {
      # Retry on all other verbs only if 429.
      terminate_on <- setdiff(200:527, 429)
      request_args <- c(request_args, list(terminate_on = terminate_on,
                                           pause_cap = 600,
                                           times = 10))
      request <- httr::RETRY
    }
    response <- do.call(request, request_args)

    stop_for_status(response,
                    paste(response$request$method, response$request$url))

    if (response$status_code %in% c(204, 205)) {
      content <- list()
    } else {
      content <- try(httr::content(response), silent = TRUE)
    }

    if (is.error(content)) {
      msg <- paste0("Unable to parse JSON from response")
      call <- build_function_name(verb, path)
      cond <- condition(c("civis_api_error", "civis_error", "error"), msg, call = call, response = response)
      stop(cond)
    }

    structure(content,
              headers = httr::headers(response),
              response = response,
              path = path,
              class = c("civis_api", class(content)))
}

# Taken from Adv-R
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}

api_key <- function() {
  key <- Sys.getenv("CIVIS_API_KEY")
  if (key == "") {
    stop("The environmental variable CIVIS_API_KEY is not set. Add this to your ",
         ".Renviron or call Sys.setenv(CIVIS_API_KEY = '<api_key>')")
  }
  return(key)
}

base_url <- function() {
  url <- Sys.getenv("CIVIS_API_ENDPOINT")
  if (url == "") {
    return("https://api.civisanalytics.com")
  }
  return(url)
}

build_url <- function(path, path_params) {
  filled_path <- fill_text_template(path, path_params)
  paste0(base_url(), filled_path)
}

# fill a template "/scripts/{id}/runs/{run_id}" with values from a list
# list(id = 9, run_id = 12) to make "/scripts/9/runs/12"
fill_text_template <- function(tmpl, values) {
    for (k in names(values)) {
        tmpl <- fill_text_template_value(tmpl, k, values[[k]])
    }
    return(tmpl)
}

fill_text_template_value <- function(tmpl, value_name, value) {
    pattern <- paste0("\\{", value_name, "\\}")
    gsub(pattern, value, tmpl)
}

# Taken from httr::stop_for_status.  This allows
# The error message present in the response to propagate to user
stop_for_status <- function(x, task = NULL) {
  if (httr::status_code(x) < 300)
    return(invisible(x))

  call <- sys.call(-1)
  error_msg <- httr::content(x)$errorDescription
  condition <- httr::http_condition(x, "error", task = task, call = call)
  condition$message <- paste0(c(condition$message, error_msg), collapse = " ")
  stop(condition)
}

version <- function() {
  utils::packageVersion("civis")
}

is.error <- function(x) {
  inherits(x, "try-error")
}
