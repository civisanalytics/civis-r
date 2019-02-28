headers <- function(response) {
  attr(response, "headers")
}

is_paginated <- function(response) {
  h <- headers(response)
  "x-pagination-current-page" %in% names(h)
}

current_page <- function(response) {
  h <- headers(response)
  as.numeric(h$`x-pagination-current-page`)
}

total_pages <- function(response) {
  h <- headers(response)
  as.numeric(h$`x-pagination-total-pages`)
}

next_page <- function(response) {
  n <- current_page(response) + 1
  if (n > total_pages(response)) {
    return(NULL)
  } else {
    return(n)
  }
}

#' Retrieve all results from a paginated endpoint
#'
#' @param fn The API function to be called.
#' @param ... Arguments passed to \code{fn}.
#'
#' @return A \code{list} with the concatenated results of each page of \code{fn}.
#' @export
#'
#' @family pagination
#'
#' @examples \dontrun{
#' columns <- fetch_all(tables_list_columns, id = 123)
#' column_names <- columns %>% purrr::map_chr("name")
#' }
fetch_all <- function(fn, ...) {
  fetch_until(fn, ~FALSE, ...)
}

#' Retrieve some results from a paginated endpoint
#'
#' \code{fetch_until} will retrieve paginated results until a condition is met.
#' This is useful when searching for a particular value or record.
#'
#' @param fn The API function to be called.
#' @param .until A function or formula which returns a boolean value.
#' \code{.until} will be called with each item from the API response. When
#' \code{.until} returns \code{TRUE} iteration will stop and \code{fetch_until}
#' will return all responses accumulated so far.
#' @param ... Arguments passed to \code{fn}.
#'
#' @return A \code{list} with the concatenated results of each page of \code{fn}.
#' @export
#'
#' @family pagination
#'
#' @examples \dontrun{
#' columns <- fetch_until(tables_list_columns,
#'                        .until = function(x) x == "voterbase_id")
#' }
fetch_until <- function(fn, .until, ...) {
  until <- purrr::as_mapper(.until)
  args <- list(...)

  args$page_num <- 1
  args$limit <- NULL
  full_results <- list()

  repeat {
    res <- do.call(fn, args)
    full_results <- append(full_results, res)
    if (any(purrr::map_lgl(res, until))) {
      break
    }

    args$page_num <- next_page(res)
    if (is.null(args$page_num)) {
      break
    }
  }

  full_results
}

#' Retrieve items from a paginated endpoint matching keys
#' @param fn function, the API function to be called (usually a list endpoint)
#' @param keys list of key value pairs.
#' @return Items from \code{fn} that match all key value pairs in \code{keys}.
#' @param ... Arguments passed to \code{fn}.
#' @examples
#' \dontrun{
#' find(credentials_list, list(username = 'username', name = 'cred_name')))
#' find(templates_list_scripts, list(name = 'template name'))
#'
#' }
#' @export
find <- function(fn, keys, ...) {
  args <- list(...)

  # all keys must match values for one response x
  is_match <- function(x, keys) {
    all(purrr::map2_lgl(names(keys), keys, .f = function(key, value) {
      if (key %in% names(x)) x[[key]] == value else FALSE
    }))
  }

  args$page_num <- 1
  args$limit <- NULL
  results <- list()
  repeat {
    res <- do.call(fn, args)
    match <- res[purrr::map_lgl(res, is_match, keys = keys)]
    results <- append(results, match)
    args$page_num <- next_page(res)
    if (is.null(args$page_num)) {
      break
    }
  }
  return(results)
}
