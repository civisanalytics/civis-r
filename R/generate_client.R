IGNORE <- c("apps")
FILENAME <- c("R/generated_client.R")
BASE_RESOURCES_V1 = c("credentials", "databases", "files", "imports",
                      "jobs", "models", "predictions", "projects",
                      "queries", "reports", "scripts", "tables", "users")

#' Fetches and generates the client in generated_client.R
#'
#' @details
#' Skips autogeneration on windows with R < 3.4.0 and if R_CLIENT_DEV == "TRUE".
#' Skips downloading a fresh spec if CIVIS_API_KEY is not set.
#' @importFrom devtools document
#' @importFrom roxygen2 roxygenize
fetch_and_generate_client <- function() {
  if (Sys.getenv("R_CLIENT_DEV") != "TRUE" && windows_r_version_is_valid()) {
    message("Generating API")
    spec <- get_spec()
    client_str <- generate_client(spec, IGNORE = IGNORE)
    message(paste0("Writing API to ", FILENAME))
    write_client(client_str, FILENAME = FILENAME)
    devtools::document()
  } else {
    message("Skipping client generation")
  }
}

windows_r_version_is_valid <- function(major = 3, minor = 3.4) {
  valid <- TRUE
  if (.Platform$OS.type == "windows") {
    valid <- as.numeric(R.version$major) >= major && as.numeric(R.version$minor) >= minor
  }
  if (!valid) message("Autogenerating API on Windows requires R > 3.4.0")
  valid
}

#' Generate a client
#' @param spec usually from \code{get_spec}
#' @param IGNORE endpoints to ignore (e.g. "apps")
#' @return A string containing one documented function for each verb at each endpoint.
generate_client <- function(spec, IGNORE) {
  client_str <- ""
  ign_regex <- paste0("^/", IGNORE)
  paths <- with(spec, paths[!grepl(ign_regex, names(paths))])

  for (i in seq_along(paths)) {
    path <- paths[[i]]
    for (j in seq_along(path)) {
      verb <- path[[j]]
      verb <- replace_ref(verb, spec, previous_ref = "")
      params <- parse_params(verb)
      path_name <- names(paths)[i]
      verb_name <- names(path)[j]

      docs <- build_docs(verb)
      name <- build_function_name(verb_name, path_name)
      args <- build_function_args(params)
      body <- build_function_body(verb, verb_name, path_name, params)
      client_str  <- paste0(client_str, docs, name, args, body, "\n\n")
    }
  }
  client_str
}

build_function_body <- function(verb, verb_name, path_name, params) {
  has_params <- length(params) > 0
  path_params  <- if (has_params) with(params, name[http_location == "path"]) else NULL
  query_params <- if (has_params) with(params, name[http_location == "query"]) else NULL
  body_params  <- if (has_params) with(params, name[http_location == "body"]) else NULL
  path_param_str  <- if (length(path_params) > 0) paste0(path_params, " = ", camel_to_snake(path_params), collapse = ", ") else ""
  query_param_str <- if (length(query_params) > 0) paste0(query_params, " = ", camel_to_snake(query_params), collapse = ", ") else ""
  body_param_str  <- if (length(body_params) > 0) paste0(body_params,  " = ", camel_to_snake(body_params),  collapse = ", ") else ""

   paste0(
    "  args <- as.list(match.call())[-1]\n",
    "  path <- \"", path_name, "\"\n",
    "  path_params  <- list(", path_param_str,  ")\n",
    "  query_params <- list(", query_param_str, ")\n",
    "  body_params  <- list(", body_param_str,  ")\n",
    "  path_params  <- path_params[match_params(path_params, args)]\n",
    "  query_params <- query_params[match_params(query_params, args)]\n",
    "  body_params  <- body_params[match_params(body_params, args)]\n",
    "  resp <- call_api(\"", verb_name, "\", path, path_params, query_params, body_params)\n\n",
    "  return(resp)\n\n }\n"
  )
}

build_function_args <- function(params) {
  default_arg <- ifelse(params$required, "", " = NULL")
  arg_str <- paste0(camel_to_snake(params$name), default_arg, collapse = ", ")
  paste0(" <- function(", arg_str, ") {\n\n")
}

build_function_name <- function(verb_name, path_name) {
  parts <- stringr::str_split(path_name, "/", simplify = TRUE)
  parts <- purrr::discard(parts, ~ .x == "")
  parts <- purrr::discard(parts, ~ startsWith(.x, "{"))
  parts <- gsub("-", "_", parts)

  if (!endsWith(path_name, "}") & verb_name == "get") verb_name <- "list"

  paste(c(parts[1], verb_name, parts[-1]), collapse = "_")
}

build_docs <- function(verb) {
  title <- sprintf("#' %s\n", verb$summary)
  param_docs <- build_params_docs(verb)
  resp_docs <-  build_resp_docs(verb)
  doc_str <- escape_percent(paste0(title, param_docs, "#' \n", resp_docs, "#' @export\n"))
  return(doc_str)
}

build_params_docs <- function(verb) {
  params <- verb$parameters
  doc_str <- ""
  params <- params[order(sapply(params, function(x) x$required), decreasing = T)]

  if (length(params) > 0) {
    for (i in seq_along(params)) {
      param <- params[[i]]
      if (is_obj(param)) {
        p <- get_obj_properties(param)
        req <- param$schema$required
        if (!is.null(req)) {
          # see scripts_post_sql
          preq <- p[names(p) %in% req]
          doc_str <- paste0(doc_str,
            write_properties(preq, "required", transform_name = camel_to_snake))

          popt <- p[!(names(p) %in% req)]
          doc_str <- paste0(doc_str,
            write_properties(popt, "optional", transform_name = camel_to_snake))
        } else {
          doc_str <- paste0(doc_str,
            write_properties(p, get_req_str(param), transform_name = camel_to_snake))
        }
      } else if (is_array(param)) {
        p <- get_array_properties(param)
        doc_str <- paste0(doc_str,
          write_properties(p, get_req_str(param), transform_name = camel_to_snake))
      } else {
        name <- camel_to_snake(param$name)
        doc_str <- paste0(doc_str,
                          sprintf("#' @param %s %s %s. %s", name,
                                  param$type, get_req_str(param), get_descr_str(param)),
                          "\n")
      }
    }
  }
  doc_str
}

build_resp_docs <- function(verb) {
  # The magic number '1' works because only a successful return (a 200-level code) is documented.
  # can be 200, 201, 202, 204.
  resp <- verb$responses[[1]]
  doc_str <- ""
  if (length(resp) > 1) {
    if (is_array(resp)) {
      p <- get_array_properties(resp)
      doc_str <- paste0(doc_str, " An array containing the following fields:\n",
                        write_properties(p, fmt = "#' \\item{%s}{%s, %s%s}"))
    } else if (is_obj(resp)) {
      p <- get_obj_properties(resp)
      doc_str <- paste0(doc_str, " A list containing the following elements:\n",
                        write_properties(p, fmt = "#' \\item{%s}{%s, %s%s}"))
    } else {
      doc_str <- "An undocumented HTTP response\n"
    }
  } else {
    doc_str <- " An empty HTTP response\n"
  }
  paste0("#' @return ", doc_str)
}

write_properties <- function(x, req_str="", fmt="#' @param %s %s %s. %s",
                             doc_str="", transform_name = I) {
  for (j in seq_along(x)) {
    if (is_nested(x[[j]])) {
      descr_str <- write_nested_docs(x[[j]])
    } else {
      descr_str <- get_descr_str(x[[j]])
    }
    name <- transform_name(names(x)[j])
    doc_str <- paste0(doc_str, sprintf(fmt, name, x[[j]]$type, req_str, descr_str), "\n")
  }
  doc_str
}

write_nested_docs <- function(x) {
  doc_str <- ""
  fmt <- "#' \\item %s %s, %s"
  if (is_array(x)) {
    ps <- get_array_properties(x)
    doc_str <- paste0(doc_str,
        "An array containing the following fields: \n#' \\itemize{\n")
    for (i in seq_along(ps)) {
      doc_str <- paste0(doc_str, sprintf(fmt, names(ps)[i], ps[[i]]$type,
                                         get_descr_str(ps[[i]])), "\n")
    }
    doc_str <- paste0(doc_str, "#' }")
  } else if (is_obj(x)) {
    ps <- get_obj_properties(x)
    doc_str <- paste0(doc_str,
        "A list containing the following elements: \n#' \\itemize{\n")
    for (i in seq_along(ps)) {
      doc_str <- paste0(doc_str,
        sprintf(fmt, names(ps)[i], ps[[i]]$type, get_descr_str(ps[[i]])), "\n")
    }
    doc_str <- paste0(doc_str, "#' }")
  }
  doc_str
}

parse_params <- function(verb, spec) {
  params <- verb$parameters
  arg_names <- location <- required <- description <- type <- list()

  for (i in seq_along(params)) {
    param <- params[[i]]

    # Case 1: param is an obj
    if (!is.null(param$schema)) {
      properties <- param$schema$properties
      for (j in seq_along(properties)) {
        arg_names <- c(arg_names, names(properties)[[j]])
        descr_str <- get_descr_str(properties[[j]])
        description <- c(description, descr_str)
        type <- c(type, properties[[j]]$type)
        location <- c(location, param$`in`)
      }
      required <- c(required, names(properties) %in% unlist(param$schema$required))

    } else {
      # Case 2: param is a list.
      arg_names <- c(arg_names, param$name)
      descr_str <- get_descr_str(param)
      description <- c(description, descr_str)
      type <- c(type, param$type)
      location <- c(location, param$`in`)
      required <- c(required, param$required)
    }
  }

  df <- data.frame(name = unlist(arg_names),
                   http_location = unlist(location),
                   required = unlist(required),
                   description = unlist(description),
                   type = unlist(type), stringsAsFactors = FALSE)

  # Put required arguments first.
  if (length(df) > 0) df <- df[order(df$required, decreasing = TRUE), ]
  df
}

# ----- References -----
#  works for:
#   replace_ref(verb$parameters[[1]], spec)
#   replace_ref(verb$responses, spec)

replace_ref <- function(x, spec, previous_ref = "") {
  x_replaced <- list()
  for (i in seq_along(x)) {
    val <- x[[i]]
    key <- names(x)[i]
    key <- if (is.null(key)) i else key
    if (is_ref(val)) {
      if (!same_ref(previous_ref, val)) {
        ref_val <- get_ref(val, spec)
        x_replaced[[key]] <- replace_ref(ref_val, spec = spec, previous_ref = val)
      } else {
        #x_replaced[[key]] <- val
      }
    } else if (is.list(val)) {
      x_replaced[[key]] <- replace_ref(val, spec = spec, previous_ref = previous_ref)
    } else {
      x_replaced[[key]] <- val
    }
  }
  return(x_replaced)
}

is_ref <- function(x) {
  "$ref" %in% names(x)
}

same_ref <- function(parent, child) {
  if (is_ref(parent) & is_ref(child)) parent$`$ref` == child$`$ref` else FALSE
}

# ref is a list with key="$ref" and value="#/definitions/Objectx"
get_ref <- function(ref, spec) {
  ref <- ref[which(names(ref) == "$ref")]  # see ex5
  ref_path <- stringr::str_split(ref, "/", simplify = TRUE)
  ref_path <- ref_path[-1]  # the first portion of the path is `#`
  spec[[ref_path]]
}

# ----- Utils -----
is_nested <- function(x) { is_array(x) | is_obj(x) }


is_array <- function(x) {
  if (!is.null(x$schema)) {
    flag <- (x$schema$type == "array") & (!is.null(x$schema$items$properties))
  } else if (!is.null(x$type)) {
    flag <- (x$type == "array") & !is.null(x$items$properties)
  } else {
    flag <- FALSE
  }
  flag
}

is_obj <- function(x) {
  if (!is.null(x$schema)) {
    flag <- (x$schema$type == "object" & !is.null(x$schema$properties))
  } else if (!is.null(x$type)) {
    flag <- (x$type == "object" & !is.null(x$properties))
  } else {
    flag <- FALSE
  }
  flag
}

get_descr_str <- function(x) {
  if (!is.null(x$description)) x$description else ""
}

get_req_str <- function(x) {
  doc_str <- ""
  if (!is.null(x$required)) {
    doc_str <- if (x$required) "required" else "optional"
  }
  doc_str
}

get_array_properties <- function(x) {
  if (!is.null(x$schema)) x$schema$items$properties else x$items$properties
}

get_obj_properties <- function(x) {
  if (!is.null(x$schema)) x$schema$properties else x$properties
}

returns_array <- function(verb, verb_name, path_name) {
  if (verb_name == "get" & !endsWith(path_name, "}")) return(TRUE)
  rtype <- verb$responses[[1]]$schema$type
  if (!is.null(rtype) && (rtype == "array")) return(TRUE)
  return(FALSE)
}


# ---- Main ----
get_spec <- function() {

  # Skip retries whenever api_key not set (e.g. Travis, Appveyor, CRAN)
  if (!inherits(try(api_key(), silent = TRUE), "try-error")) {
    api_spec <- try(call_api("get", path = "/endpoints/", list(), list(), list()))
    if (inherits(api_spec, "try-error")) {
      message("Downloading API specification from Civis failed, using cached API specification.")

      # loads cached spec generated from 'tools/generate_default_client.R'
      load("R/sysdata.rda")
    } else {
      message("Downloading API specification from Civis successful.")
    }
  } else {
    message("The environment variable CIVIS_API_KEY is not set, using cached API specification.")
    load("R/sysdata.rda")
  }
  api_spec
}

write_client <- function(client_str, FILENAME) {
  cat("", file = FILENAME)
  cat(client_str, file = FILENAME, append = TRUE)
}
