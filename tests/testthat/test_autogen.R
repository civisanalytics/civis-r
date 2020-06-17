# Tests
# RRV: 2017-11-20 for 1.1.1
context("autogen")

spec <- readRDS("data/full_spec.rds")
paths <- spec$paths

# ---- example data from spec

# 1. Parameters: ref.   Responses: ref.
# 2. Parameters: list.  Responses: array ref.
# 3. Parameters: empty array. Responses: array ref.
# 4. Parameters: list.  Responses: none.
# 5. Parameters: list.  Responses: ref.
# 6. Parameters: none.  Responses: list containing arrays.
# 7. Parameters: list.  Responses: infinite reference.
# 8. Parameters: list with ref. Responses: ref.
# 9. Parameters: list with ref and arrays. Responses: list with ref and arrays.
# 10. Parameters: argument with empty list.

ex <- list(paths[["/admin/dump-memory"]]$post,
           paths[["/admin/announcements"]]$get,
           paths[["/admin/organizations"]]$get,
           paths[["/admin/announcements/{id}"]]$delete,
           paths[["/bocce_clusters/{id}/active_jobs"]]$get,
           paths[["/channels/"]]$get,
           paths[["/jobs/{id}/children"]]$get,
           paths[["/admin/announcements/{id}"]]$patch,
           paths[["/scripts/sql"]]$post,
           paths[["/notebooks/{id}"]]$put)



ex_ref <- lapply(ex, replace_ref, spec = spec)
ex_verb_names <- c("post", "get", "get", "delete", "get", "get", "get", "patch", "post", "put", "get", 'get')
ex_path_names <- c("/admin/dump-memory", "/admin/announcements", "/admin/organizations",
                   "/admin/announcements/{id}", "/bocce_clusters/{id}/active_jobs",
                   "/channels/",
                   "/jobs/{id}/children", "/admin/announcements/{id}", "/scripts/sql",
                   "/notebooks/{id}", "/aliases/{object_type}/{alias}", '/databases/{id}/tables-search')
ex_params <- lapply(ex_ref, parse_params)

# ----- test utils

str_detect_multiple <- function(string, pattern) {
  mapply(function(string, pattern) grepl(pattern, string),
    string = string, pattern = pattern)
  }


# ----- function spec ----

test_that("function names are correct", {
  # - test that 'post'/'put'/'patch' are used and not 'create' and 'partial_update'
  # - test that function names are singularized or not
  # - test that '-' is removed
  # - test that 'get'->'list'
  # - test that functions from io and models are correct

  # examples in list
  ex_names <- c("admin_post_dump_memory", "admin_list_announcements",
                "admin_list_organizations", "admin_delete_announcements",
                "bocce_clusters_list_active_jobs", "channels_list",
                "jobs_list_children", "admin_patch_announcements",
                "scripts_post_sql", "notebooks_put",
                'aliases_get_object_type', 'databases_list_tables_search')
  test_names <- mapply(build_function_name, verb_name = ex_verb_names,
                       path_name = ex_path_names)
  expect_equivalent(ex_names, test_names)

  pkg_names <- lsf.str("package:civis")

  # test on examples from io.R, models.R, etc
  sample_names <- c("scripts_post_sql_runs", "tables_list_projects", "notebooks_get", "workflows_get")
  expect_true(all(sample_names %in% pkg_names))
})

test_that("function_args are correct", {
  # - test that args are in correct order
  # - test that optional args have correct string default (currently NULL)
  # - test that args are present
  # - test that args are in snake_case

  ignore_str <- 'NULL'
  ex_arg_str <-
    list(" <- function(host_name = %s) {\n\n",
         " <- function(limit = %s, page_num = %s, order = %s, order_dir = %s) {\n\n",
         " <- function(name, sql, remote_host_id, credential_id, parent_id = %s, user_context = %s, params = %s, arguments = %s, schedule = %s, notifications = %s, next_run_at = %s, time_zone = %s, hidden = %s, target_project_id = %s, csv_settings = %s) {\n\n")
  ex_arg_str <- lapply(ex_arg_str, gsub, pattern = "%s", replacement = ignore_str)
  test_arg_str <- lapply(ex_params[c(1:2, 9)], build_function_args)
  expect_equal(ex_arg_str, test_arg_str)
})


# ----- function body -----
test_that("build_function_body", {
  # - test that path name is correct
  # - test that args are in the form camelCase = snake_case
  # - test api call exactly

  body_lines <- strsplit(build_function_body(ex_ref[[1]], ex_verb_names[[1]],
                                             ex_path_names[[1]], ex_params[[1]]), "\n")[[1]]
  path <- grep("path <- ", body_lines, value = TRUE)
  expect_match(path, ex_path_names[[1]])
  expect_match(grep("body_params", body_lines, value = TRUE)[1],
                                  "hostName = host_name")
  check_call <- paste0("  resp <- call_api(\"", toupper(ex_verb_names[[1]]), "\", ",
                       "path, path_params, query_params, body_params)")
  expect_equal(grep("resp <- ", body_lines, value = TRUE), check_call)
})

# ----- function docs -----

test_that("build_docs", {
  test_doc <- build_docs(ex_ref[[1]])
  expect_match(test_doc, "\\item\\{hostName\\}")
  expect_match(test_doc, "@param host_name")
  expect_match(test_doc, ex_ref[[1]]$summary)
  expect_match(test_doc, "@export")

  test_esc <- build_docs(replace_ref(spec$paths[['/media/dmas']]$get, spec))
  expect_match(test_esc, '\\%"')

  expect_match(build_docs(ex_ref[[4]]),
               "@return  An empty HTTP response\n#' @export\n")

})

test_that("build_params_docs", {

  # single object
  arg_str <- build_params_docs(ex_ref[[1]])
  ex_arg_str <- paste0("@param ",
    camel_to_snake(names(ex_ref[[1]]$parameters[[1]]$schema$properties)))
  expect_match(arg_str, ex_arg_str)
  expect_match(arg_str, "optional")
  expect_match(arg_str,
    ex_ref[[1]]$parameters[[1]]$schema$properties$hostName$description)

  # list of parameters
  arg_str <- strsplit(build_params_docs(ex_ref[[2]]), "\n")[[1]]
  param_lines <- grep("@param", arg_str, value = TRUE)
  check_names <- lapply(ex_ref[[2]]$parameters, function(x) camel_to_snake(x$name))
  check_descr <- lapply(ex_ref[[2]]$parameters, function(x) x$description)
  expect_true(all(str_detect_multiple(param_lines, check_names)))
  # 4 doesn't play well with regex.
  expect_true(all(str_detect_multiple(param_lines[1:3], check_descr[1:3])))

  # single param
  arg_str <- strsplit(build_params_docs(ex_ref[[4]]), "\n")[[1]][1]
  expect_match(arg_str, ex_ref[[4]]$parameters[[1]]$name)
  expect_match(arg_str, ex_ref[[4]]$parameters[[1]]$description)

  # nested object
  ex_param <- ex_ref[[9]]$parameters[[1]]

  #   level 1 arg names and order
  arg_str <- strsplit(build_params_docs(ex_ref[[9]]), "\n")[[1]]
  check <- names(ex_param$schema$properties)
  req <- unlist(ex_param$schema$required)
  check <- c(check[check %in% req], check[!(check %in% req)])
  check_ <- sapply(check, camel_to_snake)
  param_lines <- arg_str[grep("@param", arg_str)]
  expect_true(all(str_detect_multiple(param_lines, check_)))

  #   required/optional strings
  expect_true(all(str_detect_multiple(param_lines[check %in% req],
                                      rep("required", length(req)))))
  expect_true(all(str_detect_multiple(param_lines[!(check %in% req)],
                                      rep("optional", length(check) - length(req)))))

  #   all doc names in fun names: str_match is kind of magical here
  fun_arg_str <- build_function_args(ex_params[[9]])

  param_names <- gsub("@param ", "",
                      unlist(regmatches(param_lines,
                                        gregexpr("@param ([a-z_])*", param_lines))))
  check <- lapply(param_names, function(x) expect_match(fun_arg_str, x))

  #   array args
  expect_match(arg_str[7], "An array containing")
  check <- names(ex_param$schema$properties$params$items$properties)
  expect_true(all(str_detect_multiple(arg_str[9:15], check)))

  #   obj args
  expect_match(arg_str[18], "list optional. A list containing")
  check <-  names(ex_param$schema$properties$schedule$properties)
  expect_true(all(str_detect_multiple(arg_str[20:24], check)))

  # no args
  expect_equal(build_params_docs(ex_ref[[6]]), "")

})

test_that("build_response_docs", {
  resp_str <- strsplit(build_resp_docs(ex_ref[[9]]), "\n")[[1]]
  ex_resp <- ex_ref[[9]]$responses[[1]]

  expect_match(resp_str[1], "@return")

  # level 1 names: case and order
  check <- names(ex_resp$schema$properties)
  expect_true(all(str_detect_multiple(grep("\\item\\{", resp_str, value = T), check)))

  # array
  param_id <- grep("item\\{params\\}", resp_str)
  expect_match(resp_str[param_id], "An array containing")
  check <- names(ex_resp$schema$properties$params$items$properties)
  param_args <- (param_id + 2):(param_id + 1 + length(check))
  expect_true(all(str_detect_multiple(resp_str[param_args], check)))

  # obj
  expect_match(resp_str[7], "A list containing")
  check <- names(ex_resp$schema$properties$author$properties)
  expect_true(all(str_detect_multiple(resp_str[9:13], check)))

  # empty, array, object
  expect_match(build_resp_docs(ex_ref[[4]]), "@return  An empty HTTP response\n")
})

test_that("write_properties", {
  arg_prop <- get_obj_properties(ex_ref[[9]]$parameters[[1]])
  resp_prop <- get_obj_properties(ex_ref[[9]]$responses[[1]])

  arg_prop_str <- strsplit(
    write_properties(arg_prop, req_str = "required",
                     transform_name = camel_to_snake), "\n")[[1]]
  resp_prop_str <- strsplit(
    write_properties(resp_prop, fmt = "#' \\item{%s}{%s, %s%s}"), "\n")[[1]]

  # level 1 names, fmt, transform_name
  check <- sapply(names(arg_prop), camel_to_snake)
  expect_true(all(str_detect_multiple(grep("@param", arg_prop_str, value = TRUE), check)))

  check <- names(resp_prop)
  expect_true(all(
    str_detect_multiple(grep("\\item\\{", resp_prop_str, value = TRUE), check)))

  # req_str: write_properties doesn't get correct req_str, so just make sure it can be provided
  param_lines <- grep("@param", arg_prop_str, value = TRUE)
  expect_true(all(str_detect_multiple(param_lines, rep("req", length(param_lines)))))

  # test non-nested prop is not an array or list
  expect_true(!grepl("(array|list)", arg_prop_str[1]))

  # array
  expect_match(arg_prop_str[4], "An array containing")
  check_names <- names(arg_prop$params$items$properties)
  expect_true(all(str_detect_multiple(arg_prop_str[6:12], check_names)))

  check_descr <- lapply(arg_prop$params$items$properties, function(x) x$description)
  expect_true(all(str_detect_multiple(arg_prop_str[6:12], check_descr)))

  # obj
  expect_match(resp_prop_str[6], "A list containing")
  check_names <- names(resp_prop$author$properties)
  expect_true(all(str_detect_multiple(resp_prop_str[8:12], check_names)))

  check_descr <- lapply(resp_prop$author$properties, function(x) x$description)
  expect_true(all(str_detect_multiple(resp_prop_str[8:12], check_descr)))

  # not nested
  arg_prop <- get_obj_properties(ex_ref[[1]]$parameters[[1]])
  arg_prop_str <- write_properties(arg_prop, transform_name = camel_to_snake)
  check_name <- paste0("@param ", camel_to_snake(names(arg_prop)))
  check_descr <- arg_prop$hostName$description
  expect_match(arg_prop_str, check_name)
  expect_match(arg_prop_str, check_descr)
})

test_that("write_nested_docs", {
  # nested array
  ex_prop <- get_array_properties(ex_ref[[6]]$responses[[1]])
  ex_array <- ex_prop[[which(sapply(ex_prop, is_array))]]
  array_str <- strsplit(write_nested_docs(ex_array), "\n")[[1]]
  expect_match(array_str[1], "An array containing")

  item_lines <- grep("\\item ", array_str, value = TRUE)
  check_name <- names(ex_array$items$properties)
  check_descr <- ex_array$items$properties$name$description
  expect_match(item_lines, check_name)
  expect_match(item_lines, check_descr)

  # nested obj
  ex_prop <- get_obj_properties(ex_ref[[9]]$responses[[1]])
  ex_obj <- ex_prop[[which(sapply(ex_prop, is_obj))[2]]]
  obj_str <- strsplit(write_nested_docs(ex_obj), "\n")[[1]]
  expect_match(obj_str[1], "A list containing")

  item_lines <- grep("\\item ", obj_str, value = TRUE)
  check_names <- names(ex_obj$properties)
  check_descr <- lapply(ex_obj$properties, function(x) x$description)
  expect_true(all(str_detect_multiple(item_lines, check_names)))
  expect_true(all(str_detect_multiple(item_lines, check_descr)))
})

# ----- parse_params -----
test_that("parse_params", {
  # object
  param <- ex_ref[[1]]$parameters[[1]]
  check <- data.frame(name = names(param$schema$properties),
                      http_location = param$`in`,
                      required = param$required,
                      description = param$schema$properties$hostName$description,
                      type = param$schema$properties$hostName$type,
                      stringsAsFactors = FALSE)
  expect_equal(ex_params[[1]], check)

  # list
  param <- ex_ref[[2]]$parameters
  check <- do.call(rbind,
    lapply(ex_ref[[2]]$parameters, as.data.frame, stringsAsFactors = FALSE))
  colnames(check) <- c("name", "http_location", "required", "description", "type")
  expect_equivalent(ex_params[[2]], check) # ignore attributes

  # length 1
  param <- ex_ref[[4]]$parameters[[1]]
  check <- data.frame(param, stringsAsFactors = FALSE)
  colnames(check) <- c("name", "http_location", "required", "description", "type")
  expect_equivalent(ex_params[[4]], check)

  # empty
  param <- data.frame(ex_ref[[6]]$parameters, stringsAsFactors = FALSE)
  expect_equal(ex_params[[6]], param)

  # nested object (just get level 1)
  param <- ex_ref[[9]]$parameters[[1]]
  prop <- param$schema$properties
  name <- names(prop)
  http_location <- rep(param$`in`, length(prop))
  required <- name %in% param$schema$required
  desc <- unlist(lapply(prop, function(p) get_descr_str(p)))
  type <- unlist(lapply(prop, function(p) p$type))
  check <- data.frame(name = name,
                      http_location = http_location,
                      required = required,
                      description = unlist(desc),
                      type = type, stringsAsFactors = FALSE)
  check <- check[order(check$required, decreasing = TRUE), ]
  expect_equivalent(ex_params[[9]], check)
})

# ----- references -----
test_that("is_ref", {
  expect_true(all(sapply(ex[c(1, 5, 8)],
                         function(x) is_ref(x$responses[[1]]$schema))))
  expect_true(all(sapply(ex[c(2:3, 6)],
                         function(x) is_ref(x$responses[[1]]$schema$items))))
  expect_true(!is_ref(ex[[4]]$responses[[1]]))
})

test_that("get_ref", {
  expect_equal(spec$definitions$Object1,
               get_ref(ex[[1]]$responses[[1]]$schema, spec))
})

test_that("replace_ref", {
  # --- works for parameters and responses
  ex_ref <- lapply(ex, replace_ref, spec = spec)
  for (i in 1:9) expect_true(!any(grepl("#/definitions", ex_ref[[i]])))
})

# ----- utils
test_that("is_nested", {
  expect_true(is_nested(ex_ref[[9]]$parameters[[1]]))
  expect_true(is_nested(ex_ref[[9]]$responses[[1]]))
  expect_true(is_nested(ex_ref[[9]]$parameters[[1]]$schema$properties$params))
  expect_true(is_nested(ex_ref[[9]]$responses[[1]]$schema$properties$author))
  expect_false(is_nested(ex_ref[[9]]$parameters[[1]]$schema$properties$name))
  expect_false(is_nested(ex_ref[[2]]$parameters[[1]]))
})

test_that("is_array", {
  expect_true(is_array(ex_ref[[9]]$parameters[[1]]$schema$properties$params))
  expect_false(is_array(ex_ref[[9]]$parameters[[1]]$schema$properties$schedule))
  expect_false(is_array(ex_ref[[9]]$parameters[[1]]$schema$properties$name))
})

test_that("is_obj", {
  expect_true(is_obj(ex_ref[[9]]$parameters[[1]]$schema$properties$schedule))
  expect_false(is_obj(ex_ref[[9]]$parameters[[1]]$schema$properties$params))
  expect_false(is_obj(ex_ref[[9]]$parameters[[1]]$schema$properties$name))
})

test_that("get_descr_str", {
  expect_equal(get_descr_str(ex_ref[[1]]$parameters[[1]]$schema$properties$hostName),
               ex_ref[[1]]$parameters[[1]]$schema$properties$hostName$description)
  expect_equal(get_descr_str(ex_ref[[1]]$parameters[[1]]), "")
})

test_that("get_req_str", {
  expect_equal(get_req_str(ex_ref[[1]]$parameters[[1]]), "optional")
  expect_equal(get_req_str(ex_ref[[6]]$parameters), "")
})

test_that("returns_array", {
  expect_false(
    returns_array(ex_ref[[1]], verb_name = ex_verb_names[[1]],
                  path_name = ex_path_names[[1]]))
  expect_true(
    returns_array(ex_ref[[2]], verb_name = ex_verb_names[[2]],
                  path_name = ex_path_names[[2]]))
})

test_that("escape_percent", {
  expect_equal(escape_percent("%"), "\\%")
})

test_that("camel_to_snake", {
  expect_equal(camel_to_snake("TinaEatYourFood"), "tina_eat_your_food")
})

