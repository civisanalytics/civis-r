#' Return the database id for a given database name
#'
#' @param database_name string, Name of database.
#' @return database_id integer, id of the matching database.
#' @rdname get_database_id
#' @family id
#' @export
get_database_id <- function(database_name) {
  dbs <- databases_list()
  for (db in dbs) {
    if (identical(db$name, database_name)) {
      return(db$id)
    }
  }

  # We didn't find one, make a helpful suggestion in case it was a typo.
  nms <- sapply(dbs, function(x) x[["name"]])
  aprx <- nms[which.min(adist(database_name, nms, partial = TRUE, ignore.case = TRUE))]
  stop(paste("Database", database_name, "not found. Did you mean",
             paste0(aprx, "?")))
}

#' @import memoise memoise
get_database_id <- memoise::memoise(get_database_id)

# Split schema.table into a list of schema and table.
# TODO: verify `.` is not valid in a table or schema identifier
split_schema_name <- function(table_name) {
  parts <- strsplit(table_name, ".", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
      stop("table name must be 'schema.table'")
  }
  list(schema = parts[[1]], table = parts[[2]])
}

get_db <- function(database) {
  db <- if (is.null(database)) get_default_database() else database
  if (is.null(db)) {
    msg <- c("Argument database is NULL and options(\"civis.default_db\") not set. Set this option using options(civis.default_db = \"my_database\")")
    stop(msg)
  }
  db
}

# Returns options(civis.default_db).
# If not set and databases_list is length 1, sets the package option.
get_default_database <- function() {
  db <- getOption("civis.default_db")
  if (is.null(db)) {
    db_list <- databases_list()
    if (length(db_list) == 1) {
      db <- db_list[[1]]$name
      options(civis.default_db = db)
    }
  }
  db
}

get_content_type <- function(x){
  x$headers[["Content-Type"]] %||% "application/octet-stream"
}

#' The current user's default credential.
#'
#' @return credential_id, id of the default credential for the current user.
#' @export
default_credential <- function() {
  cred <- first(credentials_list(default = TRUE))
  if (is.null(cred)) {
    stop(paste("No default credential found for", get_username()))
  }
  cred[['id']]
}

#' @importFrom dplyr sql
#' @export
dplyr::sql

get_username <- function() {
  users_list_me()[['username']]
}
get_username <- memoise::memoise(get_username)

match_params <- function(params, args) {
  unlist(lapply(names(params), camel_to_snake)) %in% names(args)
}

first <- function(lst) {
  if (is.empty(lst)) {
    NULL
  } else {
    lst[[1]]
  }
}

is.empty <- function(lst) {
  length(lst) == 0
}

is.civis_error <- function(x) {
  inherits(x, "civis_error")
}

camel_to_snake <- function(s) {
  first_cap <- '(.)([A-Z][a-z]+)'
  all_cap <- '([a-z0-9])([A-Z])'

  s <- stringr::str_replace_all(s, first_cap, "\\1_\\2")
  s <- stringr::str_replace_all(s, all_cap, "\\1_\\2")
  stringr::str_to_lower(s)
}

snake_to_camel <- function(s) {
  parts <- stringr::str_split(s, "_", simplify = TRUE)
  paste(c(parts[1], stringr::str_to_title(parts[-1])), collapse = "", sep = "")
}

escape_percent <- function(x) {
  gsub("%", "\\\\%", x)
}

`%||%` <- function(x, default_val) {
  if (is.null(x)) return(default_val)
  x
}

toggle_dev <- function() {
  flag <- Sys.getenv("R_CLIENT_DEV")
  if (flag == "TRUE") {
    cat("in prod environment, regenerates client")
    Sys.unsetenv("R_CLIENT_DEV")
  } else {
    cat("in dev environment, does not generate client")
    Sys.setenv(R_CLIENT_DEV = "TRUE")
  }
}
