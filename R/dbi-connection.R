#' @include dbi-driver.R
NULL

CivisConnection <- function(database, read_only = FALSE, ...) {
  ret <- new("CivisConnection",
             database = database,
             read_only = read_only,
             .envir = new.env(parent = emptyenv()))

  get_database_id(database)
  ret@.envir$valid <- TRUE

  # Needed for the DBItest named `cannot_forget_disconnect` which expects a
  # warning when a connection object is GCed w/o being closed first.
  reg.finalizer(ret@.envir, function(e) {
    if (e$valid) {
      warning("Connection has not been closed.")
    }
  })

  ret
}

#' @rdname DBI
#' @export
setClass("CivisConnection",
  contains = "DBIConnection",
  slots = list(
    database = "character",
    read_only = "logical",
    .envir = "environment"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "CivisConnection",
  function(object) {
    cat("<CivisConnection>\n")
    cat("Database:", object@database, "\n")
    cat("Read Only:", object@read_only, "\n")
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "CivisConnection",
  function(dbObj, ...) {
    dbObj@.envir$valid
  }
)

#' Is this DBMS object in read-only mode
#'
#' This generic tests whether a database object is in read-only mode
#'
#' @name dbIsReadOnly
#' @inheritParams DBI::dbIsValid
#' @export
setGeneric("dbIsReadOnly",
  def = function(dbObj, ...) standardGeneric("dbIsReadOnly"),
  valueClass = "logical")

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsReadOnly", "CivisConnection",
  function(dbObj, ...) {
    dbObj@read_only
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbDisconnect
#' @export
setMethod(
  "dbDisconnect", "CivisConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      warning("Connection already closed.", call. = FALSE)
    }
    unset_result(conn)
    conn@.envir$valid <- FALSE

    invisible(TRUE)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
#' @export
setMethod(
  "dbSendQuery", c("CivisConnection", "character"),
  function(conn, statement, ...) {
    assert_connection_valid(conn)
    unset_result(conn)

    if (conn@read_only) {
      statement <- declare_statement_read_only(statement)
    }

    res <- CivisResult(connection = conn, statement = statement)
    set_result(conn, res)
    res
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbSendStatement
#' @export
setMethod(
  "dbSendStatement", c("CivisConnection", "character"),
  function(conn, statement, ...) {
    if (conn@read_only) {
      statement <- declare_statement_read_only(statement)
    }
    # TODO: call query_civis instead of read_civis
    CivisResult(connection = conn, statement = statement)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "CivisConnection",
  function(dbObj, obj, ...) {
    tryCatch(
      getMethod("dbDataType", "DBIObject", asNamespace("DBI"))(dbObj, obj, ...),
      error = function(e) testthat::skip("Not yet implemented: dbDataType(Connection)"))
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbQuoteString
#' @export
setMethod(
  "dbQuoteString", c("CivisConnection", "character"),
  function(conn, x, ...) {
    getMethod("dbQuoteString", c("DBIConnection", "character"), asNamespace("DBI"))(conn, x, ...)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbQuoteIdentifier
#' @export
setMethod(
  "dbQuoteIdentifier", c("CivisConnection", "character"),
  function(conn, x, ...) {
    # Optional
    getMethod("dbQuoteIdentifier", c("DBIConnection", "character"), asNamespace("DBI"))(conn, x, ...)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbWriteTable
#' @param overwrite Allow overwriting the destination table. Cannot be
#'   `TRUE` if `append` is also `TRUE`.
#' @param append Allow appending to the destination table. Cannot be
#'   `TRUE` if `overwrite` is also `TRUE`.
#' @export
setMethod(
  "dbWriteTable", c("CivisConnection", "character", "data.frame"),
  function(conn, name, value, overwrite = FALSE, append = FALSE, ...) {
    if (conn@read_only) {
      stop("This database connection is read-only. You cannot write tables.")
    }

    if (overwrite) { if_exists <- "truncate" }
    if (!overwrite & append) { if_exists <- "append" }
    if (!overwrite & !append) { if_exists <- "fail" }

    write_civis(df = value, tablename = name, database = conn@database,
                if_exists = if_exists, ...)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbReadTable
#' @export
setMethod(
  "dbReadTable", c("CivisConnection", "character"),
  function(conn, name, ...) {
    read_civis(tablename = name, database = conn@database, ...)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbListTables
#' @export
setMethod(
  "dbListTables", "CivisConnection",
  function(conn, ...) {
    all_table_names(conn@database)
  }
)

all_table_names <- function(database) {
  db_id <- get_database_id(database)
  tbls <- fetch_all(tables_list, database_id = db_id)
  purrr::map_chr(tbls, "name")
}

#' @rdname DBI
#' @inheritParams DBI::dbExistsTable
#' @export
setMethod(
  "dbExistsTable", c("CivisConnection", "character"),
  function(conn, name, ...) {
    db_id <- get_database_id(conn@database)
    name_parts <- split_schema_name(name)
    tbls <- tables_list(database_id = db_id, schema = name_parts$schema,
                        name = name_parts$name)
    return(length(tbls) > 0)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbListFields
#' @export
setMethod(
  "dbListFields", c("CivisConnection", "character"),
  function(conn, name, ...) {
    tbl_id <- get_table_id(table_name = name, database = conn@database)
    tbl_columns <- tables_get(id = tbl_id)[["columns"]]
    return(purrr::map_chr(tbl_columns, "name"))
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbRemoveTable
#' @export
setMethod(
  "dbRemoveTable", c("CivisConnection", "character"),
  function(conn, name, ...) {
    if (conn@read_only) {
      stop("This database connection is read-only. You cannot remove tables.")
    }
    testthat::skip("Not yet implemented: dbRemoveTable(Connection, character)")
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "CivisConnection",
  function(dbObj, ...) {
    list(
      db.version = 1,
      dbname = dbObj@database,
      username = get_username(),
      host = NULL,
      port = NULL,
      civis_client_version = version()
    )
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbBegin
#' @export
setMethod(
  "dbBegin", "CivisConnection",
  function(conn, ...) {
    testthat::skip("Not implemented: dbBegin(Connection)")
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbCommit
#' @export
setMethod(
  "dbCommit", "CivisConnection",
  function(conn, ...) {
    testthat::skip("Not implemented: dbCommit(Connection)")
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbRollback
#' @export
setMethod(
  "dbRollback", "CivisConnection",
  function(conn, ...) {
    testthat::skip("Not implemented: dbRollback(Connection)")
  }
)

unset_result <- function(conn) {
  if (!is.null(conn@.envir$active_result)) {
    warning("Closing active result set.", call. = FALSE)
    dbClearResult(conn@.envir$active_result)
  }
}

set_result <- function(conn, res) {
  conn@.envir$active_result <- res
}

assert_connection_valid <- function(conn) {
  if (!dbIsValid(conn)) {
    stop("Connection has been already closed.", call. = FALSE)
  }
}

declare_statement_read_only <- function(statement) {
  if (stringr::str_sub(statement, start = -1L) != ";") {
    statement <- stringr::str_c(statement, ";")
  }
  stringr::str_c("BEGIN READ ONLY;", statement)
}
