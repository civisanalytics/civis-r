#' @include dbi-connection.R
NULL

CivisResult <- function(connection, statement) {
  res <- new(
    "CivisResult",
    connection = connection,
    statement = statement,
    .envir = new.env(parent = emptyenv())
  )

  res@.envir$open <- TRUE

  res
}

#' @rdname DBI
#' @export
setClass("CivisResult",
  contains = "DBIResult",
  slots = list(
    connection = "CivisConnection",
    statement = "character",
    .envir = "environment"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "CivisResult",
  function(object) {
    cat("<CivisResult>\n")
    cat("Query:", object@statement, "\n")
    cat("Read Only:", object@connection@read_only, "\n")
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbClearResult
#' @export
setMethod(
  "dbClearResult", "CivisResult",
  function(res, ...) {
    if (!dbIsValid(res)) {
      warning("Result already closed.", call. = FALSE)
    }

    res@.envir$open <- FALSE
    set_result(res@connection, NULL)

    invisible(TRUE)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbFetch
#' @export
setMethod(
  "dbFetch", "CivisResult",
  function(res, n = -1, ...) {
    assert_result_valid(res)

    if (res@connection@read_only) {
      res@statement <- declare_statement_read_only(res@statement)
    }
    read_civis(sql(res@statement), database = res@connection@database)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbHasCompleted
#' @export
setMethod(
  "dbHasCompleted", "CivisResult",
  function(res, ...) {
    assert_connection_valid(res)

    TRUE
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "CivisResult",
  function(dbObj, ...) {
    getMethod("dbGetInfo", "DBIResult", asNamespace("DBI"))(dbObj, ...)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "CivisResult",
  function(dbObj, ...) {
    dbObj@.envir$open
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbGetStatement
#' @export
setMethod(
  "dbGetStatement", "CivisResult",
  function(res, ...) {
    assert_result_valid(res)
    res@statement
  }
)

assert_result_valid <- function(res) {
  if (!dbIsValid(res)) {
    stop("Result has been already cleared.", call. = FALSE)
  }
}
