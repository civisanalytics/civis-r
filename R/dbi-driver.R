#' DBI methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package.
#' The DBI connectors for Civis Platform are no longer maintained.
#'
#' @name DBI
#' @param database The name of a database on the Civis Platform.
NULL

#' Civis DBI driver
#'
#' This is a DBI-compliant interface for the Civis Platform.
#' The DBI connectors for Civis Platform are no longer maintained.
#'
#' @export
#' @import methods DBI
#' @examples \dontrun{
#' con <- DBI::dbConnect(dbi_driver(), database = "redshift-general")
#' rs <- DBI::dbSendQuery(con, "SELECT 1")
#' dbFetch(rs)
#' }
dbi_driver <- function() {
  new("CivisDriver")
}

#' @rdname DBI
#' @export
setClass("CivisDriver", contains = "DBIDriver")

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "CivisDriver",
  function(object) {
    cat("<CivisDriver>\n")
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbConnect
#' @export
setMethod(
  "dbConnect", "CivisDriver",
  function(drv, database, ...) {
    CivisConnection(database, ...)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "CivisDriver",
  function(dbObj, obj, ...) {
    # Optional: Can remove this if all data types conform to SQL-92
    tryCatch(
      getMethod("dbDataType", "DBIObject", asNamespace("DBI"))(dbObj, obj, ...),
      error = function(e) e)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "CivisDriver",
  function(dbObj, ...) {
    TRUE  # possibly check API key??
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "CivisDriver",
  function(dbObj, ...) {
    list(
      driver.version = version(),
      client.version = version()
    )
  }
)
