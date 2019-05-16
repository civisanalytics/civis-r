#' Query field names.
#'
#' This function is used to determine the fields/columns of a query.
#'
#' @keywords internal
#' @importFrom dplyr db_query_fields
#' @export
db_query_fields.CivisConnection <- function(con, sql, ...) {
  # dplyr does a where 0 = 1 here, but result sets with 0 rows are not
  # returned by platform
  requireNamespace('dbplyr', quietly = TRUE)
  fields <- dbplyr::build_sql("SELECT * FROM ",
                              dplyr::sql_subquery(con, sql),
                              " LIMIT 1", con = con)
  names(read_civis(sql(fields), database = con@database))
}

#' Escape SQL identifiers.
#'
#' This function is used to properly escape SQL identifiers such as table
#' or column names.
#'
#' @keywords internal
#' @importFrom dplyr sql_escape_ident
#' @export
sql_escape_ident.CivisConnection <- function(con, x) {
  # TODO: will this always be a tablename?
  requireNamespace('dbplyr', quietly = TRUE)
  tryCatch({
    # the default impl doesn't seem to properly quote "schema"."table"
    parts <- split_schema_name(x)
    paste0(dbplyr::sql_quote(parts$schema, '"'), ".", dbplyr::sql_quote(parts$table, '"'))
  },
  error = function(e) {
    dbplyr::sql_quote(x, '"')
  })
}

#' Translate R functions to their SQL (Redshift flavor) equivalent.
#'
#' @keywords internal
#' @importFrom dplyr sql_translate_env
#' @export
sql_translate_env.CivisConnection <- function(x) {
  requireNamespace('dbplyr', quietly = TRUE)
  # https://github.com/tidyverse/dbplyr/blob/c8d60a634d07fa6972345407de459f76db64cf91/R/db-postgres.r
  dbplyr::sql_variant(
    dbplyr::sql_translator(.parent = dbplyr::base_scalar,
      log = function(x, base = exp(1)) {
        if (isTRUE(all.equal(base, exp(1)))) {
          dbplyr::build_sql("ln(", x, ")")
        } else {
         # Use log change-of-base because postgres doesn't support the
         # two-argument "log(base, x)" for floating point x.
         dbplyr::build_sql("log(", x, ") / log(", base, ")")
       }
      }
    ),
    dbplyr::sql_translator(.parent = dbplyr::base_agg,
      n = function() dbplyr::sql("count(*)"),
      cor = dbplyr::sql_prefix("corr"),
      cov = dbplyr::sql_prefix("covar_samp"),
      sd = dbplyr::sql_prefix("stddev_samp"),
      var = dbplyr::sql_prefix("var_samp"),
      all = dbplyr::sql_prefix("bool_and"),
      any = dbplyr::sql_prefix("bool_or"),
      paste = function(x, collapse) dbplyr::build_sql("string_agg(", x, collapse, ")")
    ),
    dbplyr::base_win
  )
}
