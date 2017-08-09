#' Transfer a table from one location to another.
#'
#' @param source_db string or int, The name of the database where the source table is located. 
#' Optionally, could be the database ID.
#' @param dest_db string or int, The name of the database where the table will be transfered.
#' Optionally, could be the database ID.
#' @param source_table string, Full name of the table to transfer,
#' e.g., \code{"schema.table"}.
#' @param dest_table string, Full name of the table in the destination database,
#' e.g., \code{"schema.table"}.
#' @param job_name string, optional, A name to give the job. 
#' If omitted, a random job name will be used. 
#' @param source_credential_id string or int, Optional credential ID 
#' for the source database. If \code{NULL}, the default credential will be used.
#' @param dest_credential_id string or int, Optional credential ID 
#' for the source database. If \code{NULL}, the default credential will be used.
#' @param interval Number of seconds to wait between checks for job completion. 
#' If \code{NULL}, the default exponential backoff from \code{await} will be used.
#' @param verbose bool, Set to TRUE to print intermediate progress indicators.
#' @return A \code{civis_api} object.
#' @param advanced_options A list of advanced options for the sync. See \code{\link{imports_post_syncs}} for
#' details.
#' @examples 
#' \dontrun{
#' transfer_table(source_db='Cluster A', dest_db='Cluster B',
#'                source_table='schma.tbl', dest_table='schma.tbl')
#' }
#' @export
#' @family tables
transfer_table <- function(source_db, dest_db, source_table, dest_table,
                           job_name = NULL, 
                           source_credential_id = NULL, dest_credential_id = NULL,
                           interval = NULL, verbose = FALSE, advanced_options = NULL) {
  
  if (is.null(source_credential_id)) source_credential_id <- default_credential()
  if (is.null(dest_credential_id)) dest_credential_id <- default_credential()
  if (is.null(job_name)) job_name <- "R Client transfer table"
  
  source <- list("remoteHostId" = get_database_id(source_db),
                 "credentialId" = source_credential_id)
  destination <- list("remoteHostId" = get_database_id(dest_db), 
                      "credentialId" = dest_credential_id)
  id <- imports_post(job_name, sync_type = "Dbsync", is_outbound = TRUE,
                     source = source, destination = destination)$id
  args <- list(id = id, source = list(path = source_table), 
               destination = list(path = dest_table))
  if (!is.null(advanced_options)) {
    args <- c(args, list(advanced_options = advanced_options))
  }
  
  do.call(imports_post_syncs, args)
  run_id <- imports_post_runs(id = id)$runId
  await(imports_get_files_runs, id = id, run_id = run_id, 
             .interval = interval, .verbose = verbose)
}

#' Refresh a table
#' @description Refreshes a table on Redshift using \code{\link{tables_post_refresh}}, which runs the 
#' table scanner and updates table meta-data.
#' @param tablename string, Name of table and schema \code{"schema.tablename"}
#' @param database string, Name of database where data frame is to be uploaded. If no database is specified,
#' uses \code{options(civis.default_db)}
#' @param verbose bool, Set to TRUE to print intermediate progress indicators.
#' @return Returns table meta-data from \code{\link{tables_get}}.
#' @family tables
#' @export
refresh_table <- function(tablename, database = NULL, verbose = FALSE) {
  db <- get_db(database)
  table_id <- get_table_id(tablename, db)
  tables_post_refresh(table_id)
  await(tables_get, id = table_id, .status_key = "refreshStatus", 
        .success_states = c("current"), .error_states = c("stale"), .verbose = verbose)
}

#' Return the table id for a given table
#'
#' @param table_name string, Name of table and schema \code{"schema.tablename"}
#' @param database string, Name of database where data frame is to be uploaded. If no database is specified,
#' uses \code{options(civis.default_db)}
#' @return table_id integer, id of the matching table.
#' @export
#' @family tables
get_table_id <- function(table_name, database = NULL) {
  parts <- split_schema_name(table_name)
  database_name <- get_db(database)
  db_id <- get_database_id(database_name)
  table_list <- tables_list(database_id = db_id, schema = parts$schema,
                            name = parts$table)
  tbl <- first(table_list)
  if ((length(tbl) > 0) && tbl$name == parts$table) {
    return(tbl$id)
  }
  msg <- paste0("Table ", table_name, " not found.")
  stop(msg)
}
