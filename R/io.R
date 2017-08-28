#' Read a table or file from the Civis Platform as a data frame
#'
#' @description \code{read_civis} loads a table from Redshift as a data frame if
#' given a \code{"schema.table"} or \code{sql("query")} as the first argument, or
#' loads a file from Amazon S3 (the files endpoint) if a file id is given.
#'
#' A default database can be set using \code{options(civis.default_db = "my_database")}.
#' If there is only one database available,
#' this database will automatically be used as the default.
#'
#' @param x  \code{"schema.table"}, \code{sql("query")}, or a file id.
#' @param using function, Function to convert the file to a data frame or to unserialize.
#'  the file (e.g. \code{read.csv} or \code{readRDS}).
#' @param database string, Name of database where data frame is to be uploaded.
#' If no database is specified, uses \code{options(civis.default_db)}.
#' @param job_name string, Name of the job (default: \code{"Civis Export Via R Client"}).
#' @param hidden bool, Whether the job is hidden.
#' @param verbose bool, Set to TRUE to print intermediate progress indicators.
#' @param ... arguments passed to \code{using}.
#' @details For \code{read_civis.sql}, queries must be \code{READ ONLY}.
#' To execute arbitrary queries, use \code{\link{query_civis}}.
#' @examples
#' \dontrun{
#' # Read all columns in a single table
#' df <- read_civis("schema.my_table", database = "my_database")
#'
#' # Read data from a SQL select statement (READ ONLY)
#' query <- sql("SELECT * FROM table JOIN other_table USING id WHERE var1 < 23")
#' df <- read_civis(query, database = "my_database")
#'
#' # Read an R object from the files endpoint.
#' id <- write_civis_file(df)
#' df <- read_civis(id)
#'
#' # Read a text file or csv from the files endpoint.
#' id <- write_civis_file("my_csv.csv")
#' df <- read_civis(id, using = read.csv)
#'
#' # Gracefully handle when read_civis.sql returns no rows
#' query <- sql("SELECT * FROM table WHERE 1 = 2")
#' mean_x <- tryCatch({
#'   df <- read_civis(query, database = "my_database")
#'   mean(df$x)
#' }, empty_result_error = function(e) {
#'    NA
#' })
#' }
#' @export
#' @family io
read_civis <- function(x, ...) {
  UseMethod("read_civis")
}

#' @describeIn read_civis Return a file as a data frame
#' @details
#' By default, \code{read_civis.numeric} assumes the file has been serialized using
#' \code{saveRDS}, as in \code{write_civis_file} and uses \code{using = readRDS} by default. For reading
#' an uncompressed text or csv from the files endpoint, set \code{using = read.csv} for example.
#' @export
read_civis.numeric <- function(x, using = readRDS, verbose = FALSE, ...) {
  url <- files_get(x)$fileUrl
  stopifnot(is.function(using))
  resp <- httr::GET(url)
  stop_for_status(resp, task = "download file from S3")
  serialized <- get_content_type(resp) == "application/octet-stream"

  if (serialized) {
    tryCatch({
      raw <- httr::content(resp, as = "raw")
      con <- gzcon(rawConnection(raw))
      res <- using(con, ...)
    }, finally = {
      if (exists("con")) close(con)
    })
  } else {
    if (identical(using, readRDS)) {
      msg <- "Content does not appear to be serialized. Maybe using = read.csv?"
      stop(msg)
    }
    tryCatch({
      parsed <- httr::content(resp, as = "text", encoding = "UTF-8")
      con <- textConnection(parsed)
      res <- using(con, ...)
    }, finally = {
      if (exists("con")) close(con)
    })
  }
  return(res)
}

#' @export
#' @describeIn read_civis Return all columns from a table as a data frame.
read_civis.character <- function(x, database = NULL, ...) {
  if (stringr::str_detect(tolower(x), "\\bselect\\b")) {
    msg <- c("Argument x should be \"schema.tablename\". Did you mean x = sql(\"...\")?")
    stop(msg)
  }
  sql_str <- sql(paste0("SELECT * FROM ", x))
  read_civis.sql(sql_str, database = database, ...)
}


#' @describeIn read_civis  Return a SQL query as a data frame.
#' @export
read_civis.sql <- function(x, database = NULL, using = utils::read.csv, job_name = NULL,
                           hidden = TRUE, verbose = FALSE, ...) {
  db <- get_db(database)
  sql_str <- declare_statement_read_only(x)
  job_name <- if (is.null(job_name)) "Civis Export Via R Client"
  if (!is.character(job_name)) stop("job_name must be a string.")
  run <- start_scripted_sql_job(db, sql = sql_str, job_name, hidden = hidden)

  r <- await(scripts_get_sql_runs, id = run$script_id, run_id = run$run_id, .verbose = verbose)
  if (get_status(r) == "succeeded") {
    resp <- download_script_results(run$script_id, run$run_id)
    parsed <- httr::content(resp, as = "text", encoding = "UTF-8")
    con <- textConnection(parsed)
    using(con, ...)
  }
}

#' Upload a local data frame or csv file to the Civis Platform (Redshift)
#'
#' @description Uploads either a data frame or csv file to Redshift, based
#' on the first argument.
#'
#' A default database can be set using \code{options(civis.default_db = "my_database")}.
#' If there is only one database available,
#' this database will automatically be used as the default.
#'
#' @param x data frame or file path of csv to upload to platform.
#' @param tablename string, Name of table and schema \code{"schema.tablename"}.
#' @param database string, Name of database where data frame is to be uploaded. If no database is specified,
#' uses \code{options(civis.default_db)}.
#' @param if_exists string, optional,  String indicating action to take if table already
#' exists.  Must be either "fail", "truncate" or "append". Defaults to "fail".
#' @param distkey string, optional, Column name designating the distkey.
#' @param sortkey1 string, optional, Column name designating the first sortkey.
#' @param sortkey2 string, optional, Column name designating the second
#' (compound) sortkey.
#' @param max_errors int, optional, Maximum number of rows with errors
#' to remove before failing.
#' @param verbose bool, Set to TRUE to print intermediate progress indicators.
#' @param ... arguments passed to \code{write.csv}.
#' @seealso \code{\link{refresh_table}} to update table meta-data.
#'
#' @examples
#' \dontrun{
#' df <- read.csv(local_file)
#'
#' # Create new table, fail if already exists
#' write_civis(df, "schema.my_table", "my_database")
#'
#' # Create new table, append if already exists
#' write_civis(df, "schema.my_table", "my_database", if_exists="append")
#'
#' # Create new table with defined diskey / sortkeys for speed!
#' write_civis(df, "schema.my_table", "my_database", distkey="id",
#'             sortkey1="added_date")
#'
#' # Create new table directly from a saved csv
#' write_civis("my/file/path.csv", "schema.my_table", "my_database")
#'
#' }
#' @export
#' @family io
#'
write_civis <- function(x, ...) {
  UseMethod("write_civis")
}

#' @describeIn write_civis Upload a data frame to Civis Platform (Redshift).
#' @export
write_civis.data.frame <- function(x, tablename, database = NULL, if_exists="fail",
                        distkey = NULL, sortkey1 = NULL, sortkey2 = NULL,
                        max_errors = NULL, verbose = FALSE, ...) {
  db <- get_db(database)
  tryCatch({
    filename <- tempfile(fileext = ".csv")
    utils::write.csv(x, filename, row.names = FALSE, na = "", ...)
    write_civis.character(filename, tablename, database = db, if_exists,
                 distkey, sortkey1, sortkey2, max_errors,
                 verbose)
  }, finally = {
    unlink(filename)
  })
}

#' @describeIn write_civis Upload a csv to Civis Platform (Redshift).
#' @export
write_civis.character <- function(x, tablename, database = NULL, if_exists = "fail",
                         distkey = NULL, sortkey1 = NULL, sortkey2 = NULL,
                         max_errors = NULL, verbose = FALSE, ...) {
    db <- get_db(database)
    stopifnot(file.exists(x))
    job_r <- start_import_job(db, tablename, if_exists, distkey,
                              sortkey1, sortkey2, max_errors)
    put_r <- httr::PUT(job_r[["uploadUri"]], body = httr::upload_file(x))
    if (put_r$status_code != 200) {
      msg <- httr::content(put_r)
      stop(msg)
    }

    job_id <- job_r[["id"]]
    run <- imports_post_files_runs(job_id)
    r <- await(imports_get_files_runs, id = job_id, run_id = run$id, .verbose = verbose)
    r
}

#' Upload a R object or file to Civis Platform (Files endpoint)
#'
#' @description Uploads a R object or file to the files endpoint on Civis
#' Platform (Amazon S3).
#' It returns the id of the file for use with \code{\link{read_civis}}.
#'
#' R objects are serialized with \code{\link{saveRDS}}, files are unserialized.
#' Files expire after 30 days by default.
#'
#' @param x R object or path of file to upload.
#' @param name string, Name of the file or object.
#' @param expires_at string, The date and time the object will expire on in the
#' format \code{"YYYY-MM-DD HH:MM:SS"}. The default is 30 days.
#' Setting \code{expires_at = NULL} will keep the file indefinitely.
#' @param ... arguments passed to \code{\link{saveRDS}}.
#'
#' @return The file id which can be used to later retrieve the file using
#' \code{\link{read_civis}}.
#'
#' @examples \dontrun{
#' data(iris)
#' file_id <- write_civis_file(iris)
#' read_civis(file_id)
#'
#' file_id <- write_civis_file("path/to/my.csv")
#' read_civis(file_id, using = read.csv)
#'
#' # Does not expire
#' file_id <- write_civis_file(iris, expires_at = NULL)
#'
#' # Expires on a given date and time
#' file_id <- write_civis_file(iris, expires_at = "2030-01-01")
#' file_id <- write_civis_file(iris, expires_at = "12:00:00")
#' file_id <- write_civis_file(iris, expires_at = "2030-01-01 12:00:00")
#' }
#' @details By default, R objects are serialized using \code{\link{saveRDS}} before uploading the object
#' to the files endpoint. If given a filepath, the file is uploaded as-is.
#' @family io
#' @export
write_civis_file <- function(x, ...) {
  UseMethod("write_civis_file")
}

#' @export
#' @describeIn write_civis_file Serialize R object to Civis Platform (Files endpoint).
write_civis_file.default <- function(x, name = 'r-object.rds', expires_at = NULL, ...) {
  args <- list()
  if (!missing(expires_at)) args <- list(expires_at = expires_at)

  with_tempfile(function(tmp_file, ...) {
    saveRDS(x, file = tmp_file, ...)
    args <- c(list(x = tmp_file, name = name), args)
    do.call(write_civis_file.character, args)
  })
}

#' @describeIn write_civis_file Upload a text file to Civis Platform (Files endpoint).
#' @export
write_civis_file.character <- function(x, name, expires_at = NULL, ...) {
  stopifnot(file.exists(x))
  args <- list(name = name)
  if (!missing(expires_at)) args <- c(args, list(expires_at = expires_at))
  u <- do.call(files_post, args)

  # uploadFields is the set of form parameters that we must pass to S3 in a post
  # request. Here we also add the file we're uploading to the body of the
  # request.
  uploadFields <- u$uploadFields
  uploadFields$file <- httr::upload_file(x)

  resp <- httr::POST(u$uploadUrl, body = uploadFields)
  stop_for_status(resp, task = "upload file to S3")

  return(u$id)
}

#' Download a table or a file from the Civis Platform to local disk
#'
#' @description
#'
#' \code{download_civis} downloads a file based on the type of its first
#' argument, which can be a string \code{"schema.table"},
#' a SQL query \code{sql(...)}, or a numeric file ID.
#'
#' A table or a query from Redshift will be downloaded onto disk as a CSV.
#' A file from Platform files endpoint will be downloaded as is.
#'
#' A default database can be set using \code{options(civis.default_db = "my_database")}.
#' If there is only one database available,
#' this database will automatically be used as the default.
#'
#' @param x  \code{"schema.table"}, \code{sql("query")}, or a file id.
#' @param database string, The database. If \code{NULL}, tries to use the default database.
#' @param file string, The file to write to.
#' @param overwrite logical, Whether to overwrite the existing \code{file}.
#' @param split logical, Whether to download a big table by splitting it into multiple
#' CSV parts first. See \code{\link{civis_to_multifile_csv}} for details.
#' @param progress logical, Whether to display a progress bar.
#' @param job_name string, Name of the job (default: \code{"Civis Download Via R Client"}).
#' @param hidden logical, Whether the job is hidden on Platform.
#' @param verbose logical, Whether to print detailed updates of job status.
#' @param ...  Currently ignored.
#' @return The file where the downloaded files or tables are written to.
#' It is returned invisibly.
#' @examples
#' \dontrun{
#' # Download all columns in a single table into a CSV
#' download_civis("schema.table", database = "my_database",
#'                file = "~/Downloads/my_table.csv")
#'
#' # Download data from a SQL select statement (READ ONLY) into a CSV
#' query <- sql("SELECT * FROM table JOIN other_table USING id WHERE var1 < 23")
#' download_civis(query, database = "my_database",
#'                file = "~/Downloads/my_table.csv")
#'
#' # Set a default database
#' options(civis.default_db = "my_database")
#'
#' # Download any file from the files endpoint.
#' file_id <- write_civis_file(df)
#' download_civis(file_id, file = "df.rds", progress = TRUE)
#' df2 <- readRDS("df.rds")
#' identical(df, df2)
#' }
#' @export
#' @family io
download_civis <- function(x, ...) {
  UseMethod("download_civis")
}

#' @describeIn download_civis Download a table from Redshift to disk as CSV.
#' @export
download_civis.character <- function(x, database = NULL, file,
                                     overwrite = FALSE, progress = FALSE, split = FALSE,
                                     job_name = NULL, hidden = TRUE, verbose = FALSE,
                                     ...) {
  if (stringr::str_detect(tolower(x), "\\bselect\\b")) {
    msg <- c("Argument x should be \"schema.table\". Did you mean x = sql(\"...\")?")
    stop(msg)
  }

  sql_str <- sql(paste0("SELECT * FROM ", x))
  download_civis.sql(sql_str, database = database, file = file,
                     overwrite = overwrite, progress = progress, split = split,
                     job_name = job_name, hidden = hidden, verbose = verbose)
}

#' @describeIn download_civis Download the result of a SQL query from Redshift to disk as CSV.
#' @export
download_civis.sql <- function(x, database = NULL, file,
                               overwrite = FALSE, progress = FALSE, split = FALSE,
                               job_name = NULL, hidden = TRUE, verbose = FALSE,
                               ...) {
  if (!overwrite & file.exists(file)) {
    stop("File already exists. To overwrite, set overwrite = TRUE.")
  }

  db <- get_db(database)
  sql_str <- declare_statement_read_only(x)
  job_name <- if (is.null(job_name)) "Civis Download Via R Client"
  if (!is.character(job_name)) stop("job_name must be a string.")

  if (!split) {
    run <- start_scripted_sql_job(database = db, sql = sql_str,
                                  job_name = job_name, hidden = hidden)
    await(scripts_get_sql_runs,
          id = run$script_id, run_id = run$run_id, .verbose = verbose)
    download_script_results(script_id = run$script_id,
                            run_id = run$run_id, filename = file,
                            progress = progress)
  } else {
    if (!requireNamespace("R.utils", quietly = TRUE)) {
      stop("Package R.utils is needed to unzip downloaded files. Please install it.",
           call. = FALSE)
    }
    manifest <- civis_to_multifile_csv(sql = sql_str, database = db,
                                       job_name = job_name, hidden = TRUE)

    # Download and unzipped CSV parts
    downloaded_file_paths <- lapply(manifest[["entries"]], function(entry) {
      local_file_name <- paste0(tempdir(), "/", entry$id, entry$name)
      download_civis(entry$id, file = local_file_name, progress = progress)
      local_file_name
    })
    unzipped_file_paths <- lapply(downloaded_file_paths, R.utils::gunzip)

    # Concatenate CSV parts into one file
    if (file.exists(file)) file.remove(file)
    concat_command <- ifelse(.Platform$OS.type == "unix", "cat", "type")
    system2(command = concat_command,
            args = c(paste(purrr::map_chr(unzipped_file_paths, 1), collapse = " "),
                     ">", file))
  }

  invisible(file)
}

#' @describeIn download_civis Download a file from Platform files endpoint to disk.
#' @export
download_civis.numeric <- function(x, file,
                                   overwrite = FALSE, progress = FALSE,
                                   ...) {
  if (!overwrite & file.exists(file)) {
    stop("File already exists. To overwrite, set overwrite = TRUE.")
  }

  args <- c(list(files_get(x)$fileUrl),
            list(httr::write_disk(file, overwrite = overwrite)))
  if (progress) args <- c(args, list(httr::progress()))
  resp <- do.call(httr::GET, args)

  stop_for_status(resp, task = "download file from S3")
  invisible(file)
}

#' Split a Redshift table into multiple CSV parts on S3
#'
#' Split a Redshift table into multiple CSV parts on S3 and return their
#' locations as file IDs and presigned S3 urls.
#'
#' When tables are large, unloading by splitting them first is faster. When we
#' split a table, each Redshift compute node can dump its data into
#' S3 in parallel with the others. By doing so, we avoid having
#' all compute nodes sending the data through the leader node, which is slow.
#'
#' This function returns a list that contains the location
#' of the CSV parts as file IDs and presigned S3 urls. The user can use either
#' the file IDs or the presigned S3 urls to download the CSV parts. The content
#' of the list returned by this function is similar to that of the manifest file
#' returned by Amazon S3 UNLOAD statements.
#'
#' @param sql string, The SQL select string to be executed.
#' @param database string, Name of database where query is run.
#' @param job_name string, optional. Name to identify scripted sql job.
#' @param hidden logical, Whether to hide the query in platform.
#' @param include_header logical, optional. Whether to include headers as an
#' element in the returned list.
#' @param compression string, optional, Type of compression to use, if any.
#' One of 'none', 'zip', or 'gzip'.
#' @param delimiter string, optional. Which delimiter to use. One of
#' ',', '\\t' or '|'.
#' @param unquoted logical, optional. Whether or not to quote fields.
#' @param prefix string, optional. A user specified filename prefix for
#' the output files to have.
#'
#' @return A list with the items:
#' \itemize{
#'   \item header: column headers if 'include_header' is TRUE
#'   \item query: the executed query
#'   \item entries: a list containing presigned urls for each csv part
#'   \item compression: the type of compression on each csv part
#'   \item delimiter: the delimiter used to seperate fields
#'   \item unquoted: whether fields are quoted
#' }
#'
#' @examples
#' \dontrun{
#' # Download a table into multiple csv parts
#' sql <- "SELECT * FROM schema.table"
#' database <- "important_database"
#' manifest <- civis_to_multifile_csv(sql=sql, database=database)
#' files <- lapply(manifest[["entries"]], function(x) {
#'   download_civis(x$id, x$name)
#'   x$name
#' })
#' }
#' @export
civis_to_multifile_csv <- function(sql, database, job_name = NULL, hidden = TRUE,
                                   include_header = TRUE, compression = 'gzip',
                                   delimiter = ',', unquoted = FALSE,
                                   prefix = NULL) {

  # When force_multifile = TRUE, Platform appends "LIMIT 1" to the query
  # If the user-submitted SQL query ends with ";"
  # the resulting query would be "; LIMIT 1", which is a SQL syntax error
  # Thus we make sure that the user-submitted query does not end with ";"
  if (stringr::str_sub(sql, start = -1L) == ";") {
    stringr::str_sub(sql, start = -1L) <- ""
  }

  column_delimiter <- delimiter_name_from_string(delimiter)
  job_name <- ifelse(is.null(job_name), "civis Export", job_name)
  filename_prefix <- if (is.null(prefix)) "" else prefix
  csv_settings = list(include_header = include_header,
                      compression = compression,
                      column_delimiter = column_delimiter,
                      unquoted = unquoted,
                      filename_prefix = filename_prefix,
                      force_multifile = TRUE)
  script_run_ids <- start_scripted_sql_job(database, sql, job_name,
                                           hidden = hidden,
                                           csv_settings = csv_settings)
  script_id <- script_run_ids[["script_id"]]
  run_id <- script_run_ids[["run_id"]]

  stat <- await(scripts_get_sql_runs, id = script_id, run_id = run_id)
  r <- switch(get_status(stat),
         "succeeded" = download_script_results(script_id, run_id),
         "failed" = stop(scripts_get_sql_runs(script_id, run_id)[["error"]]))
  httr::content(r, as = 'parsed', type = 'application/json')
}

#' Run a Query on Platform
#'
#' @description Utility to run queries that return no output.
#'
#' A default database can be set using \code{options(civis.default_db = "my_database")}.
#' If there is only one database available,
#' this database will automatically be used as the default.
#'
#' @param x \code{sql("...")}, \code{"query"}, or id of an existing sql script.
#' @param ... arguments passed to \code{queries_post}.
#'
#' @seealso \code{\link{read_civis}} for downloading results of SQL scripts from Civis Platform as a data frame.
#' @family io
#' @examples
#' \dontrun{
#' query_civis("GRANT ALL ON schema.my_table TO GROUP admin", "database")
#' }
#'
#' @export
query_civis <- function(x, ...) {
  UseMethod("query_civis")
}

#' @export
#' @param database string, Name of database where query is run.
#' @param verbose bool, Print detailed updates of job status.
#' @describeIn query_civis Run a SQL query.
query_civis.sql <- function(x, database = NULL, verbose = FALSE, ...) {
  sql_str <- as.character(x)
  query_civis.character(sql_str, database = database, verbose = verbose, ...)
}

#' @export
#' @describeIn query_civis Run a SQL query from a previous SQL query id.
query_civis.numeric <- function(x, verbose = FALSE, ...) {
  r <- queries_post_runs(x)
  await(queries_get_runs, id = r$queryId, run_id = r$id, .verbose = verbose)
}

#' @export
#' @describeIn query_civis Run a SQL query.
query_civis.character <- function(x, database = NULL, verbose = FALSE, ...) {
  db <- get_db(database)
  db_id <- get_database_id(db)
  cred_id <- default_credential()
  q_id <- queries_post(db_id, x, preview_rows = 0, credential = cred_id, ...)[["id"]]
  await(queries_get, id = q_id, .verbose = verbose)
}

# Kick off a scripted sql job
start_scripted_sql_job <- function(database, sql, job_name, hidden = TRUE,
                                   csv_settings = NULL) {

  db_id <- get_database_id(database)
  creds <- default_credential()
  args <- list(name = job_name, sql = sql, hidden = hidden, remote_host_id = db_id,
               credential_id = creds)
  if (!missing(csv_settings)) args <- c(args, list(csv_settings = csv_settings))

  script_id <- do.call(scripts_post_sql, args)[["id"]]
  # Kick off job
  run_id <- scripts_post_sql_runs(script_id)[["id"]]
  list(script_id = script_id, run_id = run_id)
}


# Kick off a job to send data to the civis platform
start_import_job <- function(database, tablename, if_exists, distkey,
                             sortkey1, sortkey2, max_errors) {
  if (!if_exists %in% c("fail", "truncate", "append")) {
    stop('if_exists must be set to "fail", "truncate" or "append"')
  }

  # Split tablename into schema and table
  parts <- split_schema_name(tablename)
  schema <- parts$schema
  table <- parts$table

  # Instantiate table creation job
  db_id <- get_database_id(database)
  creds <- default_credential()
  job_response <- imports_post_files(schema, table, db_id, creds,
                                      max_errors, if_exists, distkey,
                                      sortkey1, sortkey2, "comma", TRUE)
  job_response
}

# Get the name of delimiter platform expects from actual string
delimiter_name_from_string <- function(s) {
  delimiters <- list("," = "comma", "|" = "pipe", "\t" = "tab")
  delimiter <- delimiters[[s]]
  if (is.null(delimiter)) {
    keys <- paste0("'", paste0(names(delimiters), collapse = "', '"), "'")
    stop(paste("Delimiter must be one of", keys))
  }
  delimiter
}

download_script_results <- function(script_id, run_id,
                                    filename = NULL, progress = FALSE) {
  script_results <- scripts_get_sql_runs(script_id, run_id)
  # Verify that the script actually generated output
  if (length(script_results[["output"]]) == 0) {
    msg <- paste0("Query produced no output. ",
                  "(script_id = ", script_id,
                  ", run_id = ", run_id, ")")
    cond <- condition(c("empty_result_error", "error"), msg, call = NULL)
    stop(cond)
  }
  url <- script_results[["output"]][[1]][["path"]]
  args <- list(url)
  if (!is.null(filename)) {
    args <- c(args, list(httr::write_disk(filename, overwrite = TRUE)))
  }
  if (progress) {
    args <- c(args, list(httr::progress()))
  }
  r <- do.call(httr::GET, args)
  stop_for_status(r)
  r
}

#' Call a function with a temporary file.
#'
#' @param fn a function that takes a filename as the first argument.
#' @param ... arguments passed to to fn.
#'
#' @return object the return value of fn
#'
#' @examples \dontrun{
#' data(iris)
#' with_tempfile(function(file_name) {
#'  write.csv(iris, file_name)
#'  # add'l operations
#' })
#' }
#'
#' @keywords internal
with_tempfile <- function(fn, ...) {
  tryCatch({
    filename <- tempfile()
    fn(filename, ...)
  }, finally = {
    unlink(filename)
  })
}


