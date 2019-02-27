#' Read tables and files from Civis Platform
#'
#' @description \code{read_civis} loads a table from Redshift as a data frame if
#' given a \code{"schema.table"} or \code{sql("query")} as the first argument, or
#' loads a file from Amazon S3 (the files endpoint) if a file id is given.
#' Run output ids or values from any Civis platform script
#' are returned if a \code{\link{civis_script}} is given.
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
#' @examples
#' \dontrun{
#' # Read all columns in a single table
#' df <- read_civis("schema.my_table", database = "my_database")
#'
#' # Read data from a SQL select statement
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
#' # Read and download all run outputs from a civis script.
#' ids <- read_civis(civis_script(1234))
#' mapply(download_civis, ids, names(ids))
#'
#' # Read run output objects into memory
#' df <- read_civis(civis_script(1234), regex = '.csv', using = read.csv)
#' obj <- read_civis(civis_script(1234), regex = '.rds', using = readRDS)
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
  stopifnot(is.function(using))
  if (is.na(x)) stop("File ID cannot be NA.")
  fn <- tempfile()
  tryCatch({
    download_civis(x, fn)
    res <- using(fn, ...)
  }, finally = {
    unlink(fn)
  })
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
read_civis.sql <- function(x, database = NULL, using = utils::read.csv,
                           job_name = NULL, hidden = TRUE, verbose = FALSE, ...) {
  db <- get_db(database)
  sql_str <- as.character(x)
  job_name <- if (is.null(job_name)) "Civis Export Via R Client"
  if (!is.character(job_name)) stop("job_name must be a string.")
  run <- start_scripted_sql_job(db, sql_str, job_name, hidden)
  r <- await(scripts_get_sql_runs, id = run$script_id, run_id = run$run_id)
  stop_if_no_output(r)
  tryCatch({
    tmp <- tempfile()
    resp <- download_script_results(run$script_id, run_id = run$run_id,
                                    filename = tmp)
    using(tmp, ...)
  }, finally = {
    unlink(tmp)
  })
}

#' @describeIn read_civis Return run output values or file ids of a \code{civis_script} as a list.
#' @param regex Regex of matching run output names.
#' @details
#' If \code{using = NULL}, \code{read_civis.civis_script}
#' will return all JSONValues with name matching \code{regex}.
#' Otherwise all File run outputs matching \code{regex} will be read into memory
#' with \code{using}.
#' Results are always a list. If the script has no outputs, an error will be signalled.
#' @export
read_civis.civis_script <- function(x, regex = NULL, using = NULL, ...) {
  output <- fetch_output(x, regex = regex)
  if (is.null(using)) {
    out <- Filter(function(o) o$objectType == 'JSONValue', output)
    names <- lapply(out, function(o) o$name)
    res <- setNames(lapply(out, function(o) o$value), names)
  } else {
    out <- Filter(function(o) o$objectType == 'File', output)
    names <- lapply(out, function(o) o$name)
    res <- setNames(lapply(out, function(o) read_civis(o, using = using, ...)), names)
  }
  return(res)
}


#' Upload a local data frame or csv file to the Civis Platform (Redshift)
#'
#' @description Uploads a data frame, a csv file, or file on S3 to Redshift based
#' on the first argument.
#'
#' A default database can be set using \code{options(civis.default_db = "my_database")}.
#' If there is only one database available,
#' this database will automatically be used as the default.
#'
#' @param x data frame, file path of a csv, or the id of a csv file on S3 to upload to platform.
#' @param tablename string, Name of table and schema \code{"schema.tablename"}.
#' @param database string, Name of database where data frame is to be uploaded. If no database is specified,
#' uses \code{options(civis.default_db)}.
#' @param if_exists string, optional,  String indicating action to take if table already
#' exists.  Must be either "fail", "drop", "truncate" or "append". Defaults to "fail".
#' @param distkey string, optional, Column name designating the distkey.
#' @param sortkey1 string, optional, Column name designating the first sortkey.
#' @param sortkey2 string, optional, Column name designating the second
#' (compound) sortkey.
#' @param max_errors int, optional, Maximum number of rows with errors
#' to remove before failing.
#' @param verbose bool, Set to TRUE to print intermediate progress indicators.
#' @param delimiter string, optional. Which delimiter to use. One of
#' \code{','}, \code{'\\t'} or \code{'|'}.
#' @param hidden bool, if \code{TRUE} (default), this job will not appear in the Civis UI.
#' @param diststyle string optional. The diststyle to use for the table. One of "even", "all", or "key".
#' @param header bool, if \code{TRUE} (default) the first row is a header.
#' @param credential_id integer, the id of the credential to be used when performing
#' the database import. If \code{NULL} (default), the default credential of the
#' current user will be used.
#' @param import_args list of additional arguments for \code{\link{imports_post_files}}.
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
#' # Create new table with additional options
#' write_civis(df, "schema.my_table", "my_database",
#'             distkey="id",
#'             sortkey1="added_date",
#'             credential_id = 1,
#'             header = FALSE)
#'
#' # Create new table directly from a saved csv
#' write_civis("my/file/path.csv", "schema.my_table", "my_database")
#'
#' # Create new table from a file_id
#' id <- write_civis_file("my/file/path.csv", name = "path.csv")
#' write_civis(id, "schema.my_table", "my_database")
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
                        max_errors = NULL, verbose = FALSE, hidden = TRUE,
                        diststyle = NULL, header = TRUE, credential_id = NULL,
                        import_args = NULL, ...) {
  db <- get_db(database)
  tryCatch({
    filename <- tempfile(fileext = ".csv")
    utils::write.csv(x, filename, row.names = FALSE, na = "", ...)
    write_civis.character(x = filename,
                          tablename = tablename,
                          database = db,
                          if_exists = if_exists,
                          distkey = distkey,
                          sortkey1 = sortkey1,
                          sortkey2 = sortkey2,
                          max_errors = max_errors,
                          verbose = verbose,
                          hidden = hidden,
                          diststyle = diststyle,
                          header = header,
                          credential_id = credential_id,
                          import_args = import_args,
                          ...)
  }, finally = {
    unlink(filename)
  })
}

#' @describeIn write_civis Upload a csv to Civis Platform (Redshift).
#' @export
write_civis.character <- function(x, tablename, database = NULL, if_exists = "fail",
                         distkey = NULL, sortkey1 = NULL, sortkey2 = NULL,
                         max_errors = NULL, verbose = FALSE, hidden = TRUE,
                         diststyle = NULL, header = TRUE,
                         credential_id = NULL, import_args = NULL,
                         ...) {
    db <- get_db(database)
    stopifnot(file.exists(x))
    if (is.null(credential_id)) credential_id <- default_credential()
    args <- append(
      list(database = db,
          tablename = tablename,
          if_exists = if_exists,
          diststyle = diststyle,
          distkey = distkey,
          sortkey1 = sortkey1,
          sortkey2 = sortkey2,
          max_errors = max_errors,
          hidden = hidden,
          header = header,
          credential_id = credential_id),
      import_args)
    job_r <- do.call(start_import_job, args)
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

#' @describeIn write_civis Upload a csv file from the files endpoint to Civis Platform (Redshift)
#' @export
write_civis.numeric <- function(x, tablename, database = NULL, if_exists = "fail",
                                distkey = NULL, sortkey1 = NULL, sortkey2 = NULL,
                                max_errors = NULL, verbose = FALSE,
                                delimiter = ",", hidden = TRUE, diststyle = NULL,
                                header = TRUE, credential_id = NULL,
                                import_args = NULL, ...) {
  if (is.na(x)) stop("File ID cannot be NA.")
  db <- get_db(database)
  db_id <- get_database_id(db)
  delimiter <- delimiter_name_from_string(delimiter)

  parts <- split_schema_name(tablename)
  import_name <- paste0("CSV import to ", tablename)
  if (is.null(credential_id)) credential_id <- default_credential()
  destination <- list(remote_host_id = db_id, credential_id = credential_id)

  job <- imports_post(import_name, 'AutoImport',
                     is_outbound = FALSE,
                     destination = destination,
                     hidden = hidden)
  options <- append(
    list(max_errors = max_errors,
         existing_table_rows = if_exists,
         distkey = distkey,
         diststyle = diststyle,
         sortkey1 = sortkey1,
         sortkey2 = sortkey2,
         column_delimiter = delimiter,
         firstRowIsHeader = header
    ),
    import_args)

  imports_post_syncs(job$id,
                     source = list(file = list(id = x)),
                     destination = list(database_table =
                                        list(schema = parts$schema, table = parts$table)),
                     advanced_options = options)
  run <- jobs_post_runs(job$id)
  await(jobs_get_runs, id = job$id, run_id = run$id, .verbose = verbose)
}


#' Upload a R object or file to Civis Platform (Files endpoint)
#'
#' @description Uploads a R object or file to the files endpoint on Civis
#' Platform (Amazon S3). It returns the id of the file for use with \code{\link{read_civis}}
#' or \code{\link{download_civis}}.
#'
#' R objects are serialized with \code{\link{saveRDS}}, files are unserialized.
#' Objects or files larger than 50mb are chunked and can be uploaded in parallel
#' if a \code{\link{plan}} has been set. Files larger than 5TB cannot be uploaded.
#'
#' @param x R object or path of file to upload.
#' @param name string, Name of the file or object.
#' @param expires_at string, The date and time the object will expire on in the
#' format \code{"YYYY-MM-DD HH:MM:SS"}. \code{expires_at = NULL} allows files to be kept indefinitely.
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
#' read_civis(file_id, using = readr::read_csv)
#'
#' # Does not expire
#' file_id <- write_civis_file(iris, expires_at = NULL)
#'
#' # Expires on a given date and time
#' file_id <- write_civis_file(iris, expires_at = "2030-01-01")
#' file_id <- write_civis_file(iris, expires_at = "12:00:00")
#' file_id <- write_civis_file(iris, expires_at = "2030-01-01 12:00:00")
#'
#' # Upload a large file in parallel.
#' library(future)
#' plan(multisession)
#' file_id <- write_civis_file("my_large_file")
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
  with_tempfile(function(tmp_file, ...) {
    saveRDS(x, file = tmp_file, ...)
    write_civis_file.character(x = tmp_file, name = name, expires_at = expires_at)
  })
}

#' @describeIn write_civis_file Upload a text file to Civis Platform (Files endpoint).
#' @export
write_civis_file.character <- function(x, name = x, expires_at = NULL, ...) {
  if (length(x) > 1 || !file.exists(x)) {
    err <- ifelse(length(x) > 1,
                  "'x' has length > 1.",
                  "File 'x' does not exist.")
    msg <- paste(err, "If 'x' is a character vector to be uploaded rather",
                 "than a filename, try write_civis_file(as.list(x), ...)")
    stop(msg)
  }
  size <- file.size(x)
  if (size > MAX_FILE_SIZE) stop("File larger than 5tb, can't upload.")
  if (size > MIN_MULTIPART_SIZE) {
    chunk_size_guess <- max(ceiling(sqrt(MIN_PART_SIZE) * sqrt(size)), MIN_PART_SIZE)
    chunk_size <- min(chunk_size_guess, MAX_PART_SIZE)
    id <- multipart_upload(x, name, chunk_size, expires_at)
  } else {
    u <- files_post(name = name, expires_at = expires_at)
    uploadFields <- u$uploadFields
    uploadFields$file <- httr::upload_file(x)
    resp <- httr::RETRY("POST", url = u$uploadUrl, body = uploadFields)
    httr::stop_for_status(resp, task = "upload file to S3")
    id <- u$id
  }
  return(id)
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
#' # Download data from a SQL select statement into a CSV
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
  sql_str <- as.character(x)
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
  if (is.na(x)) stop("File ID cannot be NA.")
  if (!overwrite & file.exists(file)) {
    stop("File already exists. To overwrite, set overwrite = TRUE.")
  }

  args <- c(list(files_get(x)$fileUrl),
            list(httr::write_disk(file, overwrite = overwrite)))
  if (progress) args <- c(args, list(httr::progress()))
  resp <- do.call(httr::GET, args)

  httr::stop_for_status(resp, task = "download file from S3")
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
#'   \item delimiter: the delimiter used to separate fields
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
  job_name <- ifelse(is.null(job_name), "Civis Export", job_name)
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
#' @seealso io
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
  if (is.na(x)) stop("Query ID cannot be NA.")
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

#' Export results from a query to S3 and return a file id.
#'
#' Exports results from a Redshift SQL query, and returns the id of the file on S3 for use
#' with \code{\link{read_civis}} or \code{\link{download_civis}}.
#'
#' @param x "schema.table", \code{sql("query")}, or a sql script job id.
#' @param database string, Name of database where data frame is to be uploaded.
#' If no database is specified, uses \code{options(civis.default_db)}.
#' @param job_name string, Name of the job (default: \code{"Civis S3 Export Via R Client"}).
#' @param hidden bool, Whether the job is hidden.
#' @param verbose bool, Set to TRUE to print intermediate progress indicators.
#' @param csv_settings See \code{\link{scripts_post_sql}} for details.
#' @param ... Currently ignored.
#' @export
#' @family io
#' @details
#' By default, the export uses the default csv_settings in \code{\link{scripts_post_sql}},
#' which is a gzipped csv.
#' @examples
#' \dontrun{
#' id <- query_civis_file("schema.tablename", database = "my_database")
#' df <- read_civis(id, using = read.csv)
#'
#' query <- sql("SELECT * FROM table JOIN other_table USING id WHERE var1 < 23")
#' id <- query_civis_file(query)
#' df <- read_civis(id, using = read.csv)
#'
#' id <- query_civis_file(query_id)
#' df <- read_civis(id, using = read.csv)
#' }
query_civis_file <- function(x, ...){
  UseMethod("query_civis_file")
}

#' @export
#' @describeIn query_civis_file Export a \code{"schema.table"} to a file id.
query_civis_file.character <- function(x, database = NULL, job_name = NULL, hidden = TRUE,
                                       verbose = verbose, csv_settings = NULL, ...) {
  if (stringr::str_detect(tolower(x), "\\bselect\\b")) {
    msg <- c("Argument x should be \"schema.tablename\". Did you mean x = sql(\"...\")?")
    stop(msg)
  }
  sql_str <- sql(paste0("SELECT * FROM ", x))
  query_civis_file.sql(sql_str, database = database)
}

#' @export
#' @describeIn query_civis_file Export results of a query to a file id.
query_civis_file.sql <- function(x, database = NULL, job_name = NULL, hidden = TRUE,
                                 verbose = FALSE, csv_settings = NULL, ...) {
  x <- as.character(x)
  db <- get_db(database)
  cred_id <- default_credential()
  if (is.null(job_name)) job_name <- "Civis S3 Export Via R Client"
  run <- start_scripted_sql_job(database = db,
                                sql = x,
                                job_name = job_name,
                                hidden = hidden,
                                csv_settings = csv_settings)
  res <- await(scripts_get_sql_runs,
               id = run$script_id, run_id = run$run_id, .verbose = verbose)
  res$output[[1]]$fileId
}

#' @export
#' @describeIn query_civis_file Run an existing sql script and return the file id of the results on S3.
query_civis_file.numeric <- function(x, database = NULL, verbose = FALSE, ...) {
  if (is.na(x)) stop("Query ID cannot be NA.")
  run <- scripts_post_sql_runs(x)
  res <- await(scripts_get_sql_runs, id = x, run_id = run$id, .verbose = verbose)
  res$output[[1]]$fileId
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

#' Upload to files endpoint in parts.
#'
#' If a future::plan has been set, will be carried out in parallel.
#' @param file the file
#' @param name name of the upload, defualts to
#' @param chunk_size size of the chunks in bytes
#' @param expires_at when the file expires (default never).
#'
multipart_upload <- function(file, name = "", chunk_size = 32 * 1024, expires_at = NULL) {
  fls <- write_chunks(file, chunk_size = chunk_size)
  u <- files_post_multipart(name, length(fls), expires_at = expires_at)
  urls <- u$uploadUrls
  uploads <- lapply(seq_along(urls), upload_one, urls = urls, fls = fls)
  lapply(uploads, future::value)
  files_post_multipart_complete(u$id)
  lapply(fls, unlink)
  u$id
}

upload_one <- function(i, urls, fls) {
  future::future({
    data <- httr::upload_file(fls[[i]], type = "raw")
    resp <- httr::RETRY("PUT", url = urls[[i]], body = data)
    cat("Uploading file part ", i, " of ", length(urls), fill = TRUE)
    httr::stop_for_status(resp, task = "upload file to S3")
    return(resp)
  })
}

MIN_MULTIPART_SIZE <- 50 * 2 ^ 20  # 50MB
MIN_PART_SIZE <- 5 * 2 ^ 20  # 5MB
MAX_PART_SIZE <- 5 * 2 ^ 30  # 5GB
MAX_FILE_SIZE <- 5 * 2 ^ 40  # 5TB

#' Split a file into chunks of a given chunk size, returning a list of file names.
#' @param file name of the file
#' @param chunk_size size of the chunk in bytes.
write_chunks <- function(file, chunk_size) {
  size <- file.size(file)
  n_chunks <- ceiling(size / chunk_size)
  fns <- sapply(seq(n_chunks), function(x) tempfile(fileext = ".txt"))
  rr <- file(file, "rb")
  for (i in seq(n_chunks)) {
    vals <- readBin(rr, what = "raw", chunk_size)
    zz <- file(fns[i], "wb")
    writeBin(vals, con = zz)
    close(zz)
  }
  close(rr)
  return(fns)
}


start_import_job <- function(database, tablename, if_exists, distkey,
                             sortkey1, sortkey2, max_errors, hidden,
                             diststyle, header, credential_id, ...) {
  if (!if_exists %in% c("fail", "truncate", "append", "drop")) {
    stop('if_exists must be set to "fail", "truncate", "append", or "drop"')
  }

  # Split tablename into schema and table
  parts <- split_schema_name(tablename)
  schema <- parts$schema
  table <- parts$table

  # Instantiate table creation job
  db_id <- get_database_id(database)
  args <- list(schema = schema,
               name = table,
               remote_host_id = db_id,
               credential_id = credential_id,
               max_errors = max_errors,
               existing_table_rows = if_exists,
               diststyle = diststyle,
               distkey = distkey,
               sortkey1 = sortkey1,
               sortkey2 = sortkey2,
               column_delimiter = "comma",
               first_row_is_header = header,
               hidden = hidden,
               ...)
  job_response <- do.call(imports_post_files, args)
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
  stop_if_no_output(script_results)

  url <- script_results[["output"]][[1]][["path"]]
  args <- list(url)
  if (!is.null(filename)) {
    args <- c(args, list(httr::write_disk(filename, overwrite = TRUE)))
  }
  if (progress) {
    args <- c(args, list(httr::progress()))
  }
  r <- do.call(httr::GET, args)
  httr::stop_for_status(r)
  r
}

stop_if_no_output <- function(script_results) {
  if (length(script_results[["output"]]) == 0) {
    msg <- paste0("Query produced no output. ",
                  "(script_id = ", script_results$script_id,
                  ", run_id = ", script_results$run_id, ")")
    cond <- condition(c("empty_result_error", "error"), msg, call = NULL)
    stop(cond)
  }
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


