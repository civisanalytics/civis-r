library(civis)
context("io")

# read_civis ------------------------------------------------------------------

test_that("read_civis.sql reads csvs", {
  mock_df <- data.frame(a = 1:2, b = c("sentimental", "centipede"))
  mock_get_sql_runs <- function(...) {
    list(state = "succeeded",
         output = list(list(fileId = 1234))
    )
  }
  mock_sql_job <- function(...) list(script_id = 1337, run_id = 007)
  mock_download_script_results <- function(id, run_id, filename) {
    write.csv(mock_df, file = filename, row.names = FALSE)
    return(filename)
  }
  with_mock(
    `civis::start_scripted_sql_job` = mock_sql_job,
    `civis::scripts_post_sql_runs` = function(...) list(id = 1001),
    `civis::scripts_get_sql_runs` = mock_get_sql_runs,
    `civis::files_get` = function(...) list(fileSize = 4E9),
    `civis::download_script_results` = mock_download_script_results,
    `civis::stop_for_status` = function(...) return(TRUE),
    expect_equal(mock_df, read_civis(x = "lazy", database = "jellyfish")),
    expect_equal(mock_df, read_civis(sql("SELECT * FROM lazy"),
                                     database = "jellyfish"))
  )
})

test_that("read_civis.sql produces catchable error when query returns no rows", {
  no_results_resp <- list(state = "succeeded", output = list())

  mock_sql_job <- function(...) list(script_id = 561, run_id = 43)
  with_mock(
    `civis::start_scripted_sql_job` = mock_sql_job,
    `civis::scripts_get_sql_runs` = function(...) no_results_resp,
    try_err <- try(read_civis(sql("SELECT 0"), database = "arrgh"), silent = TRUE),
    expect_true("empty_result_error" %in% class(attr(try_err, "condition")))
  )
})

test_that("read_civis.numeric reads a csv", {
  d <- data.frame(a = 1:2, b = c("sentimental", "centipede"))
  mock_response <- function(...) {
    structure(list(url = "http://www.fakeurl.com", status_code = 200),
              class = "response")
  }
  with_mock(
    `civis::files_get` =  function(...) list(fileUrl = "fakeurl.com"),
    `httr::GET` = mock_response,
    `civis::download_civis` = function(id, fn) write.csv(d, file = fn),
    expect_equal(d, read_civis(123, using = read.csv, row.names = 1))
  )
})

test_that("read_civis.numeric fails for NA", {
  msg <- "File ID cannot be NA."
  expect_error(read_civis(as.numeric(NA)), msg)
})

test_that("read_civis.civis_script using = NULL", {
  mock_output <- list(list(name = 'asdf', objectId = 1, objectType = 'JSONValue', value = 'a'),
                      list(name = 'fake', objectId = 2, objectType = 'JSONValue', value = 'b'),
                      list(name = 'file_fake', objectId = 3, objectType = 'File'))
  vals <- with_mock(
    `civis::jobs_get` = function(...) list(type = 'JobTypes::ContainerDocker'),
    `civis::scripts_list_containers_runs_outputs` = function(...) mock_output,
    expect_equal(read_civis(civis_script(1,1), using = NULL),
                 list(asdf = 'a', fake = 'b')),
    expect_equal(read_civis(civis_script(1,1), using = NULL, regex = 'fake'),
                 list(fake = 'b'))
  )
})

test_that("read_civis.civis_script with using", {
  mock_output <- list(list(name = 'asdf', objectId = 1, objectType = 'JSONValue', value = 'a'),
                      list(name = 'lol', objectId = 2, objectType = 'File'),
                      list(name = 'fake', objectId = 3, objectType = 'File'))
  with_mock(
    `civis::jobs_get` = function(...) list(type = 'JobTypes::ContainerDocker'),
    `civis::scripts_list_containers_runs_outputs` = function(...) mock_output,
    # returns first arg of read_civis.numeric, which is the objectId
    `civis::read_civis.numeric` = function(...) list(...)[[1]],
    expect_equal(read_civis(civis_script(1,1), using = I),
                 list(lol = 2, fake = 3)),
    expect_equal(read_civis(civis_script(1,1), regex = 'fake', using = I),
                 list(fake = 3))
  )
})


# write_civis -----------------------------------------------------------------

test_that("write_civis.character returns meta data if successful", {
  mock_df <- cbind.data.frame(a = c(1,2), b = c("cape-cod", "clams"))
  write("", file = "mockfile")
  res <- with_mock(
    `civis::start_import_job` = function(...) {
      list(uploadUri = "fake", id = 1)
    },
    `civis::default_credential` = function(...) 1234,
    `civis::tables_post_refresh` = function(id) "",
    `httr::PUT` = function(...) list(status_code = 200),
    `civis::imports_post_files_runs` = function(...) list(""),
    `civis::imports_get_files_runs` = function(...) list(state = "succeeded"),
      write_civis("mockfile", "mock.table", "mockdb")
    )
  unlink("mockfile")
  expect_equal(get_status(res), "succeeded")
})

test_that("write_civis.character fails if file doesn't exist", {
  mock_df <- cbind.data.frame(a = c(1,2), b = c("cape-cod", "clams"))
  err_msg <- with_mock(
    `civis::start_import_job` = function(...) {
      list(uploadUri = "fake")
    },
    `httr::PUT` = function(...) list(status_code = 200),
    `civis::imports_post_files_runs` = function(...) list(""),
    `civis::imports_get_files_runs` = function(...) list(state = "succeeded"),
    tryCatch(write_civis("mockfile", "mock.table", "mockdb"), error = function(e) e$message)
  )
  msg <- "file.exists(x) is not TRUE"
  expect_equal(err_msg, msg)
})

test_that("write_civis.data.frame returns meta data if successful", {
  mock_df <- cbind.data.frame(a = c(1,2), b = c("cape-cod", "clams"))
  res <- with_mock(
    `civis::start_import_job` = function(...) {
      list(uploadUri = "fake", id = 1)
    },
    `civis::default_credential` = function(...) 1,
    `civis::tables_post_refresh` = function(id) "",
    `httr::PUT` = function(...) list(status_code = 200),
    `civis::imports_post_files_runs` = function(...) list(""),
    `civis::imports_get_files_runs` = function(...) list(state = "succeeded"),
    `civis::tables_list` = function(...) 1,
    write_civis(mock_df, "mock.table", "mockdb")
  )
  expect_equal(get_status(res), "succeeded")
})

test_that("write_civis.character warns under failure", {
  mock_df <- cbind.data.frame(a = c(1,2), b = c("cape-cod", "clams"))
  with_mock(
    `civis::start_import_job` = function(...) {
      list(uploadUri = "fake", id = -999)
    },
    `httr::PUT` = function(...) list(status_code = 200),
    `civis::imports_post_files_runs` = function(...) "",
    `civis::imports_get_files_runs` = function(...) list(state = "failed"),
    `httr::content` = function(...) "error",
    expect_error(
      write_civis("mockfile", "mock.table", "mockdb"))
  )
})

test_that("write_civis.character calls imports endpoints correctly", {
  ipf <- mock(list(id = 4))
  fn <- tempfile(fileext = ".csv")
  file.create(fn)
  with_mock(
    `civis:::get_database_id` = function(...) 32,
    `civis:::default_credential` = function(...) 999,
    `civis::imports_post_files` = ipf,
    `civis::imports_post_files_runs` = function(...) list(id = 44),
    `civis::imports_get_files_runs` = function(...) list(state = "succeeded"),
    `httr::PUT` = mock(list(status_code = 200)),
    res <- write_civis(fn, "mock.table", "mockdb", credential_id = 1),
    expect_equal(get_status(res), "succeeded"),
    expect_args(ipf, n = 1,
                schema = 'mock',
                name = 'table',
                remote_host_id = 32,
                credential_id = 1,
                max_errors = NULL,
                existing_table_rows = "fail",
                diststyle = NULL,
                distkey = NULL,
                sortkey1 = NULL,
                sortkey2 = NULL,
                column_delimiter = "comma",
                firstRowIsHeader = TRUE,
                hidden = TRUE
    )
  )
})

test_that("write_civis.numeric calls imports endpoints correctly", {
  ip <- mock(list(id = 4))
  with_mock(
    `civis:::get_database_id` = function(...) 32,
    `civis:::default_credential` = function(...) 999,
    `civis::imports_post` =  ip,
    `civis::imports_post_syncs` = mock(),
    `civis::jobs_post_runs` = function(...) list(id = 4),
    `civis::jobs_get_runs` = function(...) list(state = "succeeded"),
    res <- write_civis(1234, "mock.table", "mockdb", credential_id = 1,
                      import_args = list(verifyTableRowCounts = TRUE)),
    expect_equal(get_status(res), "succeeded"),
    expect_args(ip, n = 1,
                 import_name = "CSV import to mock.table",
                 sync_type = 'AutoImport',
                 is_output = FALSE,
                 destination = list(remote_host_id = 32, credential_id = 1),
                 hidden = TRUE),
    expect_args(civis::imports_post_syncs, n = 1,
                id = 4,
                list(file = list(id = 1234)),
                destination = list(database_table =
                                     list(schema = "mock", table = "table")),
                advanced_options = list(
                  max_errors = NULL,
                  existing_table_rows = "fail",
                  distkey = NULL,
                  diststyle = NULL,
                  sortkey1 = NULL,
                  sortkey2 = NULL,
                  column_delimiter = "comma",
                  firstRowIsHeader = TRUE,
                  verifyTableRowCounts = TRUE
                ))
  )
})

test_that("write_civis fails if no db given and default not provided", {
  with_mock(
    `civis::get_default_database` = function(...) NULL,
    err_msg <- tryCatch(write_civis(iris), error = function(e) e$message),
    db_err <- tryCatch(get_db(NULL), error = function(e) e$message),
    expect_equal(err_msg, db_err)
  )
})

test_that("write_civis.numeric fails for NA", {
  msg <- "File ID cannot be NA."
  expect_error(write_civis(as.numeric(NA)), msg)
})

test_that("write_civis_file fails if file doesn't exist", {
  regexp <- "File 'x' does not exist.*"
  expect_error(write_civis_file("asdf"), regexp)
})

test_that("write_civis_file fails if character vector length 2 is passed", {
  regexp <- "'x' has length > 1.*"
  expect_error(write_civis_file(c('fake', 'paths')), regexp)
})

test_that("write_civis_file.character returns a file id", {
  write("", "mockfile.txt")
  with_mock(
    `civis::files_post` = function(...) list(uploadFields = list("fakeurl.com"), id = 5),
    `httr::upload_file` = function(...) "the file",
    `httr::RETRY` = function(...) structure(list(status_code = 200), class = "response"),
     expect_equal(write_civis_file("mockfile.txt", name = "mockfile.txt"), 5)
  )
  unlink("mockfile.txt")
})

test_that("write_civis_file returns a file id", {
  mock_df <- data.frame(a = c(1,2), b = c("cape-cod", "clams"))
  with_mock(
    `civis::files_post` = function(...) list(uploadFields = list("fakeurl.com"), id = 5),
    `httr::upload_file` = function(...) "the file",
    `httr::RETRY` = function(...) structure(list(status_code = 200), class = "response"),
    expect_equal(write_civis_file(mock_df), 5),
    expect_equal(write_civis_file(as.list(mock_df)), 5),
    expect_equal(write_civis_file(1:3), 5)
  )
})

test_that("write_civis_file calls multipart_unload for big files", {
  fake_file_size <- mock(file.size)
  mockery::stub(write_civis_file.character, "file.size", MIN_MULTIPART_SIZE + 1)
  fn <- tempfile()
  file.create(fn)
  with_mock(
    `civis::multipart_upload` = function(...) 1,
    expect_equal(write_civis_file(fn, name = "asdf"), 1)
  )
  unlink(fn)
})

test_that("write_civis_file.data.frame uploads a csv", {
  m <- mock()
  with_mock(
    `civis::with_tempfile` = m,
    `civis:::write_civis_file.character` = function(...) 1,
    write_civis_file(iris),
    # call the temporary function given to with_tempfile
    mock_args(m)[[1]][[1]]('tmp.csv'),
    expect_equal(read.csv('tmp.csv'), iris),
    unlink('tmp.csv')
  )
})

# download_civis --------------------------------------------------------------

test_that("download_civis raises an error if destination file is not specified", {
  expect_error(download_civis("mock.table", "mockdb"),
               "argument \"file\" is missing")
})

test_that("download_civis raises an error if destination file already exists", {
  local_file <- "my_table.csv"
  file.create(local_file)
  expect_error(download_civis("mock.table", "mockdb", local_file, overwrite = FALSE),
               "File already exists")
  file.remove(local_file) # Clean up after testing
})

test_that("download_civis.numeric fails for NA", {
  msg <- "File ID cannot be NA."
  expect_error(write_civis(as.numeric(NA)), msg)
})


# query_civis -----------------------------------------------------------------

test_that("query_civis returns object from await", {
  qp <- mockery::mock()
  with_mock(
    `civis::get_database_id` = function(...) TRUE,
    `civis::default_credential` = function(...) 1,
    `civis::queries_post` = qp,
    `civis::queries_get` = function(...) list(state = 'succeeded'),
    expect_equal(get_status(query_civis("query", "database", credential = 10)), 'succeeded'),
    expect_equal(mockery::mock_args(qp)[[1]]$credential, 10)
  )
})

test_that("query_civis.numeric fails for NA", {
  msg <- "Query ID cannot be NA."
  expect_error(query_civis(as.numeric(NA)), msg)
})

test_that("query_civis_file.sql works", {
  with_mock(
    `civis::get_database_id` = function(...) TRUE,
    `civis::get_db` = function(...) "asdf",
    `civis::default_credential` = function(...) TRUE,
    `civis:::start_scripted_sql_job` = function(...) list(script_id = 1, run_id = 1),
    `civis::scripts_get_sql_runs` = function(...) list(state = "succeeded",
                                                       output = list(list(fileId = 1))),
    expect_equal(query_civis_file(sql("asdf")), 1)
  )
})

test_that("query_civis_file.character errors if not schema.tablename", {
  msg <- 'Argument x should be "schema.tablename". Did you mean x = sql("...")?'
  expect_error(query_civis_file("select asdf"), msg)
})

test_that("query_civis_file.numeric works", {
  with_mock(
    `civis::scripts_post_sql_runs` = function(...) list(id = 333),
    `civis::scripts_get_sql_runs` = function(...) list(state = "succeeded",
                                                       output = list(list(fileId = 1))),
    expect_equal(query_civis_file(234), 1)
  )
})

test_that("query_civis_file.numeric fails for NA", {
  msg <- "Query ID cannot be NA"
  expect_error(query_civis(as.numeric(NA), msg))
})

test_that("transfer_table succeeds", {
  res <- with_mock(
    `civis::default_credential` = function(...) 1,
    `civis::get_database_id` = function(...) 32,
    `civis::imports_post` = function(...) list(id = 999),
    `civis::imports_post_syncs` = function(...) NULL,
    `civis::imports_post_runs` = function(...) list(runId = 999),
    `civis::imports_get_files_runs` = function(...) list(state = "succeeded"),
    transfer_table("db1", "db2", "sc.tb1", "sc.tb2")
  )
  expect_equal(get_status(res), "succeeded")
})

# utils functions -------------------------------------------------------------

test_that("multipart_upload returns file_id", {
  fn <- tempfile()
  d <- data.frame(a = 1:5, b = 5:1)
  write.csv(d, fn, row.names = FALSE)
  id <- with_mock(
    `civis::upload_one` = function(...) NULL,
    `civis::files_post_multipart` = function(...) list(id = 1, uploadUrls = "url"),
    `future::value` = function(...) NULL,
    `civis::files_post_multipart_complete` = function(...) NULL,
    multipart_upload(fn, name = "asdf")
  )
  expect_equal(id, 1)
})

test_that("write_chunks splits files", {
  # csv
  d <- data.frame(a = 1:5, b = 5:1)
  fn <- tempfile(fileext = ".txt")
  write.csv(d, fn, row.names = FALSE)
  fl <- write_chunks(fn, chunk_size = file.size(fn)/4)
  expect_equal(length(fl), 4)

  the_text <- paste0(unlist(lapply(fl, function(f) {
    readChar(f, file.size(f))
  })), collapse = "")
  ans <- read.csv(textConnection(the_text))
  expect_equal(ans, d)

  # rds; again, we have to really just stitch the files together to read it back.
  fn <- tempfile(fileext = ".rds")
  saveRDS(d, fn)
  fl <- write_chunks(fn, chunk_size = file.size(fn)/4)

  the_bin <- unlist(lapply(fl, function(f) {
    readBin(f, what = "raw", file.size(f))
  }))
  zz <- rawConnection(the_bin)
  ans <- readRDS(gzcon(zz))
  close(zz)
  expect_equal(ans, d)
})

test_that("get_db returns default database or an error", {
  expect_equal(get_db("sea_creatures"), "sea_creatures")

  msg <- c("Argument database is NULL and options(\"civis.default_db\") not set. Set this option using options(civis.default_db = \"my_database\")")
  test_msg <- with_mock(
    `civis::get_default_database` = function(...) NULL,
    tryCatch(get_db(NULL), error = function(e) e$message)
  )
  expect_equal(msg, test_msg)

  options(civis.default_db = "sea_creatures")
  expect_equal(get_db(NULL), "sea_creatures")
})

test_that("delimiter_name_from_string catches bad input", {
  e <- "Delimiter must be one of ',', '|', '\t'"
  comma <- delimiter_name_from_string(",")
  pipe <- delimiter_name_from_string("|")
  tab <- delimiter_name_from_string("\t")
  expect_equal(comma, "comma")
  expect_equal(pipe, "pipe")
  expect_equal(tab, "tab")
  expect_error(delimiter_name_from_string(":"), e)
})


test_that("start_import_job parses table correctly", {
  with_mock(
    `civis::get_database_id` = function(...) -999,
    `civis::default_credential` = function(...) "fake",
    `civis::imports_post_files` = function(...) {
      args <- list(...)
      list(schema = args[[1]], table = args[[2]])
    },
    expect_equal(
      start_import_job("mockdb", "mock.table", if_exists = "append",
                       NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
      list(schema = "mock", table = "table")
    )
  )
})

test_that("start_import_job checks if_exists value", {
  error_msg <- 'if_exists must be set to "fail", "truncate", "append", or "drop"'
  with_mock(
    `civis::get_database_id` = function(...) -999,
    `civis::default_credential` = function(...) "fake",
    `civis::imports_post_files` = function(...) {
      args <- list(...)
      list(schema = args[[1]], table = args[[2]])
    },
    expect_error(
      start_import_job("mockdb", "mock.table", if_exists = "do nothing",
                       NULL, NULL, NULL, NULL),
      error_msg
    )
  )
})

test_that("download_script_results returns sensible errors", {
  error <- "Query produced no output. \\(script_id = 561, run_id = 43\\)"
  mock_get_run <- function(script_id, run_id) list(script_id = script_id, run_id = run_id)
  with_mock(
    `civis::scripts_get_sql_runs` = mock_get_run,
    expect_error(download_script_results(561, 43, "some_file"), error)
  )
})
