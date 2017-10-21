library(testthat)
library(civis)
library(dplyr)

# Smoke test for testing general end to end functions.  These tests hit
# outside resources (Civis API, Redshift), so necessary credentials are
# required. These tests can be used to test that large changes, especially
# changes to the endpoints, retain core functionality.  It is not recommended
# to run these tests during continuous integration tests. Use unit tests
# in tests/testhat/ instead.


context("smoke")

###############################################################################
# Set up
###############################################################################
start <- proc.time()
tablename <- paste0("scratch.r_smoke_test", sample(1:10000, 1))
database <- "redshift-general"
options(civis.default_db = "redshift-general")

###############################################################################
# Data in and out
###############################################################################

test_that("read_civis reads from redshift", {
  query <- sql("SELECT * FROM datascience.iris")
  d <- read_civis("datascience.iris", verbose = TRUE)
  d2 <- read_civis(query, verbose = TRUE)
  expect_equal(d, d2)
})

test_that("write_civis writes to redshift", {
  # Tests both write_civis.character and write_civis.default
  iris$id <- 1:nrow(iris)
  write_civis(iris, tablename, database = database, verbose = TRUE)
  x <- read_civis(tablename)
  colnames(x) <- colnames(iris)
  expect_equivalent(x[order(x$id), ], iris)
  query_civis(paste0("DROP TABLE IF EXISTS ", tablename), database = database)
})

test_that("download_civis downloads onto disk", {
  download_civis("datascience.iris", file = "file1.csv",
                 overwrite = TRUE, verbose = TRUE)
  download_civis(sql("SELECT * FROM datascience.iris"), file = "file2.csv",
                 overwrite = TRUE, verbose = TRUE)
  df1 <- read.csv("file1.csv")
  df2 <- read.csv("file2.csv")
  expect_identical(df1, df2)
  # Clean up after testing
  if (file.exists("file1.csv")) file.remove("file1.csv")
  if (file.exists("file2.csv")) file.remove("file2.csv")
})

test_that("queries are executed", {
  x <- query_civis(sql("SELECT * FROM datascience.iris"), database = "redshift-general")
  expect_is(x, "civis_api")
})

test_that("civis_to_multifile produces csv links", {
  x <- civis_to_multifile_csv("SELECT * FROM datascience.iris", "redshift-general")
  expect_equal(length(x$entries), 31)
  expect_equal(x$query, "SELECT * FROM datascience.iris")
})

test_that("tables can be transferred", {
  tablename <- paste0("scratch.r_smoke_test", sample(1:10000, 1))
  x <- transfer_table("redshift-general", "redshift-general",
                      "datascience.iris", tablename,
                      verbose = TRUE)
  expect_is(x, "civis_api")
  q <- queries_post(32, paste("DROP TABLE IF EXISTS ", tablename), 1)
})

test_that("files can be uploaded", {
  # tests both write_civis_file.default and write_civis_file.character
  data(iris)
  fid <- write_civis_file(iris)
  x <- read_civis(fid)
  expect_equal(x, iris)

  info <- files_get(id = fid)
  expect_null(info$expiresAt)

  # test large file
  library(future)
  plan("sequential")
  fn <- tempfile()
  system(paste0("dd if=/dev/zero of=", fn," bs=51m count=1"), ignore.stderr = TRUE)
  system.time(id <- write_civis_file(fn, name = "a"))

  plan("multisession")
  # if you've got a big pipe, it can make some difference...
  system.time(id <- write_civis_file(fn, name = "a"))
})

test_that("DBI Read-Only connections forbid writes", {
  tryCatch({
    conn <- dbConnect(dbi_driver(), database = database, read_only = TRUE)
    expect_error(dbWriteTable(conn, tablename = tablename, mpg))
  }, finally = {
    db_id <- get_database_id(database)
    q <- queries_post(db_id, paste("DROP TABLE IF EXISTS", tablename), 1)
  })
})

test_that("dplyr pipeline works", {
  redshift <- dbConnect(dbi_driver(), database = database)
  query <- tbl(redshift, "datascience.iris") %>%
    select(-sepal_width) %>%
    filter(petal_width < 3) %>%
    group_by(type) %>%
    summarize(mlength = mean(sepal_length), n = n()) %>%
    mutate(n2 = log(n), n3 = n * 3, n4 = n - 10, n5 = n + 5)
  show_query(query)
  x <- collect(query)
  expect_equal(nrow(x), 3)
})


###############################################################################
# Reports
###############################################################################

test_that("Reports are created", {
  tryCatch(
    {
      rmd <- c("```{r}", "summary(cars)", "```")
      html <- "<div><ul>A</ul><ul>B</ul><ul>C</ul></div>"
      rmd_file <- tempfile(fileext = ".Rmd")
      md_file <- gsub(".Rmd", ".md", basename(rmd_file))
      html_file <- tempfile(fileext = ".html")
      write(rmd, rmd_file)
      write(html, html_file)
      out1 <- publish_rmd(rmd_file = rmd_file, report_name = "R Smoke Test")
      out2 <- publish_html(html_file, report_name = "R Smoke Test2")
      expect_true(is.numeric(out1))
      expect_true(is.numeric(out2))
    }, finally = {
      unlink(rmd_file)
      unlink(html_file)
      unlink(md_file)
  })
})

###############################################################################
# Models
###############################################################################
test_that("iris model works", {
  tbl <- civis_table(table_name = "datascience.iris",
                     database_name = "redshift-general")
  m <- civis_ml_sparse_logistic(x = tbl, dependent_variable = "type",
                                primary_key = "index")
  # Just make sure this finished w/o error.
  expect_is(m, "civis_ml")
})

###############################################################################
# Futures
###############################################################################
library(future)

test_that("futures work", {
  plan(civis_platform)
  d2 <- future({read_civis("datascience.iris", "redshift-general")})
  d <- read_civis("datascience.iris", verbose = TRUE)
  expect_is(d2, "CivisFuture")
  expect_equal(d, value(d2))
})

test_that("additional packages get installed", {
  library(purrr)
  plan(civis_platform)
  fut <- future({map(1:2, c)})
  res <- value(fut)
  expect_equal(res, list(1, 2))
})

end <- proc.time()
tot <- end - start

cat("total time: ", tot[3], "s", fill = TRUE)

