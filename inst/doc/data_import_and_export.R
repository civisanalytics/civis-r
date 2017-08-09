## ----eval=FALSE----------------------------------------------------------
#  df <- read_civis("schema.tablename", database = "my-database")

## ----eval=FALSE----------------------------------------------------------
#  options(civis.default_db = "my-database")
#  df <- read_civis("schema.tablename")

## ----eval=FALSE----------------------------------------------------------
#  query <- "SELECT * FROM table JOIN other_table USING id WHERE var1 < 23"
#  df <- read_civis(sql(query))

## ---- eval=FALSE---------------------------------------------------------
#  data(iris)
#  id <- write_civis_file(iris)
#  df <- read_civis(id)

## ----eval=FALSE----------------------------------------------------------
#  query <- "SELECT * FROM table JOIN other_table USING id WHERE var1 < 23"
#  df <- read_civis(sql(query), colClasses = "character")
#  df2 <- read_civis(sql(query), as.is = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  options(civis.default_db = "my_database")
#  df <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
#  write_civis(df, tablename = "schema.tablename",
#              distkey = "id", sortkey1 = "date", sortkey2 = "type")

## ----eval=FALSE----------------------------------------------------------
#  write_civis(df, tablename = "schema.tablename", if_exists = "append")
#  write_civis(df, tablename = "schema.tablename", if_exists = "truncate")

## ---- eval=FALSE---------------------------------------------------------
#  write_civis("~/path/to/my_data.csv", tablename="schema.tablename")

## ---- eval = FALSE-------------------------------------------------------
#  data(iris)
#  id <- write_civis_file(iris)
#  iris2 <- read_civis(id)

## ---- eval = FALSE-------------------------------------------------------
#  id <- write_civis_file("path/to/my_data.csv")
#  read_civis(id, using = read.csv)

## ----eval=FALSE----------------------------------------------------------
#  query <- "SELECT * FROM table JOIN other_table USING id WHERE var1 < 23"
#  download_civis(sql(query), file = "path/to/my_file.csv")
#  download_civis("schema.tablename", file = "path/to/my_file.csv")
#  
#  id <- write_civis_file(iris)
#  download_civis(id, file = "path/to/my_iris.rds")

## ----eval=FALSE----------------------------------------------------------
#  q_res <- query_civis("GRANT ALL ON schema.my_table TO GROUP admin")

## ---- eval = FALSE-------------------------------------------------------
#  id <- q_res$id
#  query_civis(id)

## ---- eval=FALSE---------------------------------------------------------
#   Error in api_key() :
#    The environmental variable CIVIS_API_KEY is not set. Add this to your .Renviron or call Sys.setenv(CIVIS_API_KEY = '<api_key>')

## ---- eval=FALSE---------------------------------------------------------
#  read_civis(sql("SELECT * FROM schema.tablename WHERE 1 = 0"))
#  Error in download_script_results(run$script_id, run$run_id) :
#    Query produced no output.

## ---- eval=FALSE---------------------------------------------------------
#   Error in get_db(database) :
#    Argument database is NULL and options("civis.default_db") not set. Set this option using options(civis.default_db = "my_database")

## ---- eval=FALSE---------------------------------------------------------
#  sapply(databases_list(), function(x) x$name)

