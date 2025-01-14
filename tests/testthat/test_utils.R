library(civis)
context("utils")


db_list_response <- list(
  list(name = "db1", id = 1),
  list(name = "db2", id = 2),
  list(name = "db3", id = 3),
  list(name = "redshift-db4", id = 4),
  list(name = "DB5", id = 5),
  list(name = "ham sandwich", id = 6)
)

test_that("get_database_id returns a matching id", {
  # simulates a character of class glue often used for
  # string interpolation
  glue_str <- structure("db1", class = c("glue", "character"))
  with_mocked_bindings(
    `civis::databases_list` = function(...) db_list_response,
    expect_equal(get_database_id(glue_str), 1),
    expect_equal(get_database_id("db2"), 2)
  )
})

test_that("get_database_id returns an error on no match", {
  e1 <- "Database db4 not found\\. Did you mean redshift-db4\\?"
  e2 <- "Database db5 not found\\. Did you mean DB5\\?"
  e3 <- "Database turkey sandwich not found\\. Did you mean ham sandwich\\?"
  with_mocked_bindings(
    `civis::databases_list` = function(...) db_list_response,
    expect_error(get_database_id("db4"), e1),
    expect_error(get_database_id("db5"), e2),
    expect_error(get_database_id("turkey sandwich"), e3)
  )
})

# When `credentials_list(default = TRUE)` the response is a single element list
# with the default credential.
credentials_list_response <- list(
  list(remoteHostId = NULL, username = "c", id = 12, type = "Database")
)

test_that("default_credential returns a credential id", {
  with_mocked_bindings(
    `civis::credentials_list` = function(...) credentials_list_response,
    `civis::get_username` = function() "c",  # used by default_credential
    expect_equal(default_credential(), 12)
  )
})

test_that("get_default_database sets package option for db_list of length 1", {
  options(civis.default_db = NULL)
  with_mocked_bindings(
    `civis::databases_list` = function(...) list(list(id = 555, name = "the_only_db")),
    db <- get_default_database(),
    expect_equal(db, "the_only_db")
  )
  # test that the option is set as a side-effect
  expect_equal(get_default_database(), "the_only_db")
})
