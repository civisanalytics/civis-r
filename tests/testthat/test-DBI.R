context("DBItest")

tweaks <- DBItest::tweaks(
  constructor_name = "dbi_driver",
  omit_blob_tests = TRUE
)

connect_args <- list(
  database = "redshift-test",
  read_only = FALSE
)

mock_read_civis <- function(...) iris
mock_tables_list <- function(...) c("table_1", "table_2")
mock_get_table_id <- function(...) 2757016
mock_tables_get <- function(...) {
  ret <- list(columns = list(list(name = "col1", sqlType = "integer"),
                             list(name = "col2", sqlType = "integer")))
  class(ret) <- c("civis_api", "list")
  return(ret)
}
mock_get_database_id <- function(...) 32
mock_all_table_names <- function(...) c("table_1", "table_2", "table_3")
mock_get_username <- function() "a_user"
mock_write_civis <- function(...) TRUE

with_mock(
  `civis::tables_list` = mock_tables_list,
  `civis::get_database_id` = mock_get_database_id,
  `civis::all_table_names` = mock_all_table_names,
  `civis::get_username` = mock_get_username,
  DBItest::make_context(dbi_driver(), connect_args = connect_args, tweaks = tweaks),

  DBItest::test_getting_started(c(
    "package_name"  # The constructor does not match the package name.
  )),
  DBItest::test_driver(),
  DBItest::test_connection(c(
    "disconnect_invalid_connection"  # the test doesn't actually invalidate the connection
  ))
  # DBItest::test_result(),
  # DBItest::test_sql(),
  # DBItest::test_meta(),
  # DBItest::test_transaction(),
  # DBItest::test_compliance()
)
