context("tables")
library(civis)

options(civis.default_db = "sample_db")

test_that("transfer_table succeeds", {
  local_mocked_bindings(
    default_credential = function(...) 1,
    get_database_id = function(...) 32,
    imports_post = function(...) list(id = 999),
    imports_post_syncs = function(...) NULL,
    imports_post_runs = function(...) list(runId = 999),
    imports_get_files_runs = function(...) list(state = "succeeded"),
  )
  res <- transfer_table("db1", "db2", "sc.tb1", "sc.tb2")
  expect_equal(get_status(res), "succeeded")
})

test_that("refresh_table succeeds", {
  local_mocked_bindings(
    get_database_id = function(...) 32,
    get_table_id = function(...) 5,
    tables_post_refresh = function(...) "",
    tables_get = function(...) list(refreshStatus = "current"),
  )
  expect_equal(get_status(refresh_table("sea_creatures.whales")), "current")
})

test_that("get_table_id returns table id", {
  local_mocked_bindings(
    get_database_id = function(...) 32,
    tables_list = function(...) list(list(name = "whales", id = 5)),
  )
  expect_equal(get_table_id("sea_creatures.whales"), 5)
})

test_that("get_table_id returns error if not found", {
  msg <- paste0("Table sea_creatures.squid not found.")
  local_mocked_bindings(
    get_database_id = function(...) 32,
    tables_list = function(...) list(list(name = "whales", id = 5)),
  )
  expect_error(get_table_id("sea_creatures.squid"), msg)
})

test_that("get_table_id returns error if not schema.tablename", {
  expect_error(get_table_id("asdf"), "table name must be 'schema.table'")
})
