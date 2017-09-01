context("fetch_logs")

library(civis)
library(mockery)

log_response <- list(
  list(
    id = 1147128844,
    createdAt = "2017-07-10T02:53:11.000Z",
    message = "Script complete.",
    level = "info"
  ),
  list(
    id = 1147128841,
    createdAt = "2017-07-10T02:53:11.000Z",
    message = "Process used approximately 83.28 MiB of its 3188 limit",
    level = "info"
  )
)

fake_model <- structure(
  list(
    job = list(
      id = 123,
      name = "model_task",
      arguments = list(
        PRIMARY_KEY = "training_primary_key"
      )
    ),
    run = list(id = 456)
  ),
  class = "civis_ml"
)

test_that("fetch_logs.civis_ml calls scripts_list_custom_runs_logs", {
  fake_scripts_list_custom_runs_logs <- mock(log_response)

  with_mock(
    `civis::scripts_list_custom_runs_logs` = fake_scripts_list_custom_runs_logs,

    fetch_logs(fake_model)
  )

  expect_args(fake_scripts_list_custom_runs_logs, 1,
              id = fake_model$job$id,
              run_id = fake_model$run$id,
              limit = 100)
})

test_that("find_log_fetcher finds the right logging function", {
  expect_equal(find_log_fetcher("scripts_get_sql_runs"), scripts_list_sql_runs_logs)
})

test_that("find_log_fetcher returns error if not found", {
  expect_error(find_log_fetcher('whales_get_custom_krill'), "No function to fetch logs.")
})

fake_api <- structure(
  list(id = 123),
  response = list(),
  fname = "scripts_get_sql_runs",
  args = list(id = 123, run_id = 456),
  status = "succeeded",
  class = c("civis_api", "list")
)

test_that("fetch_logs.civis_api calls the right logging function with right args", {
  fake_scripts_list_sql_runs_logs <- mock(log_response)
  log_args <- attr(fake_api, "args")

  with_mock(
    `civis::scripts_list_sql_runs_logs` = fake_scripts_list_sql_runs_logs,
    expect_equal(fetch_logs(fake_api), format_scripts_logs(log_response))
  )
  expect_args(fake_scripts_list_sql_runs_logs, 1,
              id = log_args$id, run_id = log_args$run_id, limit = 100)
})

fake_api_err <- structure(
  list(message = "A TERRIBLE ERROR OCCURRED"),
  f = "scripts_get_sql_runs",
  args = list(id = 123, run_id = 456),
  class = c("civis_await_error", "civis_error", "error", "condition")
)


test_that("fetch_logs.civis_error calls the right logging function with right args", {
  fake_scripts_list_sql_runs_logs <- mock(log_response)
  log_args <- attr(fake_api_err, "args")

  with_mock(
    `civis::scripts_list_sql_runs_logs` = fake_scripts_list_sql_runs_logs,
    expect_equal(fetch_logs(fake_api_err), format_scripts_logs(log_response))
  )
  expect_args(fake_scripts_list_sql_runs_logs, 1,
              id = log_args$id, run_id = log_args$run_id, limit = 100)
})

test_that("formats the log messages", {
  Sys.setenv("TZ" = "CST6CDT")
  fake_scripts_list_custom_runs_logs <- mock(log_response)

  with_mock(
    `civis::scripts_list_custom_runs_logs` = fake_scripts_list_custom_runs_logs,

    messages <- fetch_logs(fake_model)
  )

  expected_messages <- structure(c(
    "2017-07-09 21:53:11 PM CDT Process used approximately 83.28 MiB of its 3188 limit",
    "2017-07-09 21:53:11 PM CDT Script complete."),
    class = "civis_logs")
  expect_equal(messages, expected_messages)

  Sys.unsetenv("TZ")
})
