context("civis_future")
library(civis)
library(future)
library(mockery)

mock_r_eval <- function(fut) {
  r_remote_eval <- parse(system.file("scripts", "r_remote_eval.R", package = "civis"))

  # so you can delete expressions.
  # this deletes the commandArgs lines, which can no longer be mocked.
  r_remote_eval[2:3] <- NULL

  with_mock(
    `civis::read_civis` = function(...) fut,
    `civis::scripts_post_containers_runs_outputs` = function(...) NULL,
    `civis::write_civis_file` = function(...) NULL,
    eval(r_remote_eval),
    unlink('r-object.rds'),
    return(res)
  )
}

test_that("r eval works", {
  fut <- CivisFuture(quote(2 + 3))
  output <- capture.output(res <- mock_r_eval(fut))
  expect_equal(res, 5)

  a <- 5
  fut <- CivisFuture(quote(a + 1))
  output <- capture.output(res <- mock_r_eval(fut))
  expect_equal(res, 6)

  library(jsonlite)
  fut <- CivisFuture(toJSON(1:2))
  output <- capture.output(res <- mock_r_eval(fut))
  expect_equal(res, toJSON(1:2))

  f <- function(x) g(x)
  g <- function(x) x
  fut <- CivisFuture({f(1)})
  o <- capture.output(res <- mock_r_eval(fut))
  expect_equal(res, 1)
})

mock_run <- function(expr) {
  fut <- CivisFuture(expr)
  with_mock(
    `civis::write_civis_file` = function(...) 123,
    `civis::upload_runner_script` = function(...) 1,
    `civis::scripts_post_containers` = function(...) NULL,
    `civis::scripts_post_containers_runs` = function(fut) NULL,
    `civis::scripts_post_containers_runs_outputs` = function(...) NULL,
    `civis::scripts_get_containers_runs` = function(...) list(state = "succeeded"),
    `civis::read_civis` = function(...) mock_r_eval(fut),
    `civis::fetch_logs` = function(...) list("a log"),
    list(fut = run(fut), value = future::value(fut))
  )
}

test_that("run and value work", {
  out <- capture.output(res <- mock_run(quote(2 + 3)))
  expect_equal(res$value, 5)
  expect_equal(res$fut$logs, list("a log"))
  expect_equal(res$fut$state, "succeeded")
  # shouldn't need to be mocked
  expect_equal(value(res$fut), res$val)
})

mock_err <- function(expr) {
  fut <- CivisFuture(expr)
  with_mock(
    `civis::write_civis_file` = function(...) 123,
    `civis::upload_runner_script` = function(...) 1,
    `civis::scripts_post_containers` = function(...) NULL,
    `civis::scripts_post_containers_runs` = function(...) list(containerId = 1, id = 2),
    `civis::scripts_post_containers_runs_outputs` = function(...) NULL,
    `civis::scripts_get_containers_runs` = function(id, run_id) list(state = "failed"),
    `civis::fetch_output` = function(...) NULL,
    `civis::fetch_logs` = function(...) list("error_log"),
    list(fut = run(fut), value = future::value(fut))
  )
}

test_that("run and value handle errors", {
  e <- tryCatch(mock_err(quote(2 + 3)), error = function(e) e)
  expect_is(e, "civis_error")
  msg <- "scripts_get_containers_runs(id = 1, run_id = 2): \nerror_log"
  expect_equal(e$message, msg)
  expect_equal(attributes(e)$args, list(id = 1, run_id = 2))
})

test_that("CivisFuture has the right stuff", {
 fut <- CivisFuture(quote(2 + 2))
 expect_identical(fut$expr, quote(2 + 2))

 a <- 5
 fut <- CivisFuture(quote(a + 3))
 expect_equal(fut$envir$a, 5)

 library(jsonlite)
 fut <- CivisFuture(quote(toJSON(1:2)))
 expect_equal(fut$packages, "jsonlite")

 # two other quick checks
 expect_equal(fut$docker_image_name, "civisanalytics/datascience-r")
 expect_false(fut$lazy)
})

test_that("resolved", {
  with_mock(
    `civis::scripts_get_containers_runs` = function(...) list(state = "running"),
    fut <- CivisFuture(quote(2 + 2)),
    expect_false(resolved(fut))
  )
})

test_that("cancel", {
  fut <- CivisFuture(quote(2 + 2))
  with_mock(
    `scripts_delete_containers_runs` = function(...) NULL,
    cancel(fut)
  )
  expect_equal(fut$state, "cancelled")
})
