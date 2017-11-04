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

  library(purrr)
  fut <- CivisFuture(map(1:2, c))
  output <- capture.output(res <- mock_r_eval(fut))
  expect_equal(res, list(1, 2))
})

mock_run <- function(expr) {
  fut <- CivisFuture(expr)
  with_mock(
    `civis::write_civis_file` = function(...) 123,
    `civis::upload_runner_script` = function(...) "",
    `civis::scripts_post_containers` = function(...) NULL,
    `civis::scripts_post_containers_runs` = function(fut) mock_r_eval(fut),
    `civis::scripts_post_containers_runs_outputs` = function(...) NULL,
    `civis::scripts_get_containers_runs` = function(...) list(state = "succeeded"),
    `civis::fetch_output` = function(...) mock_r_eval(fut),
    value(run(fut))
  )
}

test_that("run and value work", {
  out <- capture.output(res <- mock_run(quote(2 + 3)))
  expect_equal(res, 5)
})

test_that("CivisFuture has the right stuff", {
 fut <- CivisFuture(quote(2 + 2))
 expect_identical(fut$expr, quote(2 + 2))

 a <- 5
 fut <- CivisFuture(quote(a + 3))
 expect_equal(fut$envir$a, 5)

 library(purrr)
 fut <- CivisFuture(quote(map(1:2, c)))
 expect_equal(fut$packages, "purrr")

 # two other quick checks
 expect_equal(fut$docker_image_name, "civisanalytics/datascience-r")
 expect_false(fut$lazy)
})

test_that("resolved", {
  fut <- CivisFuture(quote(2 + 2))
  expect_false(resolved(fut))
})

test_that("cancel", {
  fut <- CivisFuture(quote(2 + 2))
  with_mock(
    `scripts_delete_containers_runs` = function(...) NULL,
    cancel(fut)
  )
  expect_equal(fut$state, "cancelled")
})


