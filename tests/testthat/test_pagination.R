library(civis)
context("pagination")

test_that("calls fn the proper number of times", {
  n_calls <- 0
  fn <- function(...) {
    n_calls <<- n_calls + 1
    structure(list(1,2,3), headers = list("x-pagination-total-pages" = "3",
                                          "x-pagination-current-page" = n_calls))
  }

  resp <- fetch_all(fn)
  expect_equal(n_calls, 3)
})

test_that("passes args to fn", {
  fn <- function(...) {
    structure(list(...), headers = list("x-pagination-total-pages" = "1",
                                        "x-pagination-current-page" = "1"))
  }

  called_with <- fetch_all(fn, arg1 = "abc", arg2 = 123)

  expect_named(called_with, c("arg1", "arg2", "page_num"))
  expect_equal(called_with$arg1, "abc")
  expect_equal(called_with$arg2, 123)
})

test_that("fetch_until halts", {
  n_calls <- 0
  responses <- list(1, 2, 3)
  fn <- function(...) {
    n_calls <<- n_calls + 1
    structure(responses[[n_calls]],
              headers = list("x-pagination-total-pages" = "3",
                             "x-pagination-current-page" = n_calls))
  }

  resp <- fetch_until(fn, function(.x) .x == 2)

  expect_equal(n_calls, 2)
  expect_equal(resp, list(1, 2))
})
