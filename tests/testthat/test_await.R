context("await")

test_that("await captures success_state from status key", {
  f <- function(x) {
    if (x == 0) list(state = "success", y = 2) else list(state = "fail", y = NULL)
  }
  expect_silent(r <- await(f, x = 0, .status_key = "state",
                           .success_states = c("success", "fail")))

  expect_equal(r$state, "success")
  expect_equal(get_status(r), "success")
  expect_equal(r$y, 2)

  r <- await(f, x = 1, .status_key = "state", .success_states = c("success", "fail"))
  expect_equal(r$state, "fail")
  expect_equal(get_status(r), "fail")
  expect_null(r$y)
})

test_that("await calls f until terminal status", {
  fake_f <- mockery::mock(list(status = "running"),
                 list(status = "running"),
                 list(status = "succeeded"))
  await(fake_f, .status_key = "status", .interval = .001)
  mockery::expect_called(fake_f, 3)
})

test_that("await errors on timeout or if status_key not found", {
  msg <- "Timeout exceeded. Current status: running"
  f <- function(x) list(state = "running")
  expect_error(await(f, x = 1, .status_key = "state",
                     .success_states = c("this never occurs"),
                     .timeout = .001, .interval = .001), msg)

  f <- function(x) list(x)
  msg <- "Cannot find status"
  expect_error(await(f, x = 1), msg)
})

test_that("await throws a civis_await_error with failure state", {
  g <- function(x) {
    if (x == 0) {
      list(state = "success", y = 2)
    } else {
      list(state = "fail", y = NULL, error = "this failed on platform")
    }
  }

  msg <- "g\\(x = 1\\): this failed on platform"
  expect_error(await(g, x = 1, .success_states = "success", .error_states = "fail"), msg)

  e <- tryCatch(await(g, x = 1, .success_states = "success", .error_states = "fail"),
           "civis_await_error" = function(e) e)
  expect_is(e, c("civis_await_error", "civis_error", "error"))
  msg <- "g(x = 1): this failed on platform"
  expect_equal(e$message, msg)

  e2 <- tryCatch(await(g, x = 1, .success_states = "success", .error_states = "fail"),
                "civis_error" = function(e) e)
  expect_equal(e, e2)
})

test_that("await throws a civis_timeout_error on timeout", {
  f <- function(x) list(state = "running")

  msg <- "Timeout exceeded. Current status: running"
  expect_error(await(f, x = 0, .timeout = .001, .interval = .001), msg)

  e <- tryCatch(await(f, x = 0, .timeout = .001, .interval = .001),
                "civis_timeout_error" = function(e) e)
  expect_is(e, c("civis_await_error", "civis_error", "error"))
  msg <- "Timeout exceeded. Current status: running"
  expect_equal(e$message, msg)

  e2 <- tryCatch(await(f, x = 0, .timeout = .001, .interval = .001),
                "civis_error" = function(e) e)
  expect_equal(e, e2)
})

test_that("get_error returns error data for civis_error", {
  f <- function(x) {
    if (x == 0) {
      list(state = "running", y = 2, error = NULL)
    } else {
      list(state = "fail", y = NULL, error = "this failed on platform")
    }
  }

  e <- tryCatch(await(f, x = 1, .success_states = "success", .error_states = "fail"),
                "civis_error" = function(e) e)
  e_data <- get_error(e)
  expect_equal(e_data$args, list(x = 1))
  expect_equal(e_data$f, "f")
  expect_equal(e_data$error, "this failed on platform")

  e <- tryCatch(await(f, x = 0, .timeout = .001, .error_states = "fail"),
                "civis_error" = function(e) e)
  e_data <- get_error(e)
  expect_equal(e_data$args, list(x = 0))
  expect_equal(e_data$f, "f")
  expect_null(e_data$error)
})

test_that("verbose produces correct messages", {
  f <- function(x) {
    x <- runif(1)
    if (x > .5) {
      return(list(state = "running", x = NULL))
    } else {
      return(list(state = "success", x = x))
    }
  }
  set.seed(4)
  msg <- capture_messages(await(f, x = 1, .status_key = "state",
                                .success_states = c("success", "fail"),
             .verbose = TRUE))
  patterns <- c("Status: running", "Retry 1 in 0.302 seconds")
  expect_true(all(stringr::str_detect(msg, patterns)))

  set.seed(4)
  msg <- capture_messages(await(f, x = 1, .status_key = "state",
                                .success_states = c("success", "fail"),
                                .verbose = TRUE, .interval = .01))
  patterns <- c("Status: running", "Retry 1 in 0.01 seconds")
  expect_true(all(stringr::str_detect(msg, patterns)))
})

test_that("call_once returns list with element called", {
  f <- function(id) return(list(state = "succeeded"))
  expect_true(call_once(f, .id = 1, fname = "f")$called)

  f <- function(id) return(list(state = "partying instead"))
  expect_false(call_once(f, .id = 1, fname = "f")$called)
})

test_that("if called, get_status(r$response) returns status", {
  f <- function(id) return(list(state = "succeeded"))
  expect_equal(get_status(call_once(f, .id = 1, fname = "f")$response), "succeeded")
})

test_that("call_once captures completed_states and status_keys", {
  f <- function(id) return(list(party_status = "at the party"))
  expect_true(call_once(f, .id = 1, .success_states = "at the party",
                        .status_key = "party_status",  fname = "f")$called)
})

test_that("safe_call_once catches civis_await_error", {
  f <- function(x) list(state = "succeeded", x = x)
  r <- safe_call_once(f, x = 1, fname = "f")
  expect_true(r$called)
  expect_equal(r$response$x, 1)
  expect_equal(get_status(r$response), "succeeded")

  f <- function(x) list(state = "failed", error = "platform error")
  e <- safe_call_once(f, x = 1, fname = "f")
  expect_is(e, c("civis_await_error", "civis_error", "error"))
})

f_rand <- function(job_id, ...) {
  x <- runif(1)
  if (x < .9) {
    return(list(state = "succeeded", job_id = job_id, args = list(...)))
  } else {
    return(list(state = "partying instead", job_id = job_id, args = list(...)))
  }
}

test_that("await_all calls f until completion", {
  fake_f <- mockery::mock(list(status = "running"),
                 list(status = "running"),
                 list(status = "succeeded"), cycle = TRUE)
  await_all(fake_f, .x = 1:2, .y = 3:4, .status_key = "status", .interval = .001)
  mockery::expect_called(fake_f, 6)
})

test_that("await_all returns list of completed responses", {
  set.seed(2)
  x <- await_all(f_rand, .x = 1:2, .y = 3:4, job_id = 1)
  expect_is(x, "list")
  expect_equal(sapply(x, get_status), rep("succeeded", 2))
  expect_equal(sapply(x, function(x) x$args), list(c(1, 3), c(2, 4)))
  expect_equal(sapply(x, function(x) x$job_id), rep(1, 2))
})

# test_that("await_all vectorizes over any argument", {
#   x <- await_all(f_rand, .x = 1:2, .y = 3:4, id = 1)
#   expect_equal(sapply(x, function(x) x$job_id), 1:2)
#   expect_equal(sapply(x, function(x) x$run_id), 1:2)
# })

test_that("await_all catches arbitrary status and keys", {
  f <- function(x, y) list(party_status = "going home", value = x)
  x <- await_all(f, .x = 1:2, .y = 3:4, .status_key = "party_status",
                 .success_states = "going home", .verbose = TRUE)
  expect_equal(sapply(x, get_status), rep("going home", 2))
  expect_equal(sapply(x, function(x) x$value), cbind(c(1, 3), c(2, 4)))
})

test_that("await_all throws civis_timeout_error", {
  f <- function(x) list(state = "at the party")
  msg <- c("Timeout exceeded. Current status: at the party, at the party")
  expect_error(await_all(f, .x = 1:2, .y = 3:4, .timeout = .002, .interval = .001), msg)

  e <- tryCatch(await_all(f, .x = 1:2, .y = 3:4, .timeout = .002, .interval = .001),
           "civis_timeout_error" = function(e) e)

  get_error(e)

  e2 <- tryCatch(await_all(f, .x = 1:2, .y = 3:4, .timeout = .002, .interval = .001),
                "civis_error" = function(e) e)
  expect_equal(e, e2)
})

test_that("await_all catches mixed failure states", {
  f <- function(x, y) {
    switch(x, list(state = "succeeded"),
           list(state = "failed", error = "platform error"),
           list(state = "succeeded"))
  }
  r <- await_all(f, .x = 1:2)
  expect_true(is.civis_error(r[[2]]))
  expect_equal(get_error(r[[2]])$error, "platform error")
  expect_equal(get_error(r[[2]])$args, list(x = 2))
})

test_that("await_all verbose prints all tasks and status", {
  set.seed(2)
  msgs <- capture_messages(await_all(f_rand, .x = 1:5, job_id = 1, .verbose = TRUE))
  expect_true(any(grepl("partying instead", x = msgs)))
  expect_equal(length(msgs), 5)
})

test_that("await_all throws an error if lengths of .x and .y differ", {
  expect_error(await_all(f, .x = 1:2, .y = 3:5))
})


