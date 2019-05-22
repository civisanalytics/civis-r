library(civis)
context("client base")

###############################################################################
# Set up
###############################################################################

# http://jsonplaceholder.typicode.com/users
httr_200 <- readRDS("data/httr_200_response.rds")

# http://httpbin.org/status/504
httr_504 <- readRDS("data/httr_504_response.rds")

# http://getstatuscode.com/204"
httr_204 <- readRDS("data/httr_204_response.rds")

# http://getstatuscode.com/205"
httr_205 <- readRDS("data/httr_205_response.rds")

# also from http://getstatuscode.com/"
httr_403 <- readRDS("data/httr_403_response.rds")
httr_429 <- readRDS("data/httr_429_response.rds")

# fake parameters to pass into call_api
path <- "fake_path"
path_params <- c("fake", "params")
query_params <- NULL
body_params <- NULL


###############################################################################
# Tests
###############################################################################

test_that("call api extracts json content correctly", {
  response <- with_mock(
    `civis::api_key` = function(...) "fake_key",
    `httr::VERB` = function(...) httr_200,
    `httr::RETRY` = function(...) httr_200,
    call_api("GET", path, path_params, query_params, body_params))
  expect_equal(response[[1]]$name, "Leanne Graham")
  expect_is(response, "civis_api")
})

test_that("print and str methods for civis_api", {
  response <- with_mock(
    `civis::api_key` = function(...) "fake_key",
    `httr::VERB` = function(...) httr_200,
    `httr::RETRY` = function(...) httr_200,
    call_api("GET", path, path_params, query_params, body_params))

  # print hides attributes
  expect_false(grepl("attribute", capture_output(print(response))))

  # str shows attributes and structure
  resp_str <- capture_output(str(response, 1))
  expect_match(resp_str, "attr")
  expect_match(resp_str, "List of 8")
  expect_match(resp_str, "List of 10")
})

test_that("call api catches http error", {
  with_mock(
    `civis::api_key` = function(...) "fake_key",
    `httr::VERB` = function(...) httr_504,
    `httr::RETRY` = function(...) httr_504,
    expect_error(
      call_api("GET", path, path_params, query_params, body_params),
      "Gateway Timeout \\(HTTP 504\\). Failed to GET httpbin.org/status/504."
    )
  )
})

test_that("call api retries get/put requests, but not post requests", {
  with_mock(
    `civis::api_key` = function(...) "fake_key",
    `httr::VERB` = function(...) httr_504,
    `httr::RETRY` = function(...) httr_200,
    expect_silent(call_api("GET", path, path_params, query_params, body_params)),
    expect_silent(call_api("PUT", path, path_params, query_params, body_params)),
    expect_error(
      call_api("POST", path, path_params, query_params, body_params),
      "Gateway Timeout \\(HTTP 504\\). Failed to GET httpbin.org/status/504."
    )
  )
})

test_that("stop_for_status concatenates Platform specific errors", {
  error <- "Gateway Timeout \\(HTTP 504\\). hi from platform!"
  with_mock(
    # Mocks the response so it includes an error like those in Platform
    `httr::content` = function(...) list(errorDescription = "hi from platform!"),
    expect_error(stop_for_status(httr_504), error)
  )
})

test_that("204, 205 responses return NULL", {
  response <- with_mock(
      `civis::api_key` = function(...) "fake_key",
      `httr::VERB` = function(...) httr_204,
      call_api("POST", path, path_params, query_params, body_params))
  expect_null(response$content)
  expect_is(response, "civis_api")

  response <- with_mock(
      `civis::api_key` = function(...) "fake_key",
      `httr::RETRY` = function(...) httr_205,
      call_api("GET", path, path_params, query_params, body_params))
  expect_null(response$content)
  expect_is(response, "civis_api")

})

test_that("no retry on GET/PUT and code 403", {
  for (verb in c("GET", "PUT")) {
      mock_rp <- mock(httr_403, cycle = TRUE)
      with_mock(
        `civis::api_key` = function(...) "fake_key",
        `httr:::request_perform` = mock_rp,
        expect_error(call_api(verb, path, path_params, query_params, body_params),
                     paste0(httr_403$status_code)),
        expect_called(mock_rp, 1))
    }
})

test_that("retry on GET/PUT and 429", {
  mock_rp <- mock(httr_429, cycle = TRUE)
  with_mock(
    `civis::api_key` = function(...) "fake_key",
    `httr:::request_perform` = mock_rp,
    `httr:::backoff_full_jitter` = function(...) Sys.sleep(0),
    expect_error(call_api("GET", path, path_params, query_params, body_params),
                 paste0(httr_429$status_code)),
    expect_called(mock_rp, 3))
})

test_that("failing to parse JSON content returns CivisClientError", {
  error <- 'Unable to parse JSON from response'
  with_mock(
    `civis::api_key` = function(...) "fake_key",
    `httr::VERB` = function(...) httr_200,
    `httr::RETRY` = function(...) httr_200,

    # This simulates httr::content failing with an arbitrary error/message
    `httr::content` = function(...) stop("httr failed to parse response, throwing an error!"),

    # Tests for the right error message (same as python client)
    expect_error(call_api("GET", path, path_params, query_params, body_params), error),

    # Tests that the error is a CivisClientError (same as python client) that is try-catchable
    expect_true(
      tryCatch(call_api("GET", path, path_params, query_params, body_params),
             civis_api_error = function(c) TRUE,
             error = function(e) FALSE)
    ))
})

