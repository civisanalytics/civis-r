library(civis)
context("URL template format")

test_that("fill_text_template returns the same string with no subs", {
  url <- "/scripts"
  vars <- list(user_id = 123, other_id = 456)
  filled_url <- fill_text_template(url, vars)

  expect_equal(filled_url, filled_url)
})

test_that("fill_text_template fills in ids and such", {
  url <- "/scripts/{id}/runs/{run_id}"
  vars <- list(id = 123, run_id = 456)
  filled_url <- fill_text_template(url, vars)

  expect_equal(filled_url, "/scripts/123/runs/456")
})
