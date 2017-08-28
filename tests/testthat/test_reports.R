library(civis)
library(mockery)
context("reports")

test_that("rmd_file overwritting warning is called", {
  warn_msg <- "parameter 'input' is ignored. Using 'rmd_file' instead."
  with_mock(
    `rmarkdown::render` = function(...) TRUE,
    `civis::publish_html` = function(...) -999,
    expect_warning(publish_rmd("fake.Rmd", input = "duplicate"), warn_msg)
  )
})

test_that("output_file overrides temp file when passed.", {
  output_file <- "keep_me.html"
  mock_publish_html <- mockery::mock(NULL)
  with_mock(
    `civis::publish_html` = mock_publish_html,
    `rmarkdown::render` = function(...) TRUE,
    publish_rmd("fake.Rmd", output_file = output_file)
  )
  publish_html_args <- mockery::mock_args(mock_publish_html)[[1]]
  expect_equal(publish_html_args[[1]], output_file)
})

test_that("publish_rmd passes project_id", {
  with_mock(
    `civis::publish_html` = function(project_id, ...) cat(project_id),
    `rmarkdown::render` = function(...) TRUE,
    expect_output(publish_rmd("rmd", project_id = "abc123"), "abc123"),
    expect_output(publish_rmd("rmd"), NA)
  )
})

test_that("publish_html calls reports_put_project", {
  mock_reports_put_projects <- mockery::mock(reports_put_projects)
  with_mock(
    `readChar` = function(...) TRUE,
    `civis::reports_post` = function(...) list(id = "fake_report"),
    `civis::reports_put_projects` = mock_reports_put_projects,
    publish_html("fake.html", project_id = "project_1")
  )
  expect_args(mock_reports_put_projects, 1, id = "fake_report", project_id = "project_1")
})

test_that("publish_html does not call reports_put_projects", {
  with_mock(
    `readChar` = function(...) TRUE,
    `civis::reports_post` = function(...) list(id = "fake_report"),
    `civis::reports_put_projects` = function(id, project_id) stop("should not be called"),
    expect_silent(publish_html("fake.html"))
  )
})

test_that("publish_html can put an existing report", {
  with_mock(
    `readChar` = function(...) TRUE,
    `civis::reports_patch` = function(report_id, ...) list(id = report_id),
    `civis::reports_post` = function(...) stop("should not be called"),
    expect_equal(publish_html("fake_html", report_id = 123), 123)
  )
})

test_that("publish_rmd passes report_id", {
  with_mock(
    `civis::publish_html` = function(html_file, report_id, ...) report_id,
    `rmarkdown::render` = function(...) TRUE,
    expect_equal(publish_rmd("rmd", report_id = 123), 123),
    expect_null(publish_rmd("rmd"))
  )
})
