library(civis)
library(mockery)
context("reports")

test_that("rmd_file overwritting warning is called", {
  warn_msg <- "parameter 'input' is ignored. Using 'rmd_file' instead."
  with_mocked_bindings(
    `rmarkdown::render` = function(...) TRUE,
    `civis::publish_html` = function(...) -999,
    `civis::parse_front_matter` = function(...) list(),
    expect_warning(publish_rmd("fake.Rmd", input = "duplicate"), warn_msg)
  )
})

test_that("output_file overrides temp file when passed.", {
  output_file <- "keep_me.html"
  mock_publish_html <- mockery::mock(NULL)
  with_mocked_bindings(
    `civis::publish_html` = mock_publish_html,
    `rmarkdown::render` = function(...) TRUE,
    `civis::parse_front_matter` = function(...) list(),
    publish_rmd("fake.Rmd", output_file = output_file)
  )
  publish_html_args <- mockery::mock_args(mock_publish_html)[[1]]
  expect_equal(publish_html_args[[1]], output_file)
})

test_that("publish_html calls reports_put_project", {
  mock_reports_put_projects <- mockery::mock(reports_put_projects)
  with_mocked_bindings(
    `readChar` = function(...) TRUE,
    `civis::reports_post` = function(...) list(id = "fake_report"),
    `civis::reports_put_projects` = mock_reports_put_projects,
    publish_html("fake.html", project_id = "project_1")
  )
  expect_args(mock_reports_put_projects, 1, id = "fake_report", project_id = "project_1")
})

test_that("publish_html does not call reports_put_projects", {
  with_mocked_bindings(
    `readChar` = function(...) TRUE,
    `civis::reports_post` = function(...) list(id = "fake_report"),
    `civis::reports_put_projects` = function(id, project_id) stop("should not be called"),
    expect_silent(publish_html("fake.html"))
  )
})

test_that("publish_html can put an existing report", {
  with_mocked_bindings(
    `readChar` = function(...) TRUE,
    `civis::reports_patch` = function(report_id, ...) list(id = report_id),
    `civis::reports_post` = function(...) stop("should not be called"),
    expect_equal(publish_html("fake_html", report_id = 123), 123)
  )
})

test_that("publish_rmd passes project_id", {
  mock_publish_html <- mockery::mock(NULL, cycle=TRUE)
  with_mocked_bindings(
    `civis::publish_html` = mock_publish_html,
    `rmarkdown::render` = function(...) TRUE,
    `civis::parse_front_matter` = function(...) list(),
    publish_rmd("rmd", project_id = "abc123"),
    publish_rmd("rmd")
  )
  publish_html_args <- mockery::mock_args(mock_publish_html)
  expect_equal("abc123", publish_html_args[[1]][["project_id"]])
  expect_null(publish_html_args[[2]][["project_id"]])
})

test_that("publish_rmd passes report_id", {
  mock_publish_html <- mockery::mock(NULL, cycle=TRUE)
  with_mocked_bindings(
    `civis::publish_html` = mock_publish_html,
    `rmarkdown::render` = function(...) TRUE,
    `civis::parse_front_matter` = function(...) list(),
    publish_rmd("rmd", report_id = 123),
    publish_rmd("rmd")
  )
  publish_html_args <- mockery::mock_args(mock_publish_html)
  expect_equal(123, publish_html_args[[1]][["report_id"]])
  expect_null(publish_html_args[[2]][["report_id"]])
})

test_that("parse_front_matter parses yaml correctly", {
    rmd_a <- "---\ncivis:\n  a: 1\n  b: 100\n---\n R code here.\n"
    rmd_b <- "RMD with no front matter.\n"
    expect_a <- list(civis = list(a = 1, b = 100))
    expect_b <- list()
    expect_equal(parse_front_matter(textConnection(rmd_a)), expect_a)
    expect_equal(parse_front_matter(textConnection(rmd_b)), expect_b)
})

test_that("parse_front_matter handles parsing errors with warning", {
    rmd_a <- "---\ncivis\n -bad_yaml:\n---\n R code here.\n"
    warn <- paste("Failed to parse Civis metadata from Rmarkdown file:",
                  "Scanner error: mapping values are not allowed in this",
                  "context at line 2, column 11")
    expect_warning(parse_front_matter(textConnection(rmd_a)), warn)
})
