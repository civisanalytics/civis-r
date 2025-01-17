library(civis)
context("rstudio")

test_that("publish_addin errors nicely when no path is present", {
  mock_context <- list(path = "", id = "")  # path is empty here
  mockery::stub(publish_addin, "utils::browseURL", NULL)
  local_mocked_bindings(
    getSourceEditorContext = function(...) mock_context,
    hasFun = function(...) FALSE,
    documentSave = function(...) FALSE,
    # It seems reasonable to mock the functions in the rstudioapi package here
    # since they shouldn't be called by other packages.
    # See https://testthat.r-lib.org/reference/local_mocked_bindings.html#namespaced-calls
    .package = "rstudioapi"
  )
  local_mocked_bindings(publish_rmd = function(...) 123)
  expect_error(publish_addin(), "Please save file before publishing.")
})

test_that("publish_addin errors nicely when path is not RMarkdown", {
  mock_context <- list(path="not_rmd.txt", id="")  # path is not .rmd here
  mockery::stub(publish_addin, "utils::browseURL", NULL)
  local_mocked_bindings(
    getSourceEditorContext = function(...) mock_context,
    hasFun = function(...) FALSE,
    documentSave = function(...) FALSE,
    .package = "rstudioapi"
  )
  local_mocked_bindings(publish_rmd = function(...) 123)
  expect_error(publish_addin(), "Only RMarkdown files.*")
})

test_that("publish_addin opens the correct url", {
  # test_that 2.0 cannot mock base R functions, use mockery instead
  mock_browseurl <- mockery::mock()
  mock_context <- list(path="fake.Rmd", id="")
  mockery::stub(publish_addin, "utils::browseURL", mock_browseurl)
  url <- "https://platform.civisanalytics.com/#/reports/123?fullscreen=true"
  local_mocked_bindings(
    getSourceEditorContext = function(...) mock_context,
    hasFun = function(...) FALSE,
    documentSave = function(...) FALSE,
    .package = "rstudioapi"
  )
  local_mocked_bindings(publish_rmd = function(...) 123)
  publish_addin()
  expect_args(mock_browseurl, 1, url)
})
