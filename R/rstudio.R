#' Add in to Publish an R Markdown file to Platform Reports from RStudio
#'
#' This function is called when the "Publish To Civis" button is pressed in
#' the RStudio IDE AddIn menu. It will attempt to save the current changes
#' of the active document and push it to the appropriate location in Platform.
#' RMarkdown files will be rendered to html with \code{\link{publish_rmd}}
#' and published as a report. Currently only RMarkdown files are supported.
#'
#' @seealso \code{\link{publish_rmd}} for publishing RMarkdown files
publish_addin <- function() {
  context <- rstudioapi::getSourceEditorContext()
  path <- context[["path"]]
  if (path == "") stop("Please save file before publishing.")
  if (tolower(tools::file_ext(path)) != "rmd") {
    stop("Only RMarkdown files with the .Rmd extension are supported.")
  }

  # documentSave only available in Rstudio >= 1.1.287
  if (rstudioapi::hasFun("documentSave")) {
    rstudioapi::documentSave(context$id)
  }

  report_id <- publish_rmd(rmd_file = path)
  url <- paste0("https://platform.civisanalytics.com/#/reports/",
                report_id,
                "?fullscreen=true")
  utils::browseURL(url)
}
