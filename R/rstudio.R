#' Add in to Publish an R Markdown file to Platform Reports from RStudio
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
  browseURL(url)
}
