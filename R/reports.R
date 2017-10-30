#' Publish an R Markdown file to Platform Reports
#'
#' @param rmd_file string, R Markdown file (.Rmd)
#' @param report_id integer, ID of an existing report, if provided, the
#' contents of the report will be replaced. If \code{report_id} is NULL, a new
#' report will be created.
#' @param report_name string, Title of report in platform
#' @param provide_api_key bool, Set to true to include API key in report.
#' @param project_id integer, Project_id that the report should be added to.
#' @param ... additional parameters to send to \code{rmarkdown::render}. Note:
#' A temporary file will be used for \code{output_file} if \code{output_file}
#' is not explicity set and \code{input} will be overwritten with
#' \code{rmd_file}.
#'
#' @note
#' \code{rmarkdown::render} depends on a recent version of \code{pandoc}.
#' \code{pandoc} is distributed with RStudio and thus, \code{publish_rmd}
#' will work in an RStudio environment. Outside of RStudio \code{pandoc} may
#' be installed or \code{knitr::knit2html} can be used to convert an R
#' Markdown document into html. The html can then be published to Civis
#' with \code{publish_html}.
#'
#' @seealso \code{\link{publish_html}} to publish html to Civis
#'
#' @examples
#' \dontrun{
#'
#' # Publish a standard report
#' publish_rmd("my_beautiful_doc.Rmd")
#'
#' # Publish a parameterized R Markdown document
#' # See: http://rmarkdown.rstudio.com/developer_parameterized_reports.html
#' params <- list("region" = "east", start = as.Date("2015-02-01"))
#' publish_rmd("my_parameterized_doc.Rmd", params=params)
#' }
#' @export
publish_rmd <- function(rmd_file, report_id=NULL, report_name=NULL,
                        provide_api_key=NULL, project_id=NULL, ...) {
  render_args <- list(...)

  # Ensure "rmd_file" is used over "input" in "..."
  if (!is.null(render_args[["input"]])) {
    warning("parameter 'input' is ignored. Using 'rmd_file' instead.")
  }
  render_args[["input"]] <- rmd_file

  tryCatch({
    temp_output <- tempfile(fileext = ".html")
    if (is.null(render_args[["output_file"]])) {
      render_args[["output_file"]] <- temp_output
    }
    do.call(rmarkdown::render, render_args)
    publish_html(render_args[["output_file"]],
                 report_id, report_name,
                 provide_api_key = provide_api_key,
                 project_id = project_id)
  }, finally = {
    unlink(temp_output)
  })
}


#' Publish HTML to Platform Reports
#'
#' @param html_file string, HTML file
#' @param report_id integer, ID of an existing report, if provided, the
#' contents of the report will be replaced. If \code{report_id} is NULL, a new
#' report will be created.
#' @param report_name string, Title of report in platform
#' @param provide_api_key bool, Set to true to include API key in report.
#' @param project_id integer, Project_id that the report should be added to.
#'
#' @seealso \code{\link{publish_rmd}} to publish an R Markdown document
#' to Civis
#'
#' @examples
#' \dontrun{
#' # Uploads html and prints link to report in Platform
#' publish_html("my_beautiful_report.html")
#' }
#' @export
publish_html <- function(html_file, report_id=NULL, report_name=NULL,
                         provide_api_key=NULL, project_id=NULL) {
  html <- readChar(html_file, file.info(html_file)$size)
  html <- wrap_in_scrolling_div(html)

  if (is.null(report_id)) {
    r <- reports_post(code_body = html, name = report_name,
                      provide_api_key = provide_api_key)
  } else {
    r <- reports_patch(report_id, code_body = html, name = report_name,
                       provide_api_key = provide_api_key)
  }

  report_id <- r[["id"]]
  if (!is.null(project_id)) {
    reports_put_projects(report_id, project_id)
  }
  report_id
}


# Wrap div to enable scrolling
wrap_in_scrolling_div <- function(html) {
  head <- '<div style="overflow-y:scroll;height:100%">'
  tail <- '</div>'
  paste0(head, "\n", html, "\n", tail)
}
