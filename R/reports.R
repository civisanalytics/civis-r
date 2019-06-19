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
#' is not explicitly set and \code{input} will be overwritten with
#' \code{rmd_file}.
#'
#' @details
#' This function also supports passing \code{report_id}, \code{report_name},
#' \code{provide_api_key} and \code{project_id} as metadata in the report's
#' YAML front matter.  Just as the title of an RMarkdown document can be set
#' with \code{title: "my title!"}, these parameters can be set like
#' \preformatted{
#'   civis:
#'     report_name: "My Report Name"
#'     report_id: 9000
#' }
#' Since \code{report_id} is set, this code will overwrite the existing
#' report with that number, which may be useful when updating a report on
#' a schedule.  Any argument passed in explicitly to \code{publish_rmd}
#' will be used in place of the corresponding argument set in YAML metadata.
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

  # Look for publish_html args in the yaml of the RMarkdown file, overwriting
  # the arg if it is explicitly passed into publish_rmd
  html_args <- parse_front_matter(rmd_file)[["civis"]]
  html_args <- if (is.null(html_args)) list() else html_args
  if (!missing(report_id)) html_args[["report_id"]] <- report_id
  if (!missing(report_name)) html_args[["report_name"]] <- report_name
  if (!missing(project_id)) html_args[["project_id"]] <- project_id
  if (!missing(provide_api_key)) {
    html_args[["provide_api_key"]] <- provide_api_key
  }

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
    html_args[["html_file"]] <- render_args[["output_file"]]
    do.call(rmarkdown::render, render_args)
    do.call(publish_html, html_args)
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


# Simple RMarkdown YAML front matter parser. For a more robust parser,
# see rmarkdown::render.
parse_front_matter <- function(rmd_file) {
  metadata <- tryCatch({
    rmd_lines <- readLines(rmd_file)
    delim <- grep("^---\\s*$", rmd_lines)
    start <- delim[1] + 1  # First yaml line is line after first ---
    end <- delim[2] - 1 # Last yaml line is line before second ---
    if (is.na(start) | is.na(end)) {
      list()
    } else if (end - start < 1) {
      list()
    } else {
      yaml_str <- paste(rmd_lines[start:end], collapse="\n")
      yaml::yaml.load(yaml_str)
    }
  }, error = function(e) {
    msg <- paste("Failed to parse Civis metadata from Rmarkdown file:",
                 e$message)
    warning(msg, call. = FALSE)
    list()
  })
  metadata
}
