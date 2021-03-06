% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generated_client.R
\name{json_values_get}
\alias{json_values_get}
\title{Get details about a JSON Value}
\usage{
json_values_get(id)
}
\arguments{
\item{id}{integer required. The ID of the JSON Value.}
}
\value{
A list containing the following elements:
\item{id}{integer, The ID of the JSON Value.}
\item{name}{string, The name of the JSON Value.}
\item{value}{string, The deserialized JSON value.}
}
\description{
Get details about a JSON Value
}
