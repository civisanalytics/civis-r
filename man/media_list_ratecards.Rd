% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generated_client.R
\name{media_list_ratecards}
\alias{media_list_ratecards}
\title{List all ratecards}
\usage{
media_list_ratecards(archived = NULL, filename = NULL, dma_number = NULL)
}
\arguments{
\item{archived}{string optional. The archival status of the requested item(s).}

\item{filename}{string optional. If specified, will be used to filter the ratecards returned. Substring matching is supported with "\%" and "*" wildcards (e.g., "filename=\%ratecard\%" will return both "ratecard 1" and "my ratecard").}

\item{dma_number}{integer optional. If specified, will be used to filter the ratecards by DMA.}
}
\value{
An array containing the following fields:
\item{id}{integer, The ratecard ID.}
\item{filename}{string, Name of the ratecard file.}
\item{startOn}{string, First day to which the ratecard applies.}
\item{endOn}{string, Last day to which the ratecard applies.}
\item{dmaNumber}{integer, Number of the DMA associated with the ratecard.}
\item{archived}{string, The archival status of the requested item(s).}
}
\description{
List all ratecards
}
