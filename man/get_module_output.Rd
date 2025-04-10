% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_module_output.R
\name{get_module_output}
\alias{get_module_output}
\title{Get the outputs from modules and do minor cleaning}
\usage{
get_module_output(dir, type, gaugefilter = NULL, scenariofilter = NULL)
}
\arguments{
\item{dir}{path to directory with the EWR output for all gauges and scenarios}

\item{type}{character, a grep for the files to choose. Two special cases:
\itemize{
\item 'achievement', calculates EWR achievement from 'yearly',
\item 'everything', gets all files
For the EWR tool, the direct options are
\item 'summary',
\item 'yearly',
\item 'all_events',
\item 'all_successful_events',
\item 'all_interEvents',
\item 'all_successful_interEvents'
}}

\item{gaugefilter}{character vector of gauge numbers to include. Default \code{NULL} includes all}

\item{scenariofilter}{character vector of scenario names to include. Default \code{NULL} includes all}
}
\value{
a tibble of module results
}
\description{
\emph{NOTE} Individual modules may need further bespoke processing, see \code{\link[=prep_ewr_output]{prep_ewr_output()}}.
}
