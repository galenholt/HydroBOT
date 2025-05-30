% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default_agg_funs.R
\name{Median}
\alias{Median}
\title{Median}
\usage{
Median(x, na.rm = TRUE)
}
\arguments{
\item{x}{values to aggregate}

\item{na.rm}{default \code{na.rm = TRUE} finds the aggregation ignoring NA, opposite usual na.rm default \code{na.rm = FALSE}, which returns NA if there are any NA.}
}
\value{
summarised data
}
\description{
Finds the median of the values. Lightweight wrapper of \code{medain} with \code{na.rm = TRUE} to match other functions.
}
