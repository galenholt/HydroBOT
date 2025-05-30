% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default_agg_funs.R
\name{Max}
\alias{Max}
\title{Max aggregation with na.rm = TRUE by default \emph{except} that all-NA vectors still return NA, not 0.}
\usage{
Max(x, na.rm = TRUE)
}
\arguments{
\item{x}{values to aggregate}

\item{na.rm}{default \code{na.rm = TRUE} finds the aggregation ignoring NA, opposite usual na.rm default \code{na.rm = FALSE}, which returns NA if there are any NA.}
}
\value{
numeric
}
\description{
Max aggregation with na.rm = TRUE by default \emph{except} that all-NA vectors still return NA, not 0.
}
