% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation_helpers.R
\name{identify_dimension}
\alias{identify_dimension}
\title{Helper to identify the dimension of each step in aggsequence}
\usage{
identify_dimension(aggsequence, causal_edges)
}
\arguments{
\item{aggsequence}{a named list of aggregation steps in the order to apply
them. Entries for theme aggregation should be character vectors- e.g. \code{name = c('from_theme', 'to_theme')}. Entries for spatial aggregation should be
the sf polygon to aggregate to, e.g. \code{name = sfpolygons} or a length-1
character, e.g. \code{name = "sfpolygons"}. The latter requires the object to be
available with \code{get("sfpolygons")}, but allows passing characters rather
than objects. Not requiring names and is high on the list of improvements.
If we want to be able to re-run from auto-saved metadata params, we need
the names of the spatial levels to match the object, e.g. basin: basin.}

\item{causal_edges}{causal links between all theme levels included in
\code{aggsequence}, though can also include others, which are ignored. Creates
the theme grouping}
}
\value{
character vector with 'spatial', 'theme' or 'temporal' for each step
in aggsequence
}
\description{
Helper to identify the dimension of each step in aggsequence
}
