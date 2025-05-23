% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation_helpers.R
\name{agg_names_to_cols}
\alias{agg_names_to_cols}
\title{Change aggregation history in col names to stepwise columns}
\usage{
agg_names_to_cols(aggdf, aggsequence, funsequence, aggCols)
}
\arguments{
\item{aggdf}{aggregated dataframe or tibble}

\item{aggsequence}{a list of the names of the aggregation. works best if the
list passed to the calling function is named, and those names are passed in
here.Typically it will be the second value in each theme aggregation step
(the to-theme), or the name of the spatial polygons. But extracting those
when unnamed isn't well tested.}

\item{funsequence}{a list of functions (as characters or lists of names and
arguments) to apply to do the aggregation at each step defined in
\code{aggsequence}. Can be multiple funs per aggregation step.}

\item{aggCols}{the name(s) of the original columns that were aggregated.}
}
\value{
a tibble with a column named as the original input data at the start
of aggregation, and columns for every aggregation step giving the level
aggregated into and the function used for the aggregation. if multiple
functions are used, the rows are stacked (long format).
}
\description{
By default, aggregation history (function and level) is tracked in the column
names for memory purposes and to keep units clear. But it's usually more
useful to be able to access that information, so this parses the names of the
aggregated columns into columns for the aggregation function at each
aggregation level.
}
