% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_joiner.R
\name{pseudo_spatial_joiner}
\alias{pseudo_spatial_joiner}
\title{Join two spatial dataframes non-spatially}
\usage{
pseudo_spatial_joiner(from_geo, to_geo, prefix)
}
\arguments{
\item{from_geo}{\code{sf} of the input data to be aggregated}

\item{to_geo}{\code{sf} to aggregate into}

\item{prefix}{from spatial_aggregate, only used in error-checking}
}
\value{
an sf on the scale of to_geo
}
\description{
This is really just a left join that bypasses the geometry, and has some
checks.
}
\keyword{internal}
