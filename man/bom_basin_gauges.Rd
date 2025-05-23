% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{bom_basin_gauges}
\alias{bom_basin_gauges}
\title{Gauge locations}
\format{
\subsection{\code{bom_basin_gauges} An \code{sf} object:}{

\describe{
\item{site, gauge}{Name and code for gauge}
\item{owner}{State from which data is available}
}
}
}
\source{
CSV within \url{https://pypi.org/project/mdba-gauge-getter/}
}
\usage{
bom_basin_gauges
}
\description{
Points specifying the locations of gauging stations. Sourced from \href{https://pypi.org/project/mdba-gauge-getter/}{MDBA gauge getter}, though a very similar
dataset (HydrologicIndicatorSites) is available from the \href{https://www.mdba.gov.au/publications-and-data/maps-and-spatial-data/spatial-data}{MDBA data website}.
The BOM locations are used here for consistency with the EWR tool. Full list
of gauges has been clipped to the \code{basin} polygon.
}
\keyword{datasets}
