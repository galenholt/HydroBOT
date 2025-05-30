% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{causal_ewr}
\alias{causal_ewr}
\title{Causal relationships for the EWRs}
\format{
\subsection{\code{causal_ewr} A list of dataframes:}{

\describe{
\item{ewr2obj}{Mapping from EWRs at each gauge to environmental objectives (\code{env_obj})}
\item{obj2target}{Mapping from \code{env_obj} to \code{Specific_goal}, \code{Objective}, and \code{Target}, which are all defined at the PlanningUnit scale}
\item{obj2yrtarget}{Mapping defined without spatial reference from \code{env_obj} to \code{Target}, \code{Target_Category}, \code{Objective}, \code{target_5_year_2024}, \code{target_10_year_2029}, and \code{target_20_year_2039}}
}
}
}
\source{
various Long-Term Watering Plans and State-based tables. Extracted from EWR tool (py_ewr) and reexported here.
}
\usage{
causal_ewr
}
\description{
List of dataframes specifying the causal relationships from environmental
water requirements through the rest of the levels specified in Long-Term
Watering Plans. Previously created in HydroBOT, now simply extracted from
py_ewr (as of Nov 2024). May not remain here much longer, extracting the EWR
tool version with \code{\link[=get_causal_ewr]{get_causal_ewr()}} is preferable.
}
\keyword{datasets}
