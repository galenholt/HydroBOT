% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_helpers.R
\name{handle_palettes}
\alias{handle_palettes}
\title{Makes the calls to \verb{scale_fill_*} and \verb{scale_color_*}, depending on whether color/fill and the palette type needed}
\usage{
handle_palettes(
  ggobj,
  aes_type,
  pal_list,
  color_type,
  transoutcome = "identity",
  setLimits = NULL,
  pal_direction = rep(1, length(pal_list)),
  base_list = NULL,
  nbins = 10
)
}
\arguments{
\item{ggobj}{The in-construction ggplot object}

\item{aes_type}{'color' or 'fill'}

\item{pal_list}{list of palettes for defining colors for \code{colorset}. Should
be length of \code{colorgroups}}

\item{color_type}{from \code{\link[=find_color_type]{find_color_type()}}}

\item{transoutcome}{transformation for outcome as in
\code{\link[ggplot2:scale_continuous]{ggplot2::scale_y_continuous()}} or \code{\link[ggplot2:scale_colour_continuous]{ggplot2::scale_fill_continuous()}}.
Default \code{transoutcome = 'identity'} just uses the data. Most common change
likely \verb{transoutcome = 'log10}}

\item{setLimits}{sets user-supplied color/fill limits or midpoints for maps,
heatmaps, and networks, or y limits for other plots. Also sets \code{underlay}
and \code{overlay} limits for consistency.
\itemize{
\item NULL (default) use internally-calculated limits
\item length-1 numeric, sets midpoint, limits stay auto-calculated. V. useful for divergent
\item length-2 numeric, sets lower and upper limits
\item length-3 numeric, sets limits and midpoint \code{c(lower, mid, upper)}, with caveat that one limit may be ignored (must be symmetrical about the midpoint)
\item if base_list is 'difference' or 'relative', midpoints are auto-set at 0 or 1 (respectively) unless otherwise specified here.
}}

\item{pal_direction}{vector of length pal_list, either 1 (default) or -1
(reversed) direction of the palettes}

\item{base_list}{NULL (default) or named list of arguments to
\code{\link[=baseline_compare]{baseline_compare()}};
\itemize{
\item base_lev
\item comp_fun
\item group_cols
\code{\link[=plot_data_prep]{plot_data_prep()}} handles \code{zero_adjust}, and other arguments are inferred
or not supported If \code{comp_fun} is 'difference' or 'relative', midpoint
auto-set at 0 or 1 respectively unless otherwise specified with \code{setLimits}
}}

\item{nbins}{number of bins for contours}
}
\value{
modified ggplot object
}
\description{
Uses \code{aes_type} to know the color/fill, and \code{color_type} (from \code{\link[=find_color_type]{find_color_type()}}) to know what ggplot function to use to add the color. Needs to handle any color trans as well with \code{transoutcome}
}
