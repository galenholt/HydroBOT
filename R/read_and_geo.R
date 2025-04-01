#' Prepare ewr results from the EWR tool for use in aggregation
#'
#' This reads in the EWR results given a path and `type` (summary or annual),
#' and pairs them to their geographic information
#'
#' @param datapath path to module results (Often EWR outputs with HydroBOT
#'   modifications to format)
#' @param type character, a grep for the files to choose. Two special cases:
#'  * 'achievement', calculates EWR achievement from 'yearly',
#'  * 'everything', gets all files
#'  For the EWR tool, the direct options are
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents',
#'  * 'all_successful_interEvents'
#' @param geopath sf object with geographic locations matching a column in the data, or path to a csv with gauge locations in lat/long (assumes BOM
#'   currently) or a shapefile
#' @param whichcrs desired crs for the output
#' @param ... passes `gaugefilter`, `scenariofilter`, and `add_max` to [prep_ewr_output()].
#'   Particularly useful for only processing some of the data at a time (e.g. in
#'   parallel)
#'
#' @return an `sf` dataframe with the desired outputs including scenario
#'   names and gauge or polygon locations as a POINT, POLYGON, or MULTIPOLYGON geometry. Primary purpose is to
#'   prepare data to send to [multi_aggregate()]
#' @export
#'
read_and_geo <- function(datapath, type = 'achievement', geopath, whichcrs = 4283, ...) {
  # Dots pass gaugefilter and scenariofilter
    dat <- get_module_output(datapath, type, ...)

  # deal with a single dataframe vs list of dfs
  if (is.data.frame(dat)) {
    dat <- join_to_geo(dat, geopath, whichcrs)
  } else {
    dat <- dat |>
      purrr::map(~join_to_geo(.x, geopath, whichcrs))
  }

    return(dat)
}
