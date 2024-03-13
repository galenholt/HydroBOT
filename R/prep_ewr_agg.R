#' Prepare ewr results from the EWR tool for use in aggregation
#'
#' This reads in the EWR results given a path and `type` (summary or annual),
#' and pairs them to their geographic information
#'
#' @param ewrpath path to the EWR results output by the EWR tool (with toolkit
#'   modifications to format)
#' @param type character, one of:
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents', # Does not work with current EWR tool
#'  * 'all_successful_interEvents'
#' @param geopath path to the file with gauge locations in lat/long (assumes BOM
#'   currently)
#' @param whichcrs desired crs for the output
#' @param ... passes `gaugefilter` and `scenariofilter` to [get_ewr_output()].
#'   Particularly useful for only processing some of the data at a time (e.g. in
#'   parallel)
#'
#' @return an `sf` dataframe with the desired EWR outputs including scenario
#'   names and gauge locations as a POINT geometry. Primary purpose is to
#'   prepare data to send to [multi_aggregate()]
#' @export
#'
#' @examples
prep_ewr_agg <- function(ewrpath, type = 'achievement', geopath, whichcrs = 4283, ...) {
  # Dots pass gaugefilter and scenariofilter
    dat <- get_ewr_output(ewrpath, type, ...)

  # deal with a single dataframe vs list of dfs
  if (is.data.frame(dat)) {
    dat <- gauge2geo(dat, geopath, whichcrs)
  } else {
    dat <- dat |>
      purrr::map(~gauge2geo(.x, geopath, whichcrs))
  }


}
