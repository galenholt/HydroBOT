#' Convert pulled gauge data into the EWR 'Standard time-series' format
#'
#' This adds the _flow and _level suffixes and cleans things up
#' Expects data from either states or BOM, only tested using [galenholt/hydrogauge]
#' If gauges are different lengths or need to be otherwise joined, that should be done afterwards
#'
#' @param gaugedata a dataframe of gauge data pulled from states or BOM
#'
#' @returns gauge data in 'Standard time-series' format
#' @export
#'
make_ewrformat <- function(gaugedata) {
  gaugedata <- gaugedata |>
    dplyr::mutate(
      gauge = dplyr::case_when(
        grepl('ml/d', units, ignore.case = TRUE) ~ paste0(gauge, '_flow'),
        units %in% c('m', 'Res. Level AHD', 'Metres') ~ paste0(gauge, '_level'),
        .default = paste0(gauge, '_FAIL')
      )
    ) |>
    dplyr::select(Date = time, gauge, value) |>
    tidyr::pivot_wider(
      id_cols = Date,
      names_from = gauge,
      values_from = value
    ) |>
    dplyr::mutate(Date = as.Date(Date))
  return(gaugedata)
}
