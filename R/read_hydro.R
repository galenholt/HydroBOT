#' Wrapper to read hydrographs into R
#'
#' @param hydropath path to the hydrographs
#' @param long logical, default TRUE. Do we want the data long (with a column for gauge number) or wide (FALSE, with gauge numbers as columns)
#' @param format character, default 'csv'. Format of the hydrographs, determines which reading function gets called.
#' @param scenariofilter character vector of scenario names to include. Default `NULL` includes all
#'
#' @return tibble of hydrographs
#' @export
#'
#' @examples
read_hydro <- function(hydropath, scenariofilter = NULL, long = TRUE, format = 'csv') {
  if (format == 'csv') {
    return(read_hydro_csv(hydropath, scenariofilter, long))
  }
}

#' Read csv hydrographs in the standard directory structure
#'
#' Standard structure is
#' hydropath(name of set of scenarios) ->
#' hydrographs ->
#' dirs for scenario names ->
#' csv files named SCENARIO_NAME.csv, with cols for date and each gauge
#'  This last bit could have files for each gauge separately (was the original format)
#'
#'
#' @inheritParams read_hydro
#'
#' @return tibble of hydrographs
#' @export
#'
#' @examples
read_hydro_csv <- function(hydropath, scenariofilter, long) {
  all_hydros <- list.files(hydropath, recursive = TRUE, pattern = "*.csv")

  if (!is.null(scenariofilter)) {
    all_hydros <- all_hydros[stringr::str_which(all_hydros,
                                                pattern = stringr::str_flatten(scenariofilter, collapse = '|'))]
  }

  # read-in and extract the names
  hydros <- readr::read_csv(file.path(hydropath, all_hydros),
                            id = 'scenario', show_col_types = FALSE) |>
    dplyr::mutate(scenario = stringr::str_extract(scenario, "(\\w|\\d)+\\.csv$"),
                  scenario = stringr::str_extract(scenario, '^[\\w|\\d]+'))

  if (long) {
    hydros <- tidyr::pivot_longer(hydros, cols = -c(scenario, Date), names_to = "gauge", values_to = "flow")
  }

  return(hydros)
}
