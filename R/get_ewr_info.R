#' get the gauges from the EWR tool
#'
#' @return sf with unique gauges with good EWRs
#' @export
#'
#' @examples
get_ewr_gauges <- function() {
  ewrs_in_pyewr <- get_ewr_table()
  gauges_in_pyewr <- ewrs_in_pyewr |>
    dplyr::select(Gauge, PlanningUnitName, LTWPShortName, GaugeType) |>
    dplyr::distinct() |>
    tibble::tibble()

  names(gauges_in_pyewr) <- stringr::str_to_lower(names(gauges_in_pyewr))

  gauges_in_pyewr <- gauges_in_pyewr |>
    dplyr::left_join(bom_basin_gauges)

  return(gauges_in_pyewr)
}

#' Get the table of EWRs from the EWR tool
#'
#' @param type 'good', default, or 'bad'
#'
#' @return dataframe of the ewr table
#' @export
#'
#' @examples
get_ewr_table <- function(type = "good") {
  pdi <- reticulate::import("py_ewr.data_inputs")
  ewrs_in_pyewr <- pdi$get_EWR_table()
  names(ewrs_in_pyewr) <- c("good", "bad")
  if (type == "good") {
    ewrs_in_pyewr <- ewrs_in_pyewr[[1]]
  }
  if (type == "bad") {
    ewrs_in_pyewr <- ewrs_in_pyewr[[2]]
  }
  return(ewrs_in_pyewr)
}

get_raw_ewrsheet <- function() {
  rawsheet <- readr::read_csv(".venv/Lib/site-packages/py_ewr/parameter_metadata/parameter_sheet.csv")
}

#' Check the ewr version.
#'
#' @return character, version number
#'
check_ewr_version <- function() {
  pypkgs <- reticulate::py_list_packages()
  ewrversion <- pypkgs$version[which(pypkgs$package == "py_ewr")]

  if (length(ewrversion) == 0) {
    rlang::inform("reticulate can't find version. Trying brute force")
    path_to_pypkgs <- file.path(Sys.getenv("VIRTUAL_ENV"), "Lib/site-packages")
    dirs <- list.files(path_to_pypkgs)
    pyverdir <- dirs[grep("py_ewr-", dirs)]
    # could get this from METADATA text file, but easier to just deal with the directory name
    ewrversion <- stringr::str_remove_all(pyverdir, "py_ewr-") |>
      stringr::str_remove_all(".dist-info")
  }
  return(ewrversion)
}

#' quick map of the gauges in the ewr tool
#'
#' @return ggplot
#'
#' @examples
map_ewr_gauges <- function() {
  ewrgauges <- get_ewr_gauges()
  gaugemap <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = sdl_units, ggplot2::aes(geometry = geometry)) +
    ggplot2::geom_sf(data = ewrgauges, ggplot2::aes(color = owner, geometry = geometry))
  return(gaugemap)
}

#' Get the mapping of gauges to IQQM nodes for netcdf
#'
#'
#' @return dataframe of the ewr table
#' @export
#'
#' @examples
get_iqqm_gauges <- function() {
  pdi <- reticulate::import("py_ewr.data_inputs")
  iqqm_gauges <- pdi$get_iqqm_codes()
  iqqm_gauges <- tibble::tibble(iqqm_node = names(iqqm_gauges), gauge = unlist(iqqm_gauges))
  return(iqqm_gauges)
}
