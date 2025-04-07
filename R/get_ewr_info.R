#' get the gauges from the EWR tool
#'
#' @return sf with unique gauges with good EWRs
#' @export
#'

get_ewr_gauges <- function() {
  ewrs_in_pyewr <- get_ewr_table()
  gauges_in_pyewr <- ewrs_in_pyewr |>
    dplyr::select("Gauge", "PlanningUnitName", "LTWPShortName", "GaugeType") |>
    dplyr::distinct() |>
    tibble::tibble()

  names(gauges_in_pyewr) <- stringr::str_to_lower(names(gauges_in_pyewr))

  gauges_in_pyewr <- gauges_in_pyewr |>
    dplyr::left_join(HydroBOT::bom_basin_gauges) |>
    sf::st_as_sf()

  return(gauges_in_pyewr)
}

#' Get the table of EWRs from the EWR tool
#'
#' @param type 'good', default, or 'bad'
#'
#' @return dataframe of the ewr table
#' @export
#'

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
  # There's lots of blanks that get guessed as logical unless we let it see the
  # whole thing.
  rawsheet <- readr::read_csv(
    ".venv/Lib/site-packages/py_ewr/parameter_metadata/parameter_sheet.csv",
    guess_max = 5000
  )
  return(rawsheet)
}

#' quick map of the gauges in the ewr tool
#'
#' @return ggplot
#'
#' @keywords internal
map_ewr_gauges <- function() {
  ewrgauges <- get_ewr_gauges()
  gaugemap <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = HydroBOT::sdl_units,
      ggplot2::aes(geometry = .data$geometry)
    ) +
    ggplot2::geom_sf(
      data = ewrgauges,
      ggplot2::aes(
        color = .data$owner,
        geometry = .data$geometry
      )
    )
  return(gaugemap)
}

#' Get the mapping of gauges to IQQM nodes for netcdf
#'
#'
#' @return dataframe of the ewr table
#' @export
#'

get_iqqm_gauges <- function() {
  pdi <- reticulate::import("py_ewr.data_inputs")
  iqqm_gauges <- pdi$get_iqqm_codes()
  iqqm_gauges <- tibble::tibble(
    iqqm_node = names(iqqm_gauges),
    gauge = unlist(iqqm_gauges)
  )
  return(iqqm_gauges)
}

#' Extract the package version. Does not get complications like git branches.
#'
#' @return character version number of the py-ewr package
#' @export
#'
get_ewr_version <- function() {
  b <- reticulate::import("pkg_resources")
  ewrpk <- b$get_distribution("py_ewr")

  return(as.character(ewrpk))
  # Or
  # system.time(a <- system2("pip", "show py_ewr", stdout = TRUE))
}

#' Extract causal network from EWR tool
#'
#' @return list of EWR causal networks
#' @export
#'
get_causal_ewr <- function() {
  pdi <- reticulate::import("py_ewr.data_inputs")
  gce <- pdi$get_causal_ewr()
  gce <- purrr::map(gce, tibble::as_tibble)
  gce <- purrr::map(gce, \(x) {
    attributes(x)$pandas.index <- NULL
    return(x)
  })
  return(gce)
}
