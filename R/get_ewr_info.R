#' get the gauges from the EWR tool
#'
#' @return sf with unique gauges with good EWRs
#' @export
#'

get_ewr_gauges <- function() {
  ewrs_in_pyewr <- get_ewr_table()
  gauges_in_pyewr <- ewrs_in_pyewr |>
    dplyr::select("Gauge", "PlanningUnitName", "LTWPShortName", "SWSDLName", "GaugeType") |>
    dplyr::distinct() |>
    tibble::tibble()

  names(gauges_in_pyewr) <- nameclean(names(gauges_in_pyewr))

  gauges_in_pyewr <- gauges_in_pyewr |>
    dplyr::left_join(HydroBOT::bom_basin_gauges, by = 'gauge') |>
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

  # in older versions of the ewr tool, this was a list of 'good' and 'bad' ewr
  # tables. Now it's just a table.
  if (length(ewrs_in_pyewr)==2) {
    if (type == "good") {
      ewrs_in_pyewr <- ewrs_in_pyewr[[1]]
    }
    if (type == "bad") {
      ewrs_in_pyewr <- ewrs_in_pyewr[[2]]
    }
  }

  return(tibble::tibble(ewrs_in_pyewr))
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
  b <- reticulate::import("importlib.metadata")
  ewrpk <- b$version("py_ewr")

  return(as.character(ewrpk))
  # Or
  # system.time(a <- system2("pip", "show py_ewr", stdout = TRUE))
}

#' Extract causal network from EWR tool
#'
#' @return list of EWR causal networks
#' @export
#'
get_causal_ewr <- function(struct = 'list') {
  pdi <- reticulate::import("py_ewr.data_inputs")

  ev <- get_ewr_version()
  evm <- as.numeric(unlist(strsplit(ev, '\\.')))
  if (evm[1] <= 2 & evm[2] < 4) {
    gce <- pdi$get_causal_ewr()
    gce <- purrr::map(gce, tibble::as_tibble)
    gce <- purrr::map(gce, \(x) {
      attributes(x)$pandas.index <- NULL
      return(x)
    })
  } else {
    # now it has a new name and is flat
    gce <- tibble::tibble(pdi$get_obj_mapping())
    attributes(gce)$pandas.index <- NULL
  }

  gce <- clean_ewr_causal(gce)

  # make a ewr_code_main column
  gce <- separate_ewr_codes(gce)

  if (struct == 'list') {
    refcols <- c('planning_unit_name', 'LTWPShortName', 'SWSDLName', 'state', 'gauge')
    # just return all pairwise combinations. Otherwise return flat.
    causal_steps <- names(gce)[!names(gce) %in% refcols]

    causal_list <- vector(mode = 'list')
    for (i in seq_along(causal_steps[1:(length(causal_steps)-1)])) {
      for (j in (i + 1):length(causal_steps)) {

        causal_list[[paste0(causal_steps[i], '_to_', causal_steps[j])]] <- dplyr::distinct(gce[, c(refcols, causal_steps[i], causal_steps[j])])

      }
    }

    # for common return val
    gce <- causal_list
  }



  return(gce)
}
