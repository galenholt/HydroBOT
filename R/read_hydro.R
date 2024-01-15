#' Wrapper to read hydrographs into R
#'
#' @param hydropath path to the hydrographs
#' @param long logical, default TRUE. Do we want the data long (with a column for gauge number) or wide (FALSE, with gauge numbers as columns)
#' @param format character, default 'csv'. Format of the hydrographs, determines which reading function gets called.
#' @param scenariofilter character vector of scenario names to include. Default `NULL` includes all
#' @param gaugemap mapping of gauges from nodes. Default 'iqqm' gets it from EWR tool, otherwise, a dataframe with columns 'node' and 'gauge'
#'
#' @return tibble of hydrographs
#' @export
#'
#' @examples
read_hydro <- function(hydropath, scenariofilter = NULL, long = TRUE, format = 'csv', gaugemap = 'iqqm') {
  if (format == 'csv') {
    return(read_hydro_csv(hydropath, scenariofilter, long))
  }
  if (format == 'nc') {
    return(read_hydro_nc(hydropath, scenariofilter, long, gaugemap = gaugemap))
  }
}

#' Read csv hydrographs in the standard directory structure
#'
#' Uses the machinery of [find_scenario_paths] to get paths and names of the
#' hydrographs, read them in, and name them.
#'
#' @inheritParams read_hydro
#'
#' @return tibble of hydrographs
#' @export
#'
#' @examples
read_hydro_csv <- function(hydropath, scenariofilter, long) {
  hydro_paths <- find_scenario_paths(hydropath, type = 'csv')

  if (!is.null(scenariofilter)) {
    hll <- length(hydro_paths)
    hydro_paths <- hydro_paths[names(hydro_paths) %in% scenariofilter]
    if (hll > 0 & length(hydro_paths) == 0) {
      rlang::abort(c("No hydrographs read.",
                     "i" = glue::glue("{hll} hydrographs found in {hydropath}, but none meet scenariofilter."),
                     "i" = "Are the scenariofilter values full scenario names?",
                     "i" = glue::glue("A common issue is that the toolkit will read these in as 'scenarioname_scenarioname' when they are files in folders with the same name. Try `scenariofilter = {stringr::str_c(scenariofilter, '_', scenariofilter)}`")))
    }
  }



  # read-in and extract the names
  hydros <- purrr::imap(hydro_paths,
                        \(x, idx) readr::read_csv(x, show_col_types = FALSE) |>
                          dplyr::mutate(scenario = idx) |>
                          dplyr::select(scenario, tidyselect::everything())) |>
    dplyr::bind_rows()


  if (long) {
    hydros <- tidyr::pivot_longer(hydros, cols = -c(scenario, Date), names_to = "gauge", values_to = "flow")
  }

  return(hydros)
}

#' Read netcdf hydrographs in the standard directory structure
#'
#' Uses the machinery of [find_scenario_paths] to get paths and names of the
#' hydrographs, read them in, and name them.
#'
#' @inheritParams read_hydro
#'
#' @return tibble of hydrographs
#' @export
#'
#' @examples
read_hydro_nc <- function(hydropath, scenariofilter, long, gaugemap) {

  rlang::check_installed(c('metR', 'PCICt', 'ncdf4'), reason = "reading netcdf hydrographs requires `metR`, which requires `PCICt`.")
  hydro_paths <- find_scenario_paths(hydropath, type = 'nc')

  if (!is.null(scenariofilter)) {
    hydro_paths <- hydro_paths[names(hydro_paths) %in% scenariofilter]
  }

  # ncdf4 is the main ncdf package, but metR's wrapper is WAY nicer to use, and I don't have to reinvent the wheel to retain date formats and make tidy.
  # nco <- ncdf4::nc_open(fp)
  # metR::GlanceNetCDF(fp)

  if (!inherits(gaugemap, "data.frame") && gaugemap == 'iqqm') {
    gaugemap <- get_iqqm_gauges() |>
      dplyr::mutate(node = as.integer(iqqm_node)) |>
      dplyr::select(-iqqm_node)
  }

  # we need to purrr over hydro_paths
  readscenes <- function(x,idx) {
    # this is fairly specific to Ash's naming conventions. If that changes, we'll need to be more general
    if (inherits(gaugemap, 'data.frame')) {
      # A major downside of `metR` is that it grabs the closest value. So if we ask for nodes that aren't there, it just grabs something that *is*.
      truenodes <- metR::GlanceNetCDF(x)$dims$node$vals
      sh <- metR::ReadNetCDF(x, vars = 'Simulated flow', subset = list(node = as.list(gaugemap$node[gaugemap$node %in% truenodes]))) |>
        dplyr::left_join(gaugemap, by = 'node') |>
        dplyr::select(gauge, everything())
    } else {
      sh <- metR::ReadNetCDF(x, vars = 'Simulated flow')
    }

    sh <- sh |>
      dplyr::mutate(scenario = idx) |>
      dplyr::select(scenario, tidyselect::everything()) |>
      dplyr::rename(flow = 'Simulated flow')

    return(sh)
  }

  hydros <- purrr::imap(hydro_paths, readscenes) |>
    dplyr::bind_rows() |>
    tibble::tibble()

  # make the same as the csv version, but retain the node column
  # The any_of there is if we aren't mapping to gauge
  hydros <- hydros |>
    dplyr::select(scenario, Date = time, any_of('gauge'), flow, node)

  if (!long) {
    hydros <- tidyr::pivot_wider(hydros, id_cols = c(scenario, Date), names_from = gauge, values_from = flow)
  }

  return(hydros)
}
