# Helper functions for finding and setting up directories. I don't think we want to export these.

#' get the paths to scenarios
#'
#' @param hydro_dir directory with hydrographs
#' @param type filetype, default 'csv', likely will also need 'ncdf'
#'
#' @return a list of filepaths to the hydrographs
#'
#' @examples
find_scenario_paths <- function(hydro_dir, type = 'csv') {


  # get the paths relative to hydro_dir
  if (grepl('.zip', hydro_dir)) {
    rlang::inform("data in zip is assumed to be 'Straight Node (Gauge).nc'. If that is not the case, will need to allow an argument to specify.")
    hydro_paths <- unzip(hydro_dir, list = TRUE)$Name
    hydro_paths <- hydro_paths[grepl('Straight Node \\(Gauge\\)\\.nc', hydro_paths)]
  } else {
    hydro_paths <- list.files(hydro_dir, pattern = paste0('.', type, '$'), recursive = TRUE)
  }

  unique_names <- hydro_paths |>
    stringr::str_remove_all(paste0('\\.', type)) |>
    stringr::str_replace_all("/", "_")

  # add the path to hydro_dir back on. This keeps things relative to whatever hydro_dir is relative to
  hydro_paths <- file.path(hydro_dir, hydro_paths)

  hydro_paths <- as.list(hydro_paths) |>
    setNames(unique_names)

  return(hydro_paths)
}


#' Set up the output directory structure based on parent directory and module
#'
#' @param parent_dir parent directory- typically for a project, but could be a single scenario
#' @param scenarios names of the scenarios (character vector)
#' @param module_name default 'EWR', sets up different directories for different modules
#' @param ewr_outtypes character vector, names of EWR outputs to return. Options include
#'  * 'summary': outputs summarised to the period
#'  * 'yearly': outputs summarised to year
#'  * 'all_events'
#'  * 'all_successful_events'
#'  * 'all_interEvents': currently fails in EWR tool, and so not supported
#'  * 'all_successful_interEvents'
#'
#' @return path to output directory
#'
#' @examples
make_output_dir <- function(parent_dir, scenarios, module_name = 'EWR',
                            ewr_outtypes = c('summary', 'annual', 'all_events')) {
  output_path <- file.path(parent_dir, 'module_output', module_name)

  # if parent_dir is the scenario dir (we're in a single run), use `scenarios = ''` to not make a subdir
  sceneout <- file.path(output_path, scenarios)

  # make the scenario directory if needed
  for (i in sceneout) {
    if (!dir.exists(i)) {dir.create(i, recursive = TRUE)}
  }

  # make the internal directories if the module returns different sorts of output
  # This will have to be built for each module, since they will return different things.
  # We could do this in the loop above, but that will just get hard to read.
  if (module_name == 'EWR') {
    for (i in sceneout) {
      for (j in ewr_outtypes) {
        if (!dir.exists(file.path(i, j))) {dir.create(file.path(i,j), recursive = TRUE)}
      }
    }
  }

  return(output_path)
}



#' Used to auto-acquire scenario names from the hydrograph directory
#'
#' DEPRECATED, only used for the old prep_run_save_ewrs
#'
#' @param hydro_dir path to hydrograph directory. Can be the outer directory with all, or potentially a single scenario
#'
#' @return character vector of scenario names
#'
#' @examples
scenario_names_from_hydro <- function(hydro_dir) {

  # Remove files with extensions- we only want directories
  scenarios <- list.files(hydro_dir)
  only_dirs <- stringr::str_which(scenarios, '\\.', negate = TRUE)
  # If there's already module_output, ignore it
  no_modules <- stringr::str_which(scenarios, 'module_output', negate = TRUE)

  # This really seems unnecessarily complicated
  scenarios <- scenarios[only_dirs[which(only_dirs %in% no_modules)]]

  if (length(scenarios) == 0) {
    rlang::inform(glue::glue("no internal directories in hydro_dir. Assuming this is a single scenario and using the last directory of hydro_dir ({basename(hydro_dir)}) as the scenario name. If this is not correct, manually specify `scenarios`"))
    scenarios <- basename(hydro_dir)
  }
  return(scenarios)
}


#' Fixes potential issue with non-unique scenario names, which *may involve file renaming*
#'
#' 'scenario' here gets the csv name. That happens inside the EWR tool- the
#' `_get_file_names` function does a `.split('/')` on the filepath to name the
#' scenario. This causes issues when we have csvs per gauge, each with just the
#' gaugename. In that case the EWR tool assigns them the gaugename as the
#' scenario. So we need to ensure that doesn't happen. I think probably the best
#' way to do this is to encourage the use of single files per scenario, and if
#' not, filenames that have the scenario name in them with the gauge name. This
#' function tests if there's an issue, tries to fix it and informs or warns the
#' user to name files better
#'
#' @param hydro_paths paths to hydrographs, typically from [find_scenario_paths()]
#' @param scenarios names of the scenarios, useful for cross-checking
#'
#' @return paths to hydrographs, possibly fixed
#'
#' @examples
fix_file_scenarios <- function(hydro_paths, scenarios) {
  # Do the split exactly like the ewr tool to make sure the scenarios will work
  split_path <- sapply(hydro_paths, strsplit, '/')
  ewr_scenario_name <- sapply(split_path, \(x) stringr::str_remove(x[length(x)], '\\.[A-z]+'))
  if (any(duplicated(ewr_scenario_name))) {
    path_scenarios <- sapply(split_path, \(x) x[length(x)-1]) |> unique()

    if (all(path_scenarios %in% scenarios)) {
     rename_path <- lapply(split_path, \(x) end_paster(x, npastes = 2))
     rlang::inform("files not uniquely named and so EWR tool would return
                   duplicate scenario names for different scenarios.
                   Since file structure appears to be structured around scenarios,
                   scenario names are being appended.
                   A better solution would be unique scenario names to start with")
    } else {
      rename_path <- lapply(split_path, \(x) end_paster(x, npastes = length(x)))
      rlang::warn("files not uniquely named and so EWR tool would return
                  duplicate scenario names for different scenarios.
                  File structure does not appear to be structured around scenarios,
                  so the scenario name is being given the full path.
                  A better solution would be unique scenario names to start with")
    }
    new_paths <- lapply(rename_path, \(x) paste0(x, collapse = '/'))
    file.rename(from = unlist(hydro_paths), to = unlist(new_paths))

    rlang::inform("Due to duplicate input files across scenario directories,
                  they have been made unique. The resulting `scenario` column
                  in the EWR tool will include both `scenario` and `filename`,
                  separated by '_DIRECTORYAPPEND_'. You will likely need to adjust by splitting
                  on '_DIRECTORYAPPEND_' and only keeping the scenario.")

    hydro_paths <- new_paths
  }

  return(hydro_paths)
}

# silly helper
end_paster <- function(x, npastes) {
  x[length(x)] <- paste0(x[(length(x)-(npastes-1)):length(x)], collapse = '_DIRECTORYAPPEND_')
  return(x)
}
