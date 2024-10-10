# Helper functions for finding and setting up directories. I don't think we want to export these.

#' get the paths to scenarios
#'
#' @param hydro_dir directory with hydrographs
#' @param type filetype, default 'csv', likely will also need 'ncdf'
#' @param file_search character, regex for additional limitations on filenames. Useful if several files have the extension defined by `type`, but only some are hydrographs.
#'
#' @return a list of filepaths to the hydrographs
#' @export
#'
#' @examples
find_scenario_paths <- function(hydro_dir, type = 'csv', scenarios_from = 'directory', file_search = NULL) {


  # get the paths relative to hydro_dir
  if (grepl('.zip', hydro_dir)) {
    # rlang::inform("data in zip is assumed to be 'Straight Node (Gauge).nc'. If that is not the case, will need to allow an argument to specify.")
    hydro_paths <- unzip(hydro_dir, list = TRUE)$Name
    # hydro_paths <- hydro_paths[grepl('Straight Node \\(Gauge\\)\\.nc', hydro_paths)]
  } else {
    hydro_paths <- list.files(hydro_dir, pattern = paste0('.', type, '$'), recursive = TRUE)
  }

  # at this point, hydro_paths has *all* files with extension `type`. Allow additional grep
  if (!is.null(file_search)) {
    hydro_paths <- hydro_paths[grepl(file_search, hydro_paths)]
  }

  # This removes the extension and turns / into _, since paths must be unique
  if (scenarios_from == 'directory') {
    # Remove the final file, leaving only the directory
    scenario_names <- gsub("/[^/]+$",'', hydro_paths) |>
      stringr::str_remove_all(paste0('\\.', type)) |>
      stringr::str_replace_all("/", "_")
  } else {
    scenario_names <- hydro_paths |>
      stringr::str_remove_all(paste0('\\.', type)) |>
      stringr::str_replace_all("/", "_")
  }


  # no need for a scenarios_from == 'file', since that's what scenario_names is originally.

  # add the path to hydro_dir back on. This keeps things relative to whatever hydro_dir is relative to
  hydro_paths <- file.path(hydro_dir, hydro_paths)

  hydro_paths <- as.list(hydro_paths) |>
    setNames(scenario_names)

  # a specific bit of cleanup
  names(hydro_paths) <- gsub(' |\\(|\\)', '', names(hydro_paths))
  names(hydro_paths) <- gsub('_StraightNodeGauge', '', names(hydro_paths))

  return(hydro_paths)
}


#' Set up the output directory structure based on parent directory and module at `parent_dir/module_output/MODULE_NAME/subdir`
#'
#' @param parent_dir parent directory- typically for a project, but could be a single scenario
#' @param scenarios names of the scenarios (character vector)
#' @param module_name default 'EWR', sets up different directories for different modules
#' @param subdir character, default "" for none, but if included, makes a sub-directory in parent_dir/module_output/MODULE_NAME
#' @param ewr_outtypes character vector, names of EWR outputs to return. Options include
#'  * 'summary': outputs summarised to the period
#'  * 'yearly': outputs summarised to year
#'  * 'all_events'
#'  * 'all_successful_events'
#'  * 'all_interEvents': currently fails in EWR tool, and so not supported
#'  * 'all_successful_interEvents'
#'
#' @return path to output directory
#' @export
#'
#' @examples
make_output_dir <- function(parent_dir,
                            scenarios,
                            module_name = 'EWR',
                            subdir = "",
                            ewr_outtypes = c('summary', 'yearly')) {

  output_path <- file.path(parent_dir, 'module_output', module_name, subdir)

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
      if (!dir.exists(file.path(i))) {dir.create(file.path(i), recursive = TRUE)}
      # for (j in ewr_outtypes) {
      #   if (!dir.exists(file.path(i, j))) {dir.create(file.path(i,j), recursive = TRUE)}
      # }
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

#' Figure out the expected outputs
#'
#' @param hydro_paths list of paths to scenario files with names scenario names
#' @param output_path path for outputs
#' @param outputType types of outputs, for inferring csvs
#'
#' @return
#' @export
#'
find_expected_files <- function(hydro_paths, output_path, outputType, scenarios_from) {

  # output path should be length 1, but output Type may not be.
  if (length(output_path) > 1 ) {
    rlang::abort('More than one output path, unclear where to look for expected outputs.')
  }

  filepart <- ''
  if (scenarios_from == 'directory') {
    filepart <- stringr::str_extract(hydro_paths, '/[^/]+$') |>
      stringr::str_remove_all('/|\\.csv|\\.nc')
    filepart <- paste0('_', filepart)
  }
  expected_files <- foreach::foreach(ot = outputType,
                              .combine = c) %do% {
    file.path(names(hydro_paths), paste0(ot, filepart, '.csv'))
                              }

  return(expected_files)

}

#' Figure out missing expected outputs
#'
#' @param hydro_paths list of paths to scenario files with names scenario names
#' @param output_path path for outputs
#' @param outputType types of outputs, for inferring csvs
#'
#' @return
#' @export
#'
find_missing_runs <- function(hydro_paths, output_path, outputType, scenarios_from) {
  # Get the files that exist
  existing_files <- file.path(list.files(output_path, recursive = TRUE))

  # Get expected
  expected_files <- find_expected_files(hydro_paths, output_path, outputType, scenarios_from)

  # get the expected files that aren't existing
  missing_files <- expected_files[!expected_files %in% existing_files]

  # what we want is anyting with *any* missing files. so if e.g. there's a
  # 'summary' that got made but a crash before 'yearly', the yearly will still
  # be in the file list here, and we then can pick it up. I think that's very
  # unlikely
  missing_scenarios <- missing_files |>
    stringr::str_remove("/[^/]+$") |>
    unique()

  return(missing_scenarios)
}


#' Similar to find_missing_runs, but excised from prep_run_save_ewrs to take the same arguments
#'
#' @inheritParams prep_run_save_ewrs
#'
#' @return
#' @export
#'
#' @examples
check_missing_runs <- function(hydro_dir,
                               output_parent_dir,
                               output_subdir = '',
                               scenarios = NULL,
                               file_search = NULL,
                               model_format = "Standard time-series",
                               outputType = "none",
                               scenarios_from = 'directory') {

  if (is.null(scenarios)) {
    if (model_format %in% c("IQQM - NSW 10,000 years", 'Standard time-series', "Bigmod - MDBA")) {
      filetype <- "csv"
    }
    if (grepl("netcdf", model_format)) {
      filetype <- "nc"
    }
    hydro_paths <- find_scenario_paths(hydro_dir, type = filetype, file_search = file_search)
  } else {
    hydro_paths <- purrr::map(scenarios, \(x) file.path(hydro_dir, x))
  }

  output_path <- make_output_dir(output_parent_dir,
                                 scenarios = names(hydro_paths),
                                 module_name = "EWR",
                                 subdir = output_subdir,
                                 ewr_outtypes = unlist(outputType)
  )

  missing_scenarios <- find_missing_runs(hydro_paths, output_path, outputType, scenarios_from)

  return(missing_scenarios)
}

# silly helper
end_paster <- function(x, npastes) {
  x[length(x)] <- paste0(x[(length(x)-(npastes-1)):length(x)], collapse = '_DIRECTORYAPPEND_')
  return(x)
}
