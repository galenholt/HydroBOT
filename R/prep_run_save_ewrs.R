# Wrap the python

## NOTE: this imports from system.file, and so while developing won't pick up
## new versions without `install`ing the package. The `future` uses seem most
## sensitive to this- I think maybe test shims don't work right?
controller_functions <- reticulate::import_from_path("controller_functions",
                                                     path = system.file("python",
                                                                        package = 'werptoolkitr'),
                                                     delay_load = TRUE)

#' Set up, run, and (possibly) save EWR outputs
#'
#' This does some directory setup and parsing, runs the EWR tool and, if asked,
#' saves the output. If the output saves, it also auto-saves both yaml and json
#' metadata files with all parameters needed to run this part of the toolkit
#' with parameters. Scenario metadata is prepended, if found.
#'
#' @param hydro_dir Directory containing hydrographs. Can be an outer directory,
#'   e.g. `hydrographs` that splits into scenario subdirs, or can be a single
#'   scenario subdir.
#' @param output_parent_dir parent directory for the outputs. Can be anything,
#'   but two typical cases:
#'  * The directory containing `hydro_dir`, which puts the `module_outputs` at the same level as the hydrographs
#'  * If running in batches for single scenarios, may be `hydro_dir`, which just puts the `module_outputs` in `hydro_dir`
#' @param scenarios `NULL` (default) or character.
#'  * `NULL`- finds scenario names by parsing directory names in `hydro_dir`. If no internal directories, just stays in `hydro_dir`. This captures the two typical situations discussed for `output_parent_dir`. If there are other directories in `hydro_dir` that do not contain hydrological scenarios, should use a character vector.
#'  * character vector of scenario names. This allows unusual directory structures.
#' @param model_format see EWR tool. One of:
#'  * 'IQQM - NSW 10,000 years' (default, among other things accepts a csv with a Date column followed by gauge columns)
#'  * 'Bigmod - MDBA'
#'  * 'Source - NSW (res.csv)'
#'  * 'IQQM - netcdf': in development
#' @param outputType list of strings or character vector defining what to save
#'   to disk. One or more of:
#'  * 'none' (default), do not save outputs- ignored if in a list with others
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents', # Does not work with EWR 2.0 tool
#'  * 'all_successful_interEvents'
#' @param returnType list of strings or character vector defining what to return
#'   to the active R session. Same options as `outputType`
#' @param datesuffix logical. whether to add a suffix to saved filenames to
#'   provide a datestamp. Should be deprecated in favour of metadata files.
#' @param scenario_filename_split character (including regex) to split the scenario filenames in the 'scenario' column of the EWR outputs. Mostly for handling the situation of gauge-named csvs with the scenario name appended to the front. When auto-appended, '_DIRECTORYAPPEND_' is used. If the appending has been done by the user, will need to pass in the split. The 'scenario' is given the first piece of the split, so do not use a split pattern in the scenario name, e.g. do not name scenarios a_1 and then append csvs with "_" like a_1_401234.csv, or the _1 will be lost..
#' @param extrameta list, extra information to include in saved metadata documentation for the run. Default NULL.
#' @param rparallel logical, default FALSE. If TRUE, parallelises over the scenarios in hydro_dir using `furrr`. To use, install `furrr` and set a [future::plan()] (likely `multisession` or `multicore`)
#'
#' @return a list of dataframe(s) if `returnType` is not 'none', otherwise, NULL
#' @export
#'
#' @examples
prep_run_save_ewrs <- function(hydro_dir, output_parent_dir, scenarios = NULL,
                               model_format = 'IQQM - NSW 10,000 years',
                               outputType = 'none', returnType = 'none',
                               scenario_filename_split = '_DIRECTORYAPPEND_',
                               extrameta = NULL,
                               datesuffix = FALSE,
                               rparallel = FALSE) {

  # Version checking is slow. Should we just immediately drop support?
  # users won't actually get here anyway since the arguments are changing
  # pypkgs <- reticulate::py_list_packages()
  # if ('py-ewr' %in% pypkgs$package) {
  #   ewrversion <- pypkgs$version[which(pypkgs$package == 'py-ewr')] |>
  #     stringr::str_extract("[0-9]*\\.[0-9]*") |>
  #     as.numeric()
  #
  #   rlang::abort("py-ewr updates have caused breaking changes that are no longer supported in this function. Please update py-ewr to > 2.1. If you need to use the old version, call `prep_run_save_ewrs_old`, but this will not be maintained.")
  # }


  # allow sloppy outputTypes and returnTypes
  if (!is.list(outputType)) {outputType <- as.list(outputType)}
  if (!is.list(returnType)) {returnType <- as.list(returnType)}

  # ensure the spellings and calls are consistent
  outputType <- make_ewr_consistent(outputType)
  returnType <- make_ewr_consistent(returnType)

  # get the paths to all the hydrographs. python used to need a list, and though
  # that's no longer true, it makes the now-required loops easier to use one in
  # R
  hydro_paths <- as.list(find_scenario_paths(hydro_dir))

  # allow passing in a vector of scenario names or getting them from hydro if NULL
  if (is.null(scenarios)) {
    scenarios <- scenario_names_from_hydro(hydro_dir)
  }

  # We need to check the files have unique names (and fix if not), since the EWR
  # tool makes them the 'scenario' column.
  hydro_paths <- fix_file_scenarios(hydro_paths, scenarios)


  # output_path doesn't get used for outputType == 'none', but we don't really
  # want to build directories in that case, so skip it.
  if (length(outputType) == 1 && outputType == 'none') {
    output_path <- '' # This shouldn't do anything
  } else {
    output_path <- make_output_dir(output_parent_dir, scenarios = scenarios,
                                   module_name = 'EWR', ewr_outtypes = unlist(outputType))
    # set up flags for the metadata in case the ewr fails partway
    init_params <- list(meta_message = "Started run, has not finished. New metadata file will write when it does. If this metadata entry persists, the run failed.",
                        ewr_status = FALSE,
                        time = format(Sys.time(), digits = 0, usetz = TRUE))
    yaml::write_yaml(init_params,
                     file = file.path(output_path, 'ewr_metadata.yml'))
    if (rlang::is_installed('jsonlite')) {
      jsonlite::write_json(init_params,
                           path = file.path(output_path, 'ewr_metadata.json'))
    } else {
      rlang::inform('json metadata not saved. If desired, install `jsonlite`',
                    .frequency = 'regularly', .frequency_id = 'jsoncheck')
    }

  }

  if (rparallel && !rlang::is_installed('furrr')) {
    rlang::warn("parallel processing over hydro_paths requires furrr. Please install it. Setting `parallel = FALSE` and proceeding")
    rparallel <- FALSE
  }

  # define this particular function with the current set of args, so the syntax
  # is simpler in furrr and purrr
  ewrfun <- function(x) {
    controller_functions$run_save_ewrs(x,
                                       output_path,
                                       model_format,
                                       outputType = outputType,
                                       returnType = returnType,
                                       scenario_filename_split = scenario_filename_split,
                                       datesuffix = datesuffix)
  }

  if (rparallel) {
    ewr_out <- furrr::future_map(hydro_paths, ewrfun,
                                 .options = furrr::furrr_options(seed = TRUE))

  } else {
    ewr_out <- purrr::map(hydro_paths, ewrfun)
  }

  # Clean up the list structure
  ewr_out <- purrr::list_transpose(ewr_out) |>
    purrr::map(dplyr::bind_rows)




  # auto-build metadata- this builds the data needed to run from params
  # And do it *after* the ewrs get processed so it only happens if the run works
  if (output_path != '') {
    if (!rlang::is_installed('git2r')) {
      rlang::inform('git sha not available. Install `git2r`',
                    .frequency = 'regularly', .frequency_id = 'git2rcheck')
      gitcom <- NULL
    }
    if (rlang::is_installed('git2r')) {
      gitcom <- try(git2r::sha(git2r::commits()[[1]]), silent = TRUE)
      if (inherits(gitcom, 'try-error')) {gitcom <- NULL}
    }


    ewr_params <- list(output_parent_dir = output_parent_dir,
                       hydro_dir = hydro_dir,
                       ewr_results = output_path,
                       model_format = model_format,
                       outputType = outputType,
                       returnType = returnType,
                       ewr_finish_time = format(Sys.time(), digits = 0, usetz = TRUE),
                       ewr_status = TRUE,
                       ewr_git_commit = gitcom)

    # add any passed metadata info
    if (is.list(extrameta)) {ewr_params <- utils::modifyList(ewr_params, extrameta)}

    # append any scenario metadata, so it all stays together
    ymlscenepath <- list.files(hydro_dir, pattern = "*.yml")
    if (length(ymlscenepath) != 0) {
      ymlscenes <- file.path(hydro_dir, ymlscenepath) |>
        yaml::read_yaml()
    } else {
      ymlscenes <- NULL
    }

    yaml::write_yaml(c(ymlscenes, ewr_params),
                     file = file.path(output_path, 'ewr_metadata.yml'))

    if (rlang::is_installed('jsonlite')) {
      jsonscenepath <- list.files(hydro_dir, pattern = "*.json")
      if (length(jsonscenepath) != 0) {
        jsonscenes <- file.path(hydro_dir, jsonscenepath) |>
          jsonlite::read_json()
      } else {
        jsonscenes <- NULL
      }
      jsonlite::write_json(c(jsonscenes, ewr_params),
                           path = file.path(output_path, 'ewr_metadata.json'))
    } else {
      rlang::inform('json metadata not saved. If desired, install `jsonlite`',
                    .frequency = 'regularly', .frequency_id = 'jsoncheck')
    }

  }


  return(ewr_out)
}

make_ewr_consistent <- function(typearg) {
  typearg <- unlist(typearg)
  typearg <- dplyr::case_when(typearg == 'all' ~ 'all_events',
                              typearg == 'annual' ~ 'yearly',
                              typearg == 'successful' ~ 'all_successful_events',
                              typearg == 'all_interevents' ~ 'all_interEvents',
                              typearg == 'all_successful_interevents' ~ 'all_successful_interEvents',
                              .default = typearg)
  typearg <- as.list(typearg)

}


#' Set up, run, and (possibly) save EWR outputs
#'
#' This does some directory setup and parsing, runs the EWR tool and, if asked,
#' saves the output. If the output saves, it also auto-saves both yaml and json
#' metadata files with all parameters needed to run this part of the toolkit
#' with parameters. Scenario metadata is prepended, if found.
#'
#' @param hydro_dir Directory containing hydrographs. Can be an outer directory,
#'   e.g. `hydrographs` that splits into scenario subdirs, or can be a single
#'   scenario subdir.
#' @param output_parent_dir parent directory for the outputs. Can be anything,
#'   but two typical cases:
#'  * The directory containing `hydro_dir`, which puts the `module_outputs` at the same level as the hydrographs
#'  * If running in batches for single scenarios, may be `hydro_dir`, which just puts the `module_outputs` in `hydro_dir`
#' @param scenarios `NULL` (default) or character.
#'  * `NULL`- finds scenario names by parsing directory names in `hydro_dir`. If no internal directories, just stays in `hydro_dir`. This captures the two typical situations discussed for `output_parent_dir`. If there are other directories in `hydro_dir` that do not contain hydrological scenarios, should use a character vector.
#'  * character vector of scenario names. This allows unusual directory structures.
#' @param model_format see EWR tool. One of:
#'  * 'IQQM - NSW 10,000 years' (default)
#'  * 'Bigmod - MDBA'
#'  * 'Source - NSW (res.csv)'
#' @param climate see EWR tool. One of:
#'  * 'Standard - 1911 to 2018 climate categorisation' (default, with categories)
#'  * 'NSW 10,000 year climate sequence' (not categorised, just a blank csv for 10,000 years that allows it to run)
#' @param outputType list of strings or character vector defining what to save
#'   to disk. One or more of:
#'  * 'none' (default), do not save outputs- ignored if in a list with others
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents', # Does not work with current EWR tool
#'  * 'all_successful_interEvents'
#' @param returnType list of strings or character vector defining what to return
#'   to the active R session. Same options as `outputType`
#' @param MINT see EWR
#' @param MAXT see EWR
#' @param DUR see EWR
#' @param DRAW see EWR
#' @param datesuffix logical. whether to add a suffix to saved filenames to
#'   provide a datestamp. Should be deprecated in favour of metadata files.
#' @param scenario_filename_split character (including regex) to split the scenario filenames in the 'scenario' column of the EWR outputs. Mostly for handling the situation of gauge-named csvs with the scenario name appended to the front. When auto-appended, '_DIRECTORYAPPEND_' is used. If the appending has been done by the user, will need to pass in the split. The 'scenario' is given the first piece of the split, so do not use a split pattern in the scenario name, e.g. do not name scenarios a_1 and then append csvs with "_" like a_1_401234.csv, or the _1 will be lost..
#' @param extrameta list, extra information to include in saved metadata documentation for the run. Default NULL.
#' @param rparallel logical, default FALSE. If TRUE, parallelises over the scenarios in hydro_dir using `furrr`. To use, install `furrr` and set a [future::plan()] (likely `multisession` or `multicore`)
#'
#' @return a list of dataframe(s) if `returnType` is not 'none', otherwise, NULL
#' @export
#'
#' @examples
prep_run_save_ewrs_old <- function(hydro_dir, output_parent_dir, scenarios = NULL,
                               model_format = 'IQQM - NSW 10,000 years',
                               climate = 'Standard - 1911 to 2018 climate categorisation',
                               outputType = 'none', returnType = 'none',
                               MINT = (100 - 0)/100, MAXT = (100 + 0 )/100,
                               DUR = (100 - 0 )/100, DRAW = (100 -0 )/100,
                               scenario_filename_split = '_DIRECTORYAPPEND_',
                               extrameta = NULL,
                               datesuffix = FALSE,
                               rparallel = FALSE) {

  # allow sloppy outputTypes and returnTypes
  if (!is.list(outputType)) {outputType <- as.list(outputType)}
  if (!is.list(returnType)) {returnType <- as.list(returnType)}

  # ensure the spellings and calls are consistent
  outputType <- make_ewr_consistent(outputType)
  returnType <- make_ewr_consistent(returnType)

  # dicts in py come from named lists in R
  allowance <- list(minThreshold = MINT, maxThreshold = MAXT,
                    duration = DUR, drawdown = DRAW)

  # get the paths to all the hydrographs. python needs a list, not a vector
  hydro_paths <- as.list(find_scenario_paths(hydro_dir))

  # allow passing in a vector of scenario names or getting them from hydro if NULL
  if (is.null(scenarios)) {
    scenarios <- scenario_names_from_hydro(hydro_dir)
  }

  # We need to check the files have unique names (and fix if not), since the EWR
  # tool makes them the 'scenario' column.
  hydro_paths <- fix_file_scenarios(hydro_paths, scenarios)


  # output_path doesn't get used for outputType == 'none', but we don't really
  # want to build directories in that case, so skip it.
  if (length(outputType) == 1 && outputType == 'none') {
    output_path <- '' # This shouldn't do anything
  } else {
    output_path <- make_output_dir(output_parent_dir, scenarios = scenarios,
                                   module_name = 'EWR', ewr_outtypes = unlist(outputType))
    # set up flags for the metadata in case the ewr fails partway
    init_params <- list(meta_message = "Started run, has not finished. New metadata file will write when it does. If this metadata entry persists, the run failed.",
                        ewr_status = FALSE,
                        time = format(Sys.time(), digits = 0, usetz = TRUE))
    yaml::write_yaml(init_params,
                     file = file.path(output_path, 'ewr_metadata.yml'))
    if (rlang::is_installed('jsonlite')) {
      jsonlite::write_json(init_params,
                           path = file.path(output_path, 'ewr_metadata.json'))
    } else {
      rlang::inform('json metadata not saved. If desired, install `jsonlite`',
                    .frequency = 'regularly', .frequency_id = 'jsoncheck')
    }

  }

  if (rparallel && !rlang::is_installed('furrr')) {
    rlang::warn("parallel processing over hydro_paths requires furrr. Please install it. Setting `parallel = FALSE` and proceeding")
    rparallel <- FALSE
  }

  if (rparallel) {
    ewr_out <- furrr::future_map(hydro_paths, \(x) controller_functions$run_save_ewrs_old(list(x), output_path,
                                                                                      model_format, allowance, climate,
                                                                                      outputType = outputType,
                                                                                      returnType = returnType,
                                                                                      scenario_filename_split = scenario_filename_split,
                                                                                      datesuffix = datesuffix),
                                 .options = furrr::furrr_options(seed = TRUE))

    ewr_out <- purrr::list_transpose(ewr_out) |>
      purrr::map(dplyr::bind_rows)

  } else {
    ewr_out <- controller_functions$run_save_ewrs_old(hydro_paths, output_path,
                                                  model_format, allowance, climate,
                                                  outputType = outputType,
                                                  returnType = returnType,
                                                  scenario_filename_split = scenario_filename_split,
                                                  datesuffix = datesuffix)
  }




  # auto-build metadata- this builds the data needed to run from params
  # And do it *after* the ewrs get processed so it only happens if the run works
  if (output_path != '') {
    if (!rlang::is_installed('git2r')) {
      rlang::inform('git sha not available. Install `git2r`',
                    .frequency = 'regularly', .frequency_id = 'git2rcheck')
      gitcom <- NULL
    }
    if (rlang::is_installed('git2r')) {
      gitcom <- try(git2r::sha(git2r::commits()[[1]]), silent = TRUE)
      if (inherits(gitcom, 'try-error')) {gitcom <- NULL}
    }


    ewr_params <- list(output_parent_dir = output_parent_dir,
                       hydro_dir = hydro_dir,
                       ewr_results = output_path,
                       model_format = model_format,
                       climate = climate,
                       outputType = outputType,
                       returnType = returnType,
                       ewr_finish_time = format(Sys.time(), digits = 0, usetz = TRUE),
                       ewr_status = TRUE,
                       ewr_git_commit = gitcom)

    # add any passed metadata info
    if (is.list(extrameta)) {ewr_params <- utils::modifyList(ewr_params, extrameta)}

    # append any scenario metadata, so it all stays together
    ymlscenepath <- list.files(hydro_dir, pattern = "*.yml")
    if (length(ymlscenepath) != 0) {
      ymlscenes <- file.path(hydro_dir, ymlscenepath) |>
        yaml::read_yaml()
    } else {
      ymlscenes <- NULL
    }

    yaml::write_yaml(c(ymlscenes, ewr_params),
                     file = file.path(output_path, 'ewr_metadata.yml'))

    if (rlang::is_installed('jsonlite')) {
      jsonscenepath <- list.files(hydro_dir, pattern = "*.json")
      if (length(jsonscenepath) != 0) {
        jsonscenes <- file.path(hydro_dir, jsonscenepath) |>
          jsonlite::read_json()
      } else {
        jsonscenes <- NULL
      }
      jsonlite::write_json(c(jsonscenes, ewr_params),
                           path = file.path(output_path, 'ewr_metadata.json'))
    } else {
      rlang::inform('json metadata not saved. If desired, install `jsonlite`',
                    .frequency = 'regularly', .frequency_id = 'jsoncheck')
    }

  }


  return(ewr_out)
}
