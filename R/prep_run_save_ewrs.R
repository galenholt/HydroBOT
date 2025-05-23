# Wrap the python

## NOTE: this imports from system.file, and so while developing won't pick up
## new versions without `install`ing the package. The `future` uses seem most
## sensitive to this- I think maybe test shims don't work right?
controller_functions <- reticulate::import_from_path("controller_functions",
  path = system.file("python",
    package = "HydroBOT"
  ),
  delay_load = TRUE
)

#' Set up, run, and (possibly) save EWR outputs
#'
#' This does some directory setup and parsing, runs the EWR tool and, if asked,
#' saves the output. If the output saves, it also auto-saves both yaml and json
#' metadata files with all parameters needed to run this part of HydroBOT
#' with parameters. Scenario metadata is prepended, if found.
#'
#' By far the cleanest way for this to work is to have your input hydrographs in
#' a file structure with the directories defining the scenarios, and single or
#' multiple hydrograph files within them. I.e. a structure that does not mix
#' files of different scenarios in the final directory. If you have that
#' structure, using `scenarios_from = 'directory'` will ensure your scenarios
#' are named uniquely and output files are also unique and not mixed between
#' scenarios. This is particularly important for parallelising, which depends on
#' parallelling over scenarios. This structure is then retained in the output
#' structure, making aggregation simpler as well. If for some reason you
#' *cannot* establish this structure, set `scenarios_from = 'file'`, and
#' everythign will be given a unique name, but your life will probably be
#' difficult when aggregating and other subsequent processing, requiring more
#' work in scripts to make the appropriate comparisons.
#'
#' @param hydro_dir Directory containing hydrographs. Can be an outer directory,
#'   e.g. `hydrographs` that splits into scenario subdirs, or can be a single
#'   scenario subdir.
#' @param output_parent_dir parent directory for the outputs. Can be anything,
#'   but two typical cases:
#'  * The directory containing `hydro_dir`, which puts the `module_outputs` at the same level as the hydrographs
#'  * If running in batches for single scenarios, may be `hydro_dir`, which just puts the `module_outputs` in `hydro_dir`
#' @param output_subdir a sub-directory for the outputs, if for example we want
#'   `module_output/EWR/V1` and `module_output/EWR/V2`
#' @param scenarios `NULL` (default) or named list.
#'  * `NULL`- finds scenario names by parsing directory names in `hydro_dir`. If no internal directories, just stays in `hydro_dir`. This captures the two typical situations discussed for `output_parent_dir`. If there are other directories in `hydro_dir` that do not contain hydrological scenarios, should use a character vector.
#'  * named list of paths to files. names become scenario names, paths should be relative to `hydro_dir`. This allows unusual directory structures.
#' @param model_format see EWR tool. One of:
#'  * 'Standard time-series': (default, among other things accepts a csv with a Date column followed by gauge columns, with _flow or _level appended to the gauge number)
#'  * 'IQQM - netcdf': in development, finds all netcdf files in `hydro_dir`. Should also work when `hydro_dir` is a .zip with netcdfs inside
#'  * 'ten thousand year': old default (IQQM - NSW 10,000 years), works nearly the same as standard time-series
#'  * 'All Bigmod': previously 'Bigmod - MDBA'
#'  * 'Source - NSW (res.csv)'
#' @param outputType list of strings or character vector defining what to save
#'   to disk. One or more of:
#'  * 'none' (default), do not save outputs- ignored if in a list with others
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents'
#'  * 'all_successful_interEvents'
#' @param returnType list of strings or character vector defining what to return
#'   to the active R session. Same options as `outputType`
#' @param scenarios_from character, default 'directory' gets scenario names from
#'   directory names. If anything else, gets them from filenames (safest).
#'   Expect additional options in future, e.g from metadata.
#' @param file_search character, regex for additional limitations on filenames.
#'   Useful to run a subset of scenarios or if several files have the extension
#'   defined by `model_format`, but only some are hydrographs.
#' @param fill_missing logical, default FALSE. If TRUE, figures out the expected
#'   outputs and only runs those that are missing. Useful for long runs that
#'   might break.
#' @param extrameta list, extra information to include in saved metadata
#'   documentation for the run. Default NULL.
#' @param rparallel logical, default FALSE. If TRUE, parallelises over the
#'   scenarios in hydro_dir using `furrr`. To use, install `furrr` and set a
#'   [future::plan()] (likely `multisession` or `multicore`)
#' @param retries Number of retries if there are errors. 0 is no retries, but
#'   still runs once. Default 2.
#' @param print_runs logical, default FALSE. If true, print the set of runs to
#'   be done.
#' @param url logical, default FALSE. If TRUE, `scenarios` needs to be a named
#'   list with full file paths (URLs). This bypasses the otherwise automatic
#'   prepending of hydro_dir onto a named scenario list.
#'
#' @return a list of dataframe(s) if `returnType` is not 'none', otherwise, NULL
#' @export
prep_run_save_ewrs <- function(hydro_dir,
                               output_parent_dir,
                               output_subdir = '',
                               scenarios = NULL,
                               model_format = "Standard time-series",
                               outputType = "none",
                               returnType = "none",
                               scenarios_from = 'directory',
                               file_search = NULL,
                               fill_missing = FALSE,
                               extrameta = NULL,
                               rparallel = FALSE,
                               retries = 2,
                               print_runs = FALSE,
                               url = FALSE) {

  # allow sloppy outputTypes and returnTypes
  if (!is.list(outputType)) {
    outputType <- as.list(outputType)
  }
  if (!is.list(returnType)) {
    returnType <- as.list(returnType)
  }

  # ensure the spellings and calls are consistent
  outputType <- make_ewr_consistent(outputType)
  returnType <- make_ewr_consistent(returnType)

  # python and R put ~ in different places
  if (grepl('~', hydro_dir)) {
    hydro_dir <- path.expand(hydro_dir)
  }
  if (grepl('~', output_parent_dir)) {
    output_parent_dir <- path.expand(output_parent_dir)
  }

  # get the paths to all the hydrographs. python used to need a list, and though
  # that's no longer true, it makes the now-required loops easier to use one in
  # R
  if (is.null(scenarios)) {
    if (model_format %in% c("IQQM - NSW 10,000 years",
                            "All Bigmod",
                            "ten thousand year",
                            "Standard time-series",
                            "Bigmod - MDBA")) {
      filetype <- "csv"
    }
    if (grepl("netcdf", model_format)) {
      filetype <- "nc"
    }
    hydro_paths <- find_scenario_paths(hydro_dir, type = filetype,
                                       scenarios_from = scenarios_from,
                                       file_search = file_search)
  } else {
    # if files are from URL (blob), we have to pre-create the paths
    if (url) {
      hydro_paths <- scenarios
    }
    if (!url) {
      hydro_paths <- purrr::map(scenarios, \(x) file.path(hydro_dir, x))
    }
  }

  # We need to check the files have unique names (and fix if not), since the EWR
  # tool makes them the 'scenario' column.
  # hydro_paths <- fix_file_scenarios(hydro_paths, scenarios)
  if (any(duplicated(hydro_paths))) {
    rlang::abort(glue::glue("The {hydro_paths[duplicated(hydro_paths)]} are duplicated. Fix your directory structure."))
  }

  # output_path doesn't get used for outputType == 'none', but we don't really
  # want to build directories in that case, so skip it.
  if (length(outputType) == 1 && outputType == "none") {
    output_path <- "" # This shouldn't do anything
  } else {
    output_path <- make_output_dir(output_parent_dir,
      scenarios = names(hydro_paths),
      module_name = "EWR",
      subdir = output_subdir,
      ewr_outtypes = unlist(outputType)
    )

    # Adjust if filling missing
    if (fill_missing) {
      missing_scenarios <- find_missing_runs(hydro_paths, output_path, outputType, scenarios_from)

      hydro_paths <- hydro_paths[names(hydro_paths) %in% missing_scenarios]
    }


    # set up flags for the metadata in case the ewr fails partway
    init_params <- list(
      meta_message = "Started run, has not finished. New metadata file will write when it does. If this metadata entry persists, the run failed.",
      ewr_status = FALSE,
      time = format(Sys.time(), digits = 0, usetz = TRUE)
    )
    yaml::write_yaml(init_params,
      file = file.path(output_path, "ewr_metadata.yml")
    )
    if (rlang::is_installed("jsonlite")) {
      jsonlite::write_json(init_params,
        path = file.path(output_path, "ewr_metadata.json")
      )
    } else {
      rlang::inform("json metadata not saved. If desired, install `jsonlite`",
        .frequency = "regularly", .frequency_id = "jsoncheck"
      )
    }
  }

  if (rparallel && !rlang::is_installed("furrr")) {
    rlang::warn("parallel processing over hydro_paths requires furrr. Please install it. Setting `parallel = FALSE` and proceeding")
    rparallel <- FALSE
  }

  # define this particular function with the current set of args, so the syntax
  # is simpler in furrr and purrr
  ewrfun <- function(x, y) {
    # x <- file.path(hydro_dir, x)
    controller_functions$run_save_ewrs(x,
      output_path,
      model_format,
      outputType = outputType,
      returnType = returnType,
      scenario_name = y,
      scenarios_from = scenarios_from
    )
  }


  # There's an approximately 260 character path limit on windows for python. Try to detect here (I do it there too)
  unout <- unlist(outputType)
  unout <- unout[which(nchar(unout) == max(nchar(unout)))]
  approx_py_path <- paste0(output_path, '\\', names(hydro_paths[1]),'\\', unout, '\\', names(hydro_paths[1]), '.csv')
  if (nchar(approx_py_path) >= 260 & .Platform$OS.type == 'windows') {
    rlang::warn(glue::glue('Output path is {nchar(approx_py_path)}, windows has about a 260 limit. If files are not saving, try a shorter path.'))
  }

  # Spit out info if requested
  if (print_runs) {

    if (length(hydro_paths) < 10) {
      runprint <- names(hydro_paths)
    } else {
      runprint <- c(glue::glue("{names(hydro_paths)[1:10]}"),
                    glue::glue("and {length(hydro_paths) - 10} more"))
    }
    rlang::inform(c(glue::glue("{length(hydro_paths)} scenarios to run:"),
                    runprint))
  }


  # Run the EWR tool over all hydro_paths
  # This is abandoned for now, could put an auto cluster looper in here.
  # inner_imap <- function()
  # if (outer_parallel > 1) {
  #   nodeloops <- split(fulloop, cut(1:length(fulloop), nodes_wanted, labels = FALSE))
  # }
  ewr_out <- safe_imap(hydro_paths, ewrfun, retries = retries, parallel = rparallel)

  # save metadata immediately after processing so any errors in the returning don't prevent its creation.
  # auto-build metadata- this builds the data needed to run from params
  # And do it *after* the ewrs get processed so it only happens if the run works
  if (output_path != "") {

    ewr_params <- list(
      output_parent_dir = output_parent_dir,
      hydro_dir = hydro_dir,
      output_path = output_path,
      model_format = model_format,
      outputType = outputType,
      returnType = returnType,
      finish_time = format(Sys.time(), digits = 0, usetz = TRUE),
      status = TRUE,
      ewr_version = get_ewr_version(),
      HydroBOT_version = as.character(utils::packageVersion('HydroBOT'))
    )

    # add any passed metadata info
    if (is.list(extrameta)) {
      ewr_params <- utils::modifyList(ewr_params, extrameta)
    }

    # append any scenario metadata, so it all stays together
    ymlscenepath <- list.files(hydro_dir, pattern = "*.yml")
    if (length(ymlscenepath) != 0) {
      ymlscenes <- file.path(hydro_dir, ymlscenepath) |>
        yaml::read_yaml()
    } else {
      ymlscenes <- NULL
    }

    yaml::write_yaml(list(scenarios = ymlscenes, ewr = ewr_params),
                     file = file.path(output_path, "ewr_metadata.yml")
    )

    if (rlang::is_installed("jsonlite")) {
      jsonscenepath <- list.files(hydro_dir, pattern = "*.json")
      if (length(jsonscenepath) != 0) {
        jsonscenes <- file.path(hydro_dir, jsonscenepath) |>
          jsonlite::read_json()
      } else {
        jsonscenes <- NULL
      }
      jsonlite::write_json(c(jsonscenes, ewr_params),
                           path = file.path(output_path, "ewr_metadata.json")
      )
    } else {
      rlang::inform("json metadata not saved. If desired, install `jsonlite`",
                    .frequency = "regularly", .frequency_id = "jsoncheck"
      )
    }
  }

  ## CLEAN DATA FOR RETURN
  # rearrange to be a list of the different types of output, instead of the different scenarios
  ewr_out <- purrr::list_transpose(ewr_out) |>
    purrr::map(dplyr::bind_rows)

  # list cleanup in R is needed to get it to look like it does when read in from
  # csv. Some of these steps can take a while, so don't do them if not
  # returning.
  if (returnType[[1]] != "none") {
    ewr_out <- clean_ewr_in_R(ewr_out)
  }

  return(ewr_out)
}

#' Clean up the EWR outputs so returns look like they do when read from csv. Mostly dateparsing from py objects
#'
#' @param ewr_out EWR output dataframe as returned directly from python to R
#'
#' @return tibble, cleaned EWR outputs
#' @keywords internal
clean_ewr_in_R <- function(ewr_out) {
  # some ewr outputs have list-columns, and sometimes within those columns are
  # python datetime objects. Why isn't reticulate translating them? One option
  # is to turn them into characters in python. The other is to do it with
  # `reticulate::py_to_r()`. I like the idea of keeping dates dates, but if
  # the translation is slow (it may well be), we can move to the python
  # translation (and anything read from csv will be like that anyway). The
  # annoying thing is they're py objects inside an R list-column, and so we
  # need to drill in a ways. The plan here is to purrr over ewr_out, and
  # tidyr::unnest list-columns. But first we need to make those lists
  # R-objects if they're py

  # To do that, we need to find those list-cols, and then purrr down them.
  ispydatelist <- function(x) {
    is.list(x) & is.environment(x[[1]])
  }

  # do this to the lists (which are embeddedin dataframes)
  fixpydatelist <- function(pl) {
    pl |>
      purrr::map(reticulate::py_to_r) |>
      purrr::list_simplify()
  }

  # do this to the dataframes (which are embedded in the list of ewr outputs)
  fixpydate <- function(df, dfn) {
    listrows <- nrow(df)
    # catch a 0-row edge case
    if (listrows == 0) {
      return(df |> tibble::tibble())
    }
    df <- df |>
      dplyr::mutate(dplyr::across(dplyr::where(ispydatelist), fixpydatelist)) |> # The python dates
      tidyr::unnest(cols = dplyr::where(is.list)) |> # other list-cols
      tibble::tibble() # for consistency

    # check that the unlisting hasn't changed the data- could happen if the list-cols have multiple numbers per cell
    unlistrows <- nrow(df)
    if (unlistrows != listrows) {
      rlang::inform(glue::glue("Unlisting list-columns in EWR output {dfn} has caused rows to change from {listrows} to {unlistrows}. Check that this is expected behaviour."))
    }

    return(df)
  }

  # do this over the list of ewr outputs
  # use imap so the `inform` can have the name of the sheet.
  ewr_out <- ewr_out |>
    purrr::imap(fixpydate)
}

#' handle some different naming conventions
#'
#' @param typearg list of characters for the EWR types
#'
#' @return list of EWR types with standard names
#' @keywords internal
make_ewr_consistent <- function(typearg) {
  if (length(typearg) == 1 && typearg[[1]] == 'everything') {
    typearg <- list('summary',
                    'yearly',
                    'all_events',
                    'all_successful_events',
                    'all_interEvents',
                    'all_successful_interEvents')
  }

  typearg <- unlist(typearg)
  typearg <- dplyr::case_when(typearg == "all" ~ "all_events",
    typearg == "annual" ~ "yearly",
    typearg == "successful" ~ "all_successful_events",
    typearg == "all_interevents" ~ "all_interEvents",
    typearg == "all_successful_interevents" ~ "all_successful_interEvents",
    .default = typearg
  )
  typearg <- as.list(typearg)
}


