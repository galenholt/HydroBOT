# Wrap the python
controller_functions <- reticulate::import_from_path("controller_functions",
                                                     path = system.file("python",
                                                                        package = 'werptoolkitr'),
                                                     delay_load = TRUE)

#' Title
#'
#' @param hydro_dir Directory containing hydrographs. Can be an outer directory, e.g. `hydrographs` that splits into scenario subdirs, or can be a single scenario subdir.
#' @param output_parent_dir parent directory for the outputs. Can be anything, but two typical cases:
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
#' @param outputType list of strings or character vector defining what to save to disk. One or more of:
#'  * 'none' (default), do not save outputs- ignored if in a list with others
#'  * 'summary' saves the EWR outcomes summarised over the whole period
#'  * 'all' saves the EWR all events
#'  * 'annual' does not work with current EWR
#' @param returnType list of strings or character vector defining what to return to the active R session. Same options as `outputType`
#' @param MINT see EWR
#' @param MAXT see EWR
#' @param DUR see EWR
#' @param DRAW see EWR
#' @param datesuffix logical. whether to add a suffix to saved filenames to provide a datestamp. Should be deprecated in favour of metadata files.
#'
#' @return a list of dataframe(s) if `returnType` is not 'none', otherwise, NULL
#' @export
#'
#' @examples
prep_run_save_ewrs <- function(hydro_dir, output_parent_dir, scenarios = NULL,
                               model_format = 'IQQM - NSW 10,000 years',
                               climate = 'Standard - 1911 to 2018 climate categorisation',
                               outputType = 'none', returnType = 'none',
                               MINT = (100 - 0)/100, MAXT = (100 + 0 )/100,
                               DUR = (100 - 0 )/100, DRAW = (100 -0 )/100,
                               datesuffix = FALSE) {

  # allow sloppy outputTypes and returnTypes
  if (!is.list(outputType)) {outputType <- as.list(outputType)}
  if (!is.list(returnType)) {returnType <- as.list(returnType)}

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


  # output_path doesnt get used for outputType == 'none', but we don't really
  # want to build directories in that case, so skip it.
  if (length(outputType) == 1 && outputType == 'none') {
    output_path <- '' # This shouldn't do anything
  } else {
    output_path <- make_output_dir(output_parent_dir, scenarios = scenarios, module_name = 'EWR')
  }

  ewr_out <- controller_functions$run_save_ewrs(hydro_paths, output_path,
                                                model_format, allowance, climate,
                                                outputType = outputType,
                                                returnType = returnType,
                                                datesuffix = datesuffix)

  return(ewr_out)
}
