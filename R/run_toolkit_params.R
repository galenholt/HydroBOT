#' Run the toolkit through the aggregation step with a file of yaml parameters
#' or passing in yaml as characters.
#'
#' The plan is for this to make scripting easier, and be able to change single
#' simple parameters (e.g. directory) from command line or in other workflows
#'
#' @param yamlpath character, path to yaml file of parameters. Only needs to
#'   contain values for parameters for which we wish to modify `defaults`
#' @param passed_args character, in yaml format, e.g. ""scenario_dir:
#'   'project_dir/hydrographs'". Likely possible to pass more than one, but
#'   complicated to get the yaml right. Suggest in that case modifying the
#'   params.yml
#' @param defaults character, path to default set of yaml parameters, so the
#'   `yamlpath` file and `passed_args` only need to include those that are
#'   modified
#' @param list_args list of arguments. Typically would come in from
#'   parameterised quarto notebook, which pre-parses yaml into a list
#'
#' @return runs the toolkit, returns NULL invisibly, or the aggregated output if the params have `aggReturn: TRUE`
#' @export
#'
#' @examples
run_toolkit_params <- function(yamlpath = NULL,
                               passed_args = NULL,
                               list_args = NULL,
                               defaults = system.file('yml/default_params.yml', package = 'werptoolkitr')) {

  # I could have a 'defaults' file and then a params that just changes some.
  # Maybe later. Might make a lot of sense if the only thing being passed in is
  # one value.

  # Default params
  arglist <- yaml::read_yaml(defaults)

  # Read in yaml params file from user
  useryml <- yaml::read_yaml(yamlpath)

  arglist <- modifyList(arglist, useryml)

  # Allow user to pass yaml at the command line
  comargs <- yaml::yaml.load(passed_args)

  # Replace the arglist vals with passed args from command line
  if (is.list(comargs)) {arglist <- modifyList(arglist, comargs)}

  # bring in list-args (especially useful for parameterised quarto notebook)
  if (is.list(list_args)) {arglist <- modifyList(arglist, list_args)}

  # R file contains the aggregation definition, since it may need R types
  source(arglist$aggregation_def)

  # Some default modifications need R functions
  arglist <- make_default_args(arglist)

  # Type cleanup
  arglist <- type_cleanup(arglist)

  ewr_out <- prep_run_save_ewrs_R(scenario_dir = arglist$hydro_dir,
                                  output_dir = arglist$scenario_dir,
                                  outputType = arglist$outputType,
                                  returnType = arglist$returnType,
                                  climate = arglist$climate)

  aggout <- read_and_agg(datpath = arglist$ewr_results,
                         type = arglist$aggType,
                         geopath = werptoolkitr::bom_basin_gauges,
                         causalpath = werptoolkitr::causal_ewr,
                         groupers = arglist$agg_groups,
                         aggCols = arglist$agg_var,
                         aggsequence = aggseq,
                         funsequence = funseq,
                         saveintermediate = TRUE,
                         namehistory = arglist$namehistory,
                         keepAllPolys = arglist$keepAllPolys,
                         returnList = arglist$aggReturn,
                         savepath = arglist$agg_results)

  if (arglist$aggReturn) {
    return(aggout)
  } else {
    return(invisible())
  }

}

#
#' Helper to allow making the directory structure programatically from the base directory
#'
#' @param arglist
#'
#' @examples
make_default_args <- function(arglist) {
  if (arglist$hydro_dir == 'default' || is.null(arglist$hydro_dir)) {
    arglist$hydro_dir = file.path(arglist$scenario_dir, 'hydrographs')
  }

  if (arglist$ewr_results == 'default' || is.null(arglist$ewr_results)) {
    arglist$ewr_results <- file.path(arglist$scenario_dir, 'module_output', 'EWR')
  }

  if (arglist$agg_results == 'default' || is.null(arglist$agg_results)) {
    arglist$agg_results <- file.path(arglist$scenario_dir, 'aggregator_output')
  }

  return(arglist)
}

#
#' Sometimes single values come in as characters instead of lists
#'
#' @param arglist
#'
#' @examples
type_cleanup <- function(arglist) {

  if (!is.list(arglist$returnType)) {arglist$returnType <- as.list(arglist$returnType)}

  if (!is.list(arglist$outputType)) {arglist$outputType <- as.list(arglist$outputType)}

  return(arglist)
}
