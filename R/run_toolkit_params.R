#' Run the toolkit through the aggregation step with a file of yaml parameters
#' or passing in yaml as characters.
#'
#' The plan is for this to make scripting easier, and be able to change single
#' simple parameters (e.g. directory) from command line or in other workflows
#'
#' @param yamlpath character, path to yaml file of parameters. Only needs to
#'   contain values for parameters for which we wish to modify `defaults`. If we
#'   want to be able to auto-run off the saved param metadata, the names of the
#'   spatial agg levels need to match the objects, e.g. basin: basin
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
#' @return runs the toolkit, returns NULL invisibly, or the aggregated output if
#'   the params have `aggReturn: TRUE`
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

  # R file allows aggregation definition with R types and expressions. This is
  # the most general, but also requires extra files and makes handling metadata
  # trickier
  if (!is.null(arglist$aggregation_def)) {
    if (file.exists(arglist$aggregation_def)) {
      source(arglist$aggregation_def, local = TRUE)
    } else if ( file.exists(file.path('inst', arglist$aggregation_def))) {
      source(file.path('inst', arglist$aggregation_def), local = TRUE)
    } else {
      rlang::abort(glue::glue('requested parameter R file {arglist$aggregation_def} does not exist.'))
    }

    # check others
    if (!is.null(arglist$aggregation_sequence)) {
      rlang::warn("Aggregation sequence defined in both an R file and yml. R file supersedes, but best to only use one.")
    }
    if (!is.null(arglist$aggregation_funsequence)) {
      rlang::warn("Aggregation funsequence defined in both an R file and yml. R file supersedes, but best to only use one.")
    }
  }

  if (is.null(arglist$aggregation_def)) {
    aggseq <- arglist$aggregation_sequence
    funseq <- arglist$aggregation_funsequence

    # Some attempts at simple cleanup- funseq sometimes comes in as a vector
    # from the yaml if we only have one function
    if (!inherits(funseq, 'list')) {
      if (length(funseq) == length(aggseq)) {
        funseq <- as.list(aggseq)
      } else {
        rlang::abort("Function sequence is not a list and cannot obviously be translated into a list of the same length as the aggregation steps. Try to re-specify. The yaml is often easier to enforce a list using names.")
      }
    }

  }

  # catch edges
  if (is.null(aggseq) | is.null(funseq)) {
    rlang::abort("aggregation sequence or funsequence not defined in either the aggregation_def slot or the aggregation_sequence and aggregation_funsequence")
  }


  # Some default modifications need R functions
  arglist <- make_default_args(arglist)

  # Type cleanup
  arglist <- type_cleanup(arglist)

  ewr_out <- prep_run_save_ewrs(hydro_dir = arglist$hydro_dir,
                                  output_parent_dir = arglist$output_parent_dir,
                                  outputType = arglist$outputType,
                                  returnType = arglist$returnType,
                                  climate = arglist$climate,
                                extrameta = list(ewr_run_from_params = TRUE,
                                                 ewr_param_default = defaults,
                                                 ewr_param_yml = yamlpath,
                                                 ewr_param_passed = passed_args,
                                                 ewr_param_list = list_args))

  aggout <- read_and_agg(datpath = arglist$agg_input_path,
                         type = arglist$aggType,
                         geopath = bom_basin_gauges,
                         causalpath = causal_ewr,
                         groupers = arglist$agg_groups,
                         aggCols = arglist$agg_var,
                         aggsequence = aggseq,
                         funsequence = funseq,
                         saveintermediate = TRUE,
                         namehistory = arglist$namehistory,
                         keepAllPolys = arglist$keepAllPolys,
                         returnList = arglist$aggReturn,
                         savepath = arglist$agg_results,
                         extrameta = list(agg_run_from_params = TRUE,
                                          agg_param_default = defaults,
                                          agg_param_yml = yamlpath,
                                          agg_param_passed = passed_args,
                                          agg_param_list = list_args))

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
    arglist$hydro_dir = file.path(arglist$output_parent_dir, 'hydrographs')
  }

  if (arglist$agg_input_path == 'default' || is.null(arglist$agg_input_path)) {
    arglist$agg_input_path <- file.path(arglist$output_parent_dir, 'module_output', 'EWR')
  }

  if (arglist$agg_results == 'default' || is.null(arglist$agg_results)) {
    arglist$agg_results <- file.path(arglist$output_parent_dir, 'aggregator_output')
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
