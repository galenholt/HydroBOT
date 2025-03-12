#' Run HydroBOT through the aggregation step with a file of yaml parameters or
#' passing in yaml as characters.
#'
#' The goal here is to make scripting easier, and be able to change single
#' simple parameters (e.g. directory) from command line or in other workflows.
#' - Needs lists with top-level `ewr` and `aggregation` items. Those should then be lists with items named as in function arguments for [prep_run_save_ewrs()] and [read_and_agg()], respectively.
#'
#' @param yamlpath character, path to yaml file of parameters. Only needs to
#'   contain values for parameters for which we wish to modify `defaults`. If we
#'   want to be able to auto-run off the saved param metadata, the names of the
#'   spatial agg levels need to match the objects, e.g. basin: basin
#' @param passed_args character, in yaml format, e.g. ""scenario_dir:
#'   'project_dir/hydrographs'". Possible to pass more than one, but complicated
#'   to get the yaml right. Suggest in that case modifying the params.yml
#' @param defaults character, path to default set of yaml parameters, so the
#'   `yamlpath` file and `passed_args` only need to include those that are
#'   modified
#' @param list_args list of arguments. Typically would come in from
#'   parameterised quarto notebook, which pre-parses yaml into a list, but could
#'   also be list(ewr = list(arg1 = x), aggregation = list(arg1 = y))
#'
#' @return runs HydroBOT, returns NULL invisibly, or the aggregated output if
#'   the params have `aggReturn: TRUE`
#' @export
#'

run_hydrobot_params <- function(yamlpath = NULL,
                               passed_args = NULL,
                               list_args = NULL,
                               defaults = system.file("yml/default_params.yml", package = "HydroBOT")) {
  # I could have a 'defaults' file and then a params that just changes some.
  # Maybe later. Might make a lot of sense if the only thing being passed in is
  # one value.

  # Default params
  arglist <- yaml::read_yaml(defaults)

  # Read in yaml params file from user
  if (!is.null(yamlpath)) {
    useryml <- yaml::read_yaml(yamlpath)

    arglist <- utils::modifyList(arglist, useryml)
  }


  # Allow user to pass yaml at the command line
  comargs <- yaml::yaml.load(passed_args)

  # Replace the arglist vals with passed args from command line
  if (is.list(comargs)) {
    arglist <- utils::modifyList(arglist, comargs)
  }

  # bring in list-args (especially useful for parameterised quarto notebook)
  if (is.list(list_args)) {
    # remove class from quarto list so later functions know how to use it
    if (inherits(list_args, "knit_param_list")) {
      list_args <- unclass(list_args)
    }

    arglist <- utils::modifyList(arglist, list_args)
  }


  # deal with different ways to handle aggregation sequences
  arglist <- clean_sequences(arglist)

  # Some default modifications need R functions
  arglist <- make_default_args(arglist)

  # Type cleanup
  arglist <- type_cleanup(arglist)

  # a simple EWR error catch (the aggregator does a more complete job internally)
  if (grepl("ewr", arglist$ewr$output_parent_dir) &&
      is.null(arglist$aggregation$group_until) &&
      is.null(arglist$aggregation$pseudo_spatial) &&
      !arglist$aggregation$auto_ewr_PU) {
    rlang::warn(c("It appears that you're processing EWRs and not managing planning units with `auto_ewr_PU = TRUE` or with a combination of group_until and pseudo_spatial."))
  }

  # add extrameta, this is in common between the ewr and aggregation, only include in one to avoid duplication
  paramextras <- list(
    run_from_params = TRUE,
    param_default = defaults,
    param_yml = yamlpath,
    param_passed = passed_args,
    param_list = list_args
  )

  if ('extrameta' %in% names(arglist$ewr)) {
    arglist$ewr$extrameta <- utils::modifyList(arglist$ewr$extrameta,
                                           paramextras)
  } else {
    arglist$ewr$extrameta <- paramextras
  }

  # Run EWR tool
  ewr_out <- rlang::exec(prep_run_save_ewrs,
                         !!!arglist$ewr[names(arglist$ewr) %in%
                                      names(formals(prep_run_save_ewrs))])
  # ewr_out <- prep_run_save_ewrs(
  #   hydro_dir = arglist$hydro_dir,
  #   output_parent_dir = arglist$output_parent_dir,
  #   outputType = arglist$ewr$outputType,
  #   returnType = arglist$ewr$returnType,
  #   extrameta =
  # )

  # Aggregate
  ewr_out <- rlang::exec(read_and_agg,
                         !!!arglist$aggregation[names(arglist$aggregation) %in%
                                      names(formals(read_and_agg))])
  # aggout <- read_and_agg(
  #   datpath = arglist$aggregation$datpath,
  #   type = arglist$aggType,
  #   geopath = HydroBOT::bom_basin_gauges,
  #   causalpath = HydroBOT::causal_ewr,
  #   groupers = arglist$aggregation$groupers,
  #   group_until = arglist$aggregation$group_until,
  #   pseudo_spatial = arglist$aggregation$pseudo_spatial,
  #   aggCols = arglist$agg_var,
  #   aggsequence = aggseq,
  #   funsequence = funseq,
  #   saveintermediate = TRUE,
  #   namehistory = arglist$namehistory,
  #   keepAllPolys = arglist$keepAllPolys,
  #   auto_ewr_PU = arglist$aggregation$auto_ewr_PU,
  #   returnList = arglist$aggReturn,
  #   savepath = arglist$aggregation$savepath,
  #   extrameta = list(
  #     agg_run_from_params = TRUE,
  #     agg_param_default = defaults,
  #     agg_param_yml = yamlpath,
  #     agg_param_passed = passed_args,
  #     agg_param_list = list_args
  #   )
  # )

  if (arglist$aggregation$returnList) {
    return(aggout)
  } else {
    return(invisible())
  }
}


#' Helper to allow making the directory structure programatically from the base
#' directory
#'
#' @param arglist list of arguments
#'

make_default_args <- function(arglist) {
  if (arglist$ewr$hydro_dir == "default" || is.null(arglist$ewr$hydro_dir)) {
    arglist$ewr$hydro_dir <- file.path(arglist$ewr$output_parent_dir, "hydrographs")
  }

  if (arglist$aggregation$datpath == "default" || is.null(arglist$aggregation$datpath)) {
    arglist$aggregation$datpath <- file.path(arglist$ewr$output_parent_dir, "module_output", "EWR")
  }

  if (arglist$aggregation$savepath == "default" || is.null(arglist$aggregation$savepath)) {
    arglist$aggregation$savepath <- file.path(arglist$ewr$output_parent_dir, "aggregator_output")
  }

  if (is.null(arglist$aggregation$group_until)) {
    arglist$aggregation$group_until <- rep(NA, length(arglist$aggregation$groupers))
  }

  return(arglist)
}

#
#' Sometimes single values come in as characters instead of lists
#'
#' @param arglist list of arguments
#'

type_cleanup <- function(arglist) {
  if (!is.list(arglist$ewr$returnType)) {
    arglist$ewr$returnType <- as.list(arglist$ewr$returnType)
  }

  if (!is.list(arglist$ewr$outputType)) {
    arglist$ewr$outputType <- as.list(arglist$ewr$outputType)
  }

  return(arglist)
}

#' Clean up the sequences if they are provided in r scripts or otherwise
#'
#' @param arglist
#'
#' @returns arglist
#'
clean_sequences <- function(arglist) {
  # R file allows aggregation definition with R types and expressions. This is
  # the most general, but also requires extra files and makes handling metadata
  # trickier
  if (!is.null(arglist$aggregation$aggregation_def)) {
    if (file.exists(arglist$aggregation$aggregation_def)) {
      source(arglist$aggregation$aggregation_def, local = TRUE)
    } else if (file.exists(file.path("inst", arglist$aggregation$aggregation_def))) {
      source(file.path("inst", arglist$aggregation$aggregation_def), local = TRUE)
    } else {
      rlang::abort(glue::glue("requested parameter R file {arglist$aggregation$aggregation_def} does not exist."))
    }

    # check others
    if (!is.null(arglist$aggregation$aggsequence)) {
      rlang::warn("Aggregation sequence defined in both an R file and yml. R file supersedes, but best to only use one.")
    }
    if (!is.null(arglist$aggregation$funsequence)) {
      rlang::warn("Aggregation funsequence defined in both an R file and yml. R file supersedes, but best to only use one.")
    }
  }

  if (is.null(arglist$aggregation$aggregation_def)) {
    aggseq <- arglist$aggregation$aggsequence
    funseq <- arglist$aggregation$funsequence

    # Some attempts at simple cleanup- funseq sometimes comes in as a vector
    # from the yaml if we only have one function
    if (!inherits(funseq, "list")) {
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

  arglist$aggregation$aggsequence <- aggseq
  arglist$aggregation$funsequence <- funseq

  return(arglist)
}
