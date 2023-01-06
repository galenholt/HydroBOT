
# Wrap the python
#' Wrapper for prep_run_save_ewrs
#'
#' @param ... All params for python fun
#'
#' @return EWR outputs if desired
#' @export
#'
#' @examples
# The shortest but least helpful version
prep_run_save_ewrs_R <- function(...) {
  ewrout <- prep_run_save_ewrs(...)
  # a better solution, but can't quite get it to work in practice.
  # ewrout <- controller_functions$prep_run_save_ewrs(...)
}

# More expressive, but then have to keep up with args both places
# prep_run_save_ewrs_R <- function(scenario_dir, output_dir,
#                                  model_format, climate,
#                                  outputType = 'none', returnType = 'none',
#                                  MINT = (100 - 0)/100, MAXT = (100 + 0 )/100,
#                                  DUR = (100 - 0 )/100, DRAW = (100 -0 )/100,
#                                  datesuffix = FALSE) {
#
#   ewrout <- prep_run_save_ewrs(scenario_dir, output_dir,
#                                model_format, climate,
#                                outputType, returnType,
#                                MINT, MAXT, DUR, DRAW, datesuffix)
#
# }


# The rest of the functions we want to export -----------------------------

#' Wrapper for make_scenario_info
#'
#' @param ... arguments for the py function
#'
#' @return file locations as a dict
#' @export
#'
#' @examples
make_scenario_info_R <- function(...) {
  r_py_out <- make_scenario_info(...)
}

#' Wrapper for make_output_dir
#'
#' @param ... arguments for py function
#'
#' @return build the directory structure
#' @export
#'
#' @examples
make_output_dir_R <- function(...) {
  r_py_out <- make_output_dir(...)
}

#' Wrapper for run_save_ewrs
#'
#' @param ... arguments to the py function
#'
#' @return the EWRs, and saving
#' @export
#'
#' @examples
run_save_ewrs_R <- function(...) {
  r_py_out <- run_save_ewrs(...)
}

# There are other functions, but that should cover us for now.
