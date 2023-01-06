
# Wrap the python
# reticulate::source_python(system.file("python/controller_functions.py", package = 'werptoolkitr'))


#' Wrapper for prep_run_save_ewrs
#'
#' @param ... All params for python fun
#'
#' @return EWR outputs if desired
#' @export
#'
#' @examples
prep_run_save_ewrs_R <- function(...) {
  reticulate::source_python(system.file("python/controller_functions.py", package = 'werptoolkitr'))
  ewrout <- prep_run_save_ewrs(...)
  # ewrout <- controller_functions$prep_run_save_ewrs(...)
}
# prep_run_save_ewrs_R <- function(scenario_dir, output_dir,
#                                  model_format, climate,
#                                  outputType = 'none', returnType = 'none',
#                                  MINT = (100 - 0)/100, MAXT = (100 + 0 )/100,
#                                  DUR = (100 - 0 )/100, DRAW = (100 -0 )/100,
#                                  datesuffix = FALSE) {
#
#   reticulate::source_python(system.file("python/controller_functions.py", package = 'werptoolkitr'))
#   ewrout <- prep_run_save_ewrs(scenario_dir, output_dir,
#                                model_format, climate,
#                                outputType, returnType,
#                                MINT, MAXT, DUR, DRAW, datesuffix)
#
# }
