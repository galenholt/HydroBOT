plot_prep <- function(data, y_col,
                      colors = 'ggsci::default_igv',
                      gaugefilter = NULL,
                      scenariofilter = NULL,
                      base_lev = NULL,
                      comp_fun = NULL,
                      ...) {

  # This probably needs more thought to handle continuous palettes if it is to
  # be a general prep function across all plots
  if (!inherits(colors, 'colors')) {
    rlang::inform("colors not specified per level. Trying to use the 'colors' argument as a palette name")
    colors <- make_pal(levels = unique(data$scenario), palette = colors)
  }

  gaugefilter <- if(is.null(gaugefilter)) {unique(data$gauge)} else {gaugefilter}
  scenariofilter <- if(is.null(scenariofilter)) {unique(data$scenario)} else  {scenariofilter}

  ylab_append <- ''
  if (!is.null(comp_fun) & !is.null(base_lev)) {
    data <- data |>
      baseline_compare(group_col = 'scenario', base_lev = base_lev,
                       values_col = y_col, comp_fun = comp_fun, ...)

    if (is.character(comp_fun)) {
      comp_fun_name <- comp_fun
    } else if (is.list(comp_fun)) {
      comp_fun_name = names(comp_fun)
    } else {
      rlang::abort("getting the name of comp_fun isn't supported,
                       likely because of bare names lost in the call stack.
                       Please use character or list-defined functions. ")
    }
    base_col_name <- paste0(comp_fun_name, '_', y_col)
    names(data)[names(data) == paste0('comp_fun_', y_col)] <- base_col_name
    y_col <- base_col_name
    ylab_append <- paste0('\n', comp_fun_name, ' to ', as.character(base_lev))
  }

  # This names the data the same thing as it was interactively, but fails in the function. Just call it data for consistency
  # dataname <- as.character(substitute(data))
  return(tibble::lst(data, y_col, colors, gaugefilter, scenariofilter, base_lev, comp_fun, ylab_append))
}


