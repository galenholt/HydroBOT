#' Some standard data preparation for plotting
#'
#' This is an attempt to pull a lot of copy-paste out of the top of plot
#' functions. It's likely to continue to evolve quite a bit as we see all the
#' different things we need to manage in the plot setup. One of the goals here
#' is to avoid making a million very similar datasets- doing it in functions
#' keeps those changes sandboxed
#'
#' @param data dataframe to prep
#' @param gaugefilter set of gauges to plot, default `NULL` plots all of them
#' @param sceneorder character or factor giving the order to present scenario
#'   levels
#' @param scenariofilter set of scenarios to plot, default `NULL` plots all of
#'   them
#' @param colors a named `colors` object or character vector giving a
#'   {paletteer} `palette` argument. Typically the former using `make_pal` to
#'   keep scenarios with consistent colours throughout, likely with a reference
#'   level.
#' @param y_col character, column name for what's plotted on the y-axis. Default
#'   'flow', but will need to change if fed data with a different name
#' @param base_lev value to use as the base for comparison. Default NULL, no
#'   comparison. See [baseline_compare()] and [create_base()] for options.
#' @param comp_fun function to use in comparison. Default NULL, no comparison.
#'   See [baseline_compare()] and [create_base()] for options.
#' @param zero_adjust numeric (default 0) or `"auto"`, adjustment to data to
#'   avoid zeros by adding `zero_adjust` to `abs(data)`, e.g shifting all data
#'   away from zero, either positively or negatively. Zeros themselves are
#'   shifted up or down randomly. Used for avoiding x/0, NaN, and Inf when
#'   relativiszing and taking logs, primarily. Auto shifts by
#'   `0.1*min(abs(data[data != 0]))`.
#' @param onlyzeros logical, default `FALSE`. Should all values be adjusted away from zero (`TRUE`) or only adjust zero values (`FALSE`)?
#' @param ... passed to [baseline_compare()]. Note, `zero_adjust` and `onlyzeros` are *not* passed, but handled by [plot_prep()] itself since they are done to the data prior in order to adjust zeros for other purposes as well (logging, plotting).
#'
#' @return a list with prepped versions of `data`, `y_col`, `colors`,
#'   `gaugefilter`, `scenariofilter`, `base_lev`, `comp_fun`, `ylab_append` to
#'   be used in plot calls
#' @export
#'
#' @examples
plot_prep <- function(data, y_col,
                      colors = 'ggsci::default_igv',
                      sceneorder = NULL,
                      gaugefilter = NULL,
                      scenariofilter = NULL,
                      base_lev = NULL,
                      comp_fun = NULL,
                      zero_adjust = 0,
                      onlyzeros = FALSE,
                      ...) {

  # This probably needs more thought to handle continuous palettes if it is to
  # be a general prep function across all plots. Causal_colors_general can
  # probably be modified to do continuous colors. Here, though, we still might
  # use continuous *palettes*, even if the actual scale isn't continuous.
  if (!inherits(colors, 'colors')) {
    rlang::inform("colors not specified per level. Trying to use the 'colors' argument as a palette name")
    colors <- make_pal(levels = unique(data$scenario), palette = colors)
  }

  # ensure y is numeric
  # Have to [[]] because [] yields an sf-tibble, not just the col.
  if (!is.numeric(data[[y_col]])) {
    data <- data |>
      dplyr::mutate("{y_col}" := as.numeric(.data[[y_col]]))
  }

  # Adjust to keep off zero if requested
  if (!('relative' %in% comp_fun) & zero_adjust != 0) {
    rlang::warn(glue::glue("`comp_fun` is {comp_fun}, but you're adjusting the data by {zero_adjust}. Do you really want to do that? It may be appropriate if you're log-transforming or dividing, but be careful.\n"))
  }
  if (grepl('auto', zero_adjust)) {
    zero_adjust <- min(abs(data[[y_col]])[data[[y_col]] != 0], na.rm = TRUE)*0.1
  }
  # move data away from zero
  data <- adjust_zeros(data, y_col, zero_adjust, onlyzeros)


  gaugefilter <- if(is.null(gaugefilter) & ('gauge' %in% names(data))) {unique(data$gauge)} else {gaugefilter}
  scenariofilter <- if(is.null(scenariofilter)) {unique(data$scenario)} else  {scenariofilter}

  ylab_append <- ''
  if (!is.null(comp_fun) & !is.null(base_lev)) {
    # We've already handled the zero-shifts, so don't do it again
    data <- data |>
      baseline_compare(compare_col = 'scenario', base_lev = base_lev,
                       values_col = y_col, comp_fun = comp_fun, zero_adjust = 0, onlyzeros = TRUE, ...)

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

  # Order the scenario if I've given it an order.
  if (!is.null(sceneorder)) {
    if (inherits(sceneorder, 'factor')) {sceneorder <- levels(sceneorder)}
    data <- data |>
      dplyr::mutate(scenario = forcats::fct_relevel(scenario, sceneorder))
  }

  # This names the data the same thing as it was interactively, but fails in the function. Just call it data for consistency
  # dataname <- as.character(substitute(data))
  return(tibble::lst(data, y_col, colors, gaugefilter, scenariofilter, base_lev, comp_fun, ylab_append))
}


