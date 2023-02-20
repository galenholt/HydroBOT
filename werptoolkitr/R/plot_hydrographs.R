#' Standard hydrograph plot
#'
#' @param hydrolong hydrograph data in long format
#' @param gaugefilter set of gauges to plot, default `NULL` plots all of them
#' @param scenariofilter set of scenarios to plot, default `NULL` plots all of them
#' @param colors a named `colors` object or character vector giving a {paletteer} `palette` argument. Typically the former using `make_pal` to keep scenarios with consistent colours throughout, likely with a reference level.
#' @param scales facet scales, as in `?ggplot2::facet_wrap`. Default `scales = 'fixed'` holds them the same, most common change will be to `scales = 'free_y'` if gauges have very differen flows.
#' @param transy transformation for y axis as in `?ggplot2::scale_y_continuous`. Default `transy = 'identity'` just uses the data. Most common change likely `transy = 'log10`
#' @param y_col character, column name for what's plotted on the y-axis. Default 'flow', but will need to change if fed data with a different name
#' @param base_lev value to use as the base for comparison. Default NULL, no comparison. See [baseline_compare()] and [create_base()] for options.
#' @param comp_fun function to use in comparison. Default NULL, no comparison. See [baseline_compare()] and [create_base()] for options.
#' @param ...
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_hydrographs <- function(hydrolong,
                             y_col = 'flow',
                             gaugefilter = NULL,
                             scenariofilter = NULL,
                             colors = "RColorBrewer::Dark2",
                             scales = 'fixed',
                             transy = 'identity',
                             base_lev = NULL,
                             comp_fun = NULL,
                             ...) {

    if (!inherits(colors, 'colors')) {
    rlang::inform("colors not specified per level. Trying to use the 'colors' argument as a palette name")
    colors <- make_pal(levels = unique(hydrolong$scenario), palette = colors)
  }

  if (is.null(gaugefilter)) {
    gaugefilter <- unique(hydrolong$gauge)
  }
  if (is.null(scenariofilter)) {
    scenariofilter <- unique(hydrolong$scenario)
  }

  ylab_append <- ''
  if (!is.null(comp_fun) & !is.null(base_lev)) {
    hydrolong <- hydrolong |>
      baseline_compare(group_col = 'scenario', base_lev = base_lev,
                       values_col = y_col, comp_fun = comp_fun, ...)

    comp_fun_name <- as.character(substitute(comp_fun))
    base_col_name <- paste0(comp_fun_name, '_', y_col)
    names(hydrolong)[names(hydrolong) == paste0('comp_fun_', y_col)] <- base_col_name
    y_col <- base_col_name
    ylab_append <- paste0('\n', comp_fun_name, ' to ', as.character(base_lev))
  }


  hydro_plot <- hydrolong |>
    dplyr::filter(gauge %in% gaugefilter & scenario %in% scenariofilter) |>
    ggplot2::ggplot(ggplot2::aes(x = Date, y = .data[[y_col]], color = scenario)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~gauge, scales = scales) +
    ggplot2::labs(y = paste0("Flow (ML/day)", ylab_append), color = 'Scenario') +
    ggplot2::scale_y_continuous(trans = transy) +
    ggplot2::scale_color_manual(values = colors) +
    theme_werp_toolkit()

  return(hydro_plot)
}
