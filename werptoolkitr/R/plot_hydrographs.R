#' Standard hydrograph plot
#'
#' @param hydrolong hydrograph data in long format
#' @param gaugefilter set of gauges to plot, default `NULL` plots all of them
#' @param scenariofilter set of scenarios to plot, default `NULL` plots all of them
#' @param colors a named `colors` object or character vector giving a {paletteer} `palette` argument. Typically the former using `make_pal` to keep scenarios with consistent colours throughout, likely with a reference level.
#' @param scales facet scales, as in `?ggplot2::facet_wrap`. Default `scales = 'fixed'` holds them the same, most common change will be to `scales = 'free_y'` if gauges have very differen flows.
#' @param transy transformation for y axis as in `?ggplot2::scale_y_continuous`. Default `transy = 'identity'` just uses the data. Most common change likely `transy = 'log10`
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_hydrographs <- function(hydrolong,
                             gaugefilter = NULL,
                             scenariofilter = NULL,
                             y_col = flow,
                             colors = "RColorBrewer::Dark2",
                             scales = 'fixed',
                             transy = 'identity') {

  if (!inherits(colors, 'colors')) {
    warning("colors not specified per level. Trying to use the 'colors' argument as a palette name")
    colors <- make_pal(levels = unique(hydrolong$scenario), palette = colors)
  }

  if (is.null(gaugefilter)) {
    gaugefilter <- unique(hydrolong$gauge)
  }
  if (is.null(scenariofilter)) {
    scenariofilter <- unique(hydrolong$scenario)
  }

  hydro_plot <- hydrolong |>
    dplyr::filter(gauge %in% gaugefilter & scenario %in% scenariofilter) |>
    ggplot2::ggplot(ggplot2::aes(x = Date, y = {{ y_col }}, color = scenario)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~gauge, scales = scales) +
    ggplot2::labs(y = "Flow (ML/day)", color = 'Scenario') +
    ggplot2::scale_y_continuous(trans = transy) +
    ggplot2::scale_color_manual(values = colors) +
    theme_werp_toolkit()

  return(hydro_plot)
}
