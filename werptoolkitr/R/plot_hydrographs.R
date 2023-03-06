#' Standard hydrograph plot
#'
#' @inheritParams plot_prep
#' @param hydrolong hydrograph data in long format
#' @param scales facet scales, as in `?ggplot2::facet_wrap`. Default `scales = 'fixed'` holds them the same, most common change will be to `scales = 'free_y'` if gauges have very differen flows.
#' @param transy transformation for y axis as in `?ggplot2::scale_y_continuous`. Default `transy = 'identity'` just uses the data. Most common change likely `transy = 'log10`
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
                             sceneorder = NULL,
                             scales = 'fixed',
                             transy = 'identity',
                             base_lev = NULL,
                             comp_fun = NULL,
                             ...) {

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  if (is.function(comp_fun) || (is.list(comp_fun) & is.function(comp_fun[[1]]))) {
    comp_fun <- as.character(substitute(comp_fun))
    if(comp_fun[1] == "c") {comp_fun <- comp_fun[2:length(comp_fun)]}
  }

  prepped <- plot_prep(data = hydrolong, y_col = y_col, colors = colors,
                       sceneorder = sceneorder,
                       gaugefilter = gaugefilter,
                       scenariofilter = scenariofilter,
                       base_lev = base_lev,
                       comp_fun = comp_fun, ...)



  hydro_plot <- prepped$data |>
    dplyr::filter(gauge %in% prepped$gaugefilter & scenario %in% prepped$scenariofilter) |>
    ggplot2::ggplot(ggplot2::aes(x = Date, y = .data[[prepped$y_col]], color = scenario)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~gauge, scales = scales) +
    ggplot2::labs(y = paste0("Flow (ML/day)", prepped$ylab_append), color = 'Scenario') +
    ggplot2::scale_y_continuous(trans = transy) +
    ggplot2::scale_color_manual(values = prepped$colors) +
    theme_werp_toolkit()

  return(hydro_plot)
}
