#' Standard hydrograph plot
#'
#' @inheritParams plot_outcomes
#'
#' @param hydrolong hydrograph data in long format
#' @param gaugefilter set of gauges to filter
#' @param colors name of palette. Will change in future to match plot_outcomes color defining
#' @param ... passed to [plot_prep()]
#'
#'
#' @return ggplot object
#' @export
#'
plot_hydrographs <- function(hydrolong,
                             outcome_col = 'flow',
                             gaugefilter = NULL,
                             scenariofilter = NULL,
                             colors = "RColorBrewer::Dark2",
                             sceneorder = NULL,
                             scales = 'fixed',
                             transoutcome = 'identity',
                             base_lev = NULL,
                             comp_fun = NULL,
                             ...) {

  # This is not supported anymore
  rlang::abort("`plot_hydrographs` is deprecated and will soon be removed.
               Please use `plot_outcomes` instead as a general-purpose plotting function,
               or `plot_numeric` with `xdate = TRUE`.")
  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  if (is.function(comp_fun) || (is.list(comp_fun) & is.function(comp_fun[[1]]))) {
    comp_fun <- as.character(substitute(comp_fun))
    if(comp_fun[1] == "c") {comp_fun <- comp_fun[2:length(comp_fun)]}
  }

  prepped <- plot_prep(data = hydrolong, outcome_col = outcome_col, colors = colors,
                       sceneorder = sceneorder,
                       gaugefilter = gaugefilter,
                       scenariofilter = scenariofilter,
                       base_lev = base_lev,
                       comp_fun = comp_fun, ...)



  hydro_plot <- prepped$data |>
    dplyr::filter(gauge %in% prepped$gaugefilter & scenario %in% prepped$scenariofilter) |>
    ggplot2::ggplot(ggplot2::aes(x = Date, y = .data[[prepped$outcome_col]], color = scenario)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~gauge, scales = scales) +
    ggplot2::labs(y = paste0("Flow (ML/day)", prepped$ylab_append), color = 'Scenario') +
    ggplot2::scale_y_continuous(trans = transoutcome) +
    ggplot2::scale_color_manual(values = prepped$colors) +
    theme_werp_toolkit()

  return(hydro_plot)
}
