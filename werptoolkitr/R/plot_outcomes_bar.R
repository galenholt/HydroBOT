#' Plot outcomes as bar graph with scenario on x, possibly facetted and colored
#'
#' This is a simpler version of `plot_outcomes`
#'
#' @inheritParams plot_outcomes
#'
#' @param colors named `colors` object specifying the colors for the scenarios
#'
#' @return a ggplot bar graph with standardized style and color, possibly facetted
#' @export
#'
#' @examples
plot_outcomes_bar <- function(outdf,
                             y_col,
                             y_lab = y_col,
                             facet_row = NULL,
                             facet_col = NULL,
                             facet_wrapper = NULL,
                             scenariofilter = NULL,
                             colors = "scico::oslo",
                             sceneorder = NULL,
                             scales = 'fixed',
                             transy = 'identity',
                             base_lev = NULL,
                             comp_fun = NULL,
                             ...) {

  # enforcing scenario as x, but that could change. It just ends up as a ggplot
  # rewrite though at some point.

  if ((!is.null(facet_row) || !is.null(facet_col)) & !is.null(facet_wrapper)) {
    rlang::abort("asking for facet_wrap and facet_grid. Make up your mind.")
  }

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  if (is.function(comp_fun) || (is.list(comp_fun) & is.function(comp_fun[[1]]))) {
    comp_fun <- as.character(substitute(comp_fun))
    if(comp_fun[1] == "c") {comp_fun <- comp_fun[2:length(comp_fun)]}
  }

  # I guess keep scenariofilter until it breaks something, even though this
  # needs to be more flexible.
  prepped <- plot_prep(data = outdf, y_col = y_col, colors = colors,
                       sceneorder = sceneorder,
                       gaugefilter = NULL,
                       scenariofilter = scenariofilter,
                       base_lev = base_lev,
                       comp_fun = comp_fun, ...)

  # Catch errors with data arrangement
  # nrow instead of length because tibbles remain tibbles
  # The factorial depends on whether we're wrapping one col or gridding 2.
  if (!is.null(facet_row) & !is.null(facet_col)) {
    facetfact <- nrow(unique(sf::st_drop_geometry(prepped$data[facet_row])))*
    nrow(unique(sf::st_drop_geometry(prepped$data[facet_col])))
  }

  if (!is.null(facet_wrapper)) {
    facetfact <- nrow(unique(sf::st_drop_geometry(prepped$data[facet_wrapper])))
  }

  totalfactorial <- nrow(unique(sf::st_drop_geometry(prepped$data['scenario'])))*
    facetfact

  if (nrow(prepped$data) != totalfactorial) {
    rlang::abort("Data has more rows than the factorial of scenario,
                 facet_row and facet_col, and so will double-plot data")
    }

  outcome_plot <- prepped$data |>
    dplyr::filter(scenario %in% prepped$scenariofilter) |>
    ggplot2::ggplot(ggplot2::aes(x = scenario, y = .data[[prepped$y_col]], fill = scenario)) +
    ggplot2::geom_col() +
    ggplot2::labs(y = paste0(y_lab, prepped$ylab_append), color = 'Scenario') +
    ggplot2::scale_y_continuous(trans = transy) +
    ggplot2::scale_fill_manual(values = prepped$colors) +
    theme_werp_toolkit()


  if (!is.null(facet_row) & !is.null(facet_col)) {
    outcome_plot <- outcome_plot +
      ggplot2::facet_grid(reformulate(facet_row,facet_col),
                          # Good in theory, but often too long and blocks the plot, so not using
                          # labeller = ggplot2::label_wrap_gen(),
                          scales = scales)
  }

  if (!is.null(facet_wrapper)) {
    outcome_plot <- outcome_plot +
      ggplot2::facet_wrap(facet_wrapper,
                          # Good in theory, but often too long and blocks the plot, so not using
                          # labeller = ggplot2::label_wrap_gen(),
                          scales = scales)
    }

  return(outcome_plot)
}
