#' Plot stacked bar graphs of outcomes across scenarios
#'
#' Has several ways to color, including grouped palettes. Lets us choose whether
#' to stack scenarios with outcome groups on x (`scene_x = FALSE`) or stack
#' outcome groups with scenarios on x (`scene_x = TRUE`). I do *not* provide an
#' argument to drop legend labels on purpose- it's dangerous to auto-drop them.
#' The user can always `+ theme(legend.position = 'none')` to the returned
#' object.
#'
#' @inheritParams plot_hydrographs
#'
#' @param outdf dataframe of outcomes, needs a `scenario` column
#' @param y_col character, name of column for the y-axis
#' @param y_lab character, default `y_col`, allows changing y-label
#' @param x_col character, name of column for the x-axis. Default `'scenario'`
#' @param x_lab character, default `x_col`, allows changing x-label
#' @param facet_row NULL (default) or character for facet row. Can be `'.'` to
#'   have one row and column-facets
#' @param facet_col NULL (default) or character for facet column. Can be `'.'`
#'   to have one column and row-facets
#' @param facet_wrapper NULL (default) or character for column to use for
#'   `facet_wrap`
#' @param colorgroups character, column name for grouping `colorset`,
#'   particularly into different palettes
#' @param colorset character, column name defining the colors to use (if
#'   `scene_x = TRUE`) or the x-levels (if `scene_x = FALSE`)- needs to be
#'   renamed to reflect that it isn't just colors
#' @param color_lab
#' @param pal_list list of palettes for defining colors for `colorset`. Should
#'   be length of `colorgroups`
#' @param sceneorder character or factor giving the order to present scenario
#'   levels
#' @param scene_x logical, default `TRUE`- scenario on x-axis. `FALSE` puts
#'   `colorset` on x-axis and stacks scenarios
#' @param scene_pal named palette for scenarios, only used if `scene_x = FALSE`
#' @param position character, `position` argument from [ggplot2::geom_col()], to
#'   change from stacked to dodged bars
#'
#' @return a ggplot stacked bar plot with standard formatting and coloring,
#'   stacking either scenarios or colorset
#' @export
#'
#' @examples
plot_outcomes_stacked <- function(outdf,
                              y_col,
                              y_lab = y_col,
                              x_col = 'scenario',
                              x_lab = x_col,
                              facet_row = NULL,
                              facet_col = NULL,
                              facet_wrapper = NULL,
                              scenariofilter = NULL,
                              colorgroups = NULL,
                              colorset = 'scenario',
                              color_lab = ifelse(is.null(colorgroups), colorset, colorgroups),
                              pal_list = "scico::berlin",
                              sceneorder = NULL,
                              scene_pal = "scico::oslo",
                              scales = 'fixed',
                              transy = 'identity',
                              base_lev = NULL,
                              comp_fun = NULL,
                              position = 'stack',
                              ...) {


  ## setup

  if ((!is.null(facet_row) || !is.null(facet_col)) & !is.null(facet_wrapper)) {
    rlang::abort("asking for facet_wrap and facet_grid. Make up your mind.")
  }

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  if (is.function(comp_fun) || (is.list(comp_fun) & is.function(comp_fun[[1]]))) {
    comp_fun <- as.character(substitute(comp_fun))
    if(comp_fun[1] == "c") {comp_fun <- comp_fun[2:length(comp_fun)]}
  }

  # use `grouped_colors` to set colour groups
  outdf <- grouped_colours(outdf, pal_list, colorgroups, colorset)

  # I guess keep scenariofilter until it breaks something, even though this
  # needs to be more flexible.
  # The `colors` argument here gets returned in the list, NOT in the data, which
  # retains the `color` column from `grouped_colours`
  prepped <- plot_prep(data = outdf, y_col = y_col, colors = scene_pal,
                       sceneorder = sceneorder,
                       gaugefilter = NULL,
                       scenariofilter = scenariofilter,
                       base_lev = base_lev,
                       comp_fun = comp_fun, ...)

  # finds labels that match scale_color_identity
  labfind <- function(breaks, data = prepped$data) {
    if (inherits(data, 'sf')) {data <- sf::st_drop_geometry(data)}
    matched_vals <- unique(data[,c('colordef', 'color')])
    # make sure everything lines up
    m_ind <- match(matched_vals$color, breaks)
    return(dplyr::pull(matched_vals[, 'colordef'])[m_ind])
  }

  ## different plot types

  if (x_col == 'scenario') {

    outcome_plot <- prepped$data |>
      dplyr::filter(scenario %in% prepped$scenariofilter) |>
      ggplot2::ggplot(ggplot2::aes(x = scenario, y = .data[[prepped$y_col]],
                                   fill = forcats::fct_inorder(color))) +
      ggplot2::geom_col(position = position) +
      ggplot2::labs(y = paste0(y_lab, prepped$ylab_append), color = colorset) +
      ggplot2::scale_y_continuous(trans = transy) +
      ggplot2::scale_fill_identity(guide = 'legend',
                                   labels = labfind,
                                   name = color_lab) +
      theme_werp_toolkit()
  }

  # close to allowing a grouping aesthetic, but it's ugly with color, so not
  # doing it now.
  if (x_col != 'scenario' & !is.numeric(x_col)) {
    outcome_plot <- prepped$data |>
      dplyr::filter(scenario %in% prepped$scenariofilter) |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[x_col]], y = .data[[prepped$y_col]],
                                   # color = color,
                                   fill = scenario)) +
      ggplot2::geom_col(position = position) +
      ggplot2::labs(y = paste0(y_lab, prepped$ylab_append)) +
      ggplot2::scale_y_continuous(trans = transy) +
      ggplot2::scale_fill_manual(values = prepped$colors) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
      # ggplot2::scale_color_identity() +
      theme_werp_toolkit()
  }


  ## facetting


  if (!is.null(facet_row) & !is.null(facet_col)) {
    outcome_plot <- outcome_plot +
      # ahhh. reformulate is (RHS, LHS) - so, backwards to what we want.
      ggplot2::facet_grid(reformulate(facet_col,facet_row),
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
