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
#' @param color_lab character, default either `colorgroups` or `colorset` (if
#'   `is.null(colorgroups)`). Allows changing the color legend label
#' @param point_group character, column to add additional point groupings to,
#'   e.g. if plotting color by an environmental group, but need separate
#'   lines/points for each environmental objective.
#' @param pal_list list of palettes for defining colors for `colorset`. Should
#'   be length of `colorgroups`
#' @param sceneorder character or factor giving the order to present scenario
#'   levels
#' @param scene_pal named palette for scenarios, only used if `scene_x = FALSE`
#' @param transx transformation for x axis as in [ggplot2::scale_x_continuous()].
#'   Default `transx = 'identity'` just uses the data. Most common change likely
#'   `transx = 'log10`
#' @param position character or `position` function, `position` arguments from [ggplot2::geom_col()] and [ggplot2::geom_point()] (depending on plot type), to
#'   change from stacked to dodged bars or jitter points. Can be character, e.g. 'jitter' or a function, e.g. `ggplot2::position_jitter(width = 0.1, height = 0)`
#' @param smooth, logical, default `FALSE`. If plotting lines (`x_col` is numeric), do we want straight lines (the default, [ggplot2::geom_line()]) or fits ([ggplot2::geom_smooth()])?
#' @param smooth_method `method` argument to [ggplot2::geom_smooth()]. Ignored if not smoothing
#' @param smooth_se `se` argument to [ggplot2::geom_smooth()]. Ignored if not smoothing
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
                              point_group = NULL,
                              pal_list = "scico::berlin",
                              sceneorder = NULL,
                              scene_pal = "scico::oslo",
                              scales = 'fixed',
                              transy = 'identity',
                              transx = 'identity',
                              base_lev = NULL,
                              comp_fun = NULL,
                              position = 'stack',
                              smooth = FALSE,
                              smooth_method = NULL,
                              smooth_se = TRUE,
                              plotbasin = FALSE,
                              basincolor = 'lightcyan',
                              plotgauges = 'none',
                              ...) {


  ## setup- this all needs to be a single prep function. Can we wrap it into plot_prep?

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

  # Deal with additional point_groups- temped to put in grouped_colors, but
  # there are too many short-circuits
  if (is.null(point_group)) {
    outdf <- outdf |>
      dplyr::mutate(pointgroup = .data[[colorset]])
  } else {
    outdf <- outdf |>
      dplyr::mutate(pointgroup = interaction(.data[[colorset]], .data[[point_group]]))
  }

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

  # scenario on x, bar
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

  # Something else on x, scenario as fill. bar
  # close to allowing a grouping aesthetic, but it's ugly with color, so not
  # doing it now.
  if (!(x_col %in% c('scenario', 'map')) &&
      !is.numeric(dplyr::pull(sf::st_drop_geometry(prepped$data[,x_col])))) {
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

  # Some quantitative x, typically a quant representation of scenario
  if (x_col != 'map') {
    if (is.numeric(dplyr::pull(sf::st_drop_geometry(prepped$data[,x_col])))) {

      # I was trying to get this to work to color the *points* one way and the
      # lines another. It's kind of a mess because ggplot doesn't like that
      # I can potentially use 'fill' if I use shapes that take that argument

      # The more I think about this, I *could* force the use of filled points, but
      # that's not really the point. The point is the scenarios give us the values
      # on x, so let's just leave them alone, and color by whatever we're coloring
      # by. We can revisit that decision later if we want.

      if (!inherits(position, 'gg')) {
        if (is.null(position) || position == 'stack') {
          position = 'identity'
        }
      }


      outcome_plot <- prepped$data |>
        dplyr::filter(scenario %in% prepped$scenariofilter) |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[x_col]],
                                     y = .data[[prepped$y_col]],
                                     group = .data$pointgroup # interaction(env_obj, .data[[colorset]])
        )) +
        # ggplot2::geom_line(mapping = ggplot2::aes(color = .data$color)) +
        #
        # ggplot2::geom_smooth(mapping = ggplot2::aes(color = .data$color), method = NULL, linewidth = 0.1) +
        ggplot2::geom_point(mapping = ggplot2::aes(color = .data$color,
                                                   shape = scenario),
                            position = position) +
        ggplot2::labs(y = paste0(y_lab, prepped$ylab_append),
                      x = x_lab) +
        ggplot2::scale_y_continuous(trans = transy) +
        ggplot2::scale_x_continuous(trans = transx) +
        ggplot2::scale_fill_manual(values = prepped$colors) +
        ggplot2::scale_color_identity(guide = 'legend',
                                      labels = labfind,
                                      name = color_lab) +
        theme_werp_toolkit()

      if (smooth) {
        outcome_plot <- outcome_plot +
          ggplot2::geom_smooth(mapping = ggplot2::aes(color = .data$color),
                               method = smooth_method, linewidth = 0.1,
                               se = smooth_se)
      }
      if (!smooth) {
        outcome_plot <- outcome_plot +
          ggplot2::geom_line(mapping = ggplot2::aes(color = .data$color))
      }

    }
  }


  # This is getting contrived to do all these in one function. I *really* need
  # to fully pull out the dataprep from the plotting in this function.
    # I *think* I'm going to have to call the same col y_col and colorset to get the transforms to work as if it's y as well as the colors.
    # This might just be too contrived.
  # I probably really need a catcher here that identifies if there are multiple values being plotted and fail.
  # Can we split mulitple palettes? probably, and it might make sense if we
  # crossed with facetting. Doing it is a bit unclear, and I'll need to think
  # more. Do we have a way to generate the color mapping for continuuous? I
  # think yes. Do we have a way to also do that for `trans` functions? What
  # about preserving the values for display on the legend? That's all doable,
  # but gets tricky
  # What else do I want here? potentially additional geoms- gauge points and basin.
  if (x_col == 'map') {

    if (length(pal_list) > 1) {rlang::warn(glue::glue("using first palette for {y_col}, splitting up palettes needs more thought"))}

    overplot_test <- prepped$data |>
      dplyr::group_by(geometry)

    if (!is.null(facet_wrapper)) {
      overplot_test <- overplot_test |>
        dplyr::group_by(.data[[facet_wrapper]], .add = TRUE)
    }
    if (!is.null(facet_row) & facet_row != '.') {
      overplot_test <- overplot_test |>
        dplyr::group_by(.data[[facet_row]], .add = TRUE)
    }
    if (!is.null(facet_col) & facet_col != '.') {
      overplot_test <- overplot_test |>
        dplyr::group_by(.data[[facet_col]], .add = TRUE)
    }

    overplot_test <- overplot_test |>
      dplyr::summarise(nrows = dplyr::n()) |>
      dplyr::ungroup()

    if (!all(overplot_test$nrows == 1)) {
      rlang::abort("Trying to plot multiple values
                   (up to {max(overplot_test$nrows)}) in single polygons.
                   Something is duplicated-
                   do you need more facetting or filtering?")
    }

    outcome_plot <- ggplot2::ggplot()

    # underlay basin
    if (plotbasin) {
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = basin, fill = basincolor)
    }


    if (all(sf::st_is(prepped$data, c("POINT", "LINESTRING")))) {
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = prepped$data |>
                           dplyr::filter(scenario %in% prepped$scenariofilter),
                         ggplot2::aes(color = .data[[prepped$y_col]])) +
        paletteer::scale_color_paletteer_c(palette = pal_list[[1]], trans = transy)
    }
    if (all(sf::st_is(prepped$data, c("POLYGON", "MULTIPOLYGON")))) {
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = prepped$data |>
                           dplyr::filter(scenario %in% prepped$scenariofilter),
                         ggplot2::aes(fill = .data[[prepped$y_col]])) +
        paletteer::scale_fill_paletteer_c(palette = pal_list[[1]], trans = transy)
    }

    outcome_plot <- outcome_plot +
      ggplot2::labs(fill = paste0(y_lab, prepped$ylab_append)) +
      # ggplot2::scale_fill_manual(values = prepped$colors) +
      # ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
      # ggplot2::scale_color_identity() +
      theme_werp_toolkit()


    # I need to have a way to send in color args etc for the gauges, I think.
    if (inherits(plotgauges, 'sf')) {
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = plotgauges)
    } else if (is.character(plotgauges) && plotgauges != 'none') {
      if (plotgauges == 'clip') {
        gauges_to_plot <- sf::st_filter(bom_basin_gauges, dplyr::distinct(overplot_test))
      }
      if (plotgauges == 'all') {
        gauges_to_plot <- bom_basin_gauges
      }
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = gauges_to_plot)
    }


  }

  ## facetting- happens in common for all plot types


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
