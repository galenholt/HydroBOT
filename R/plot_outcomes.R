#' Plot stacked bar graphs of outcomes across scenarios
#'
#' Has several ways to color, including grouped palettes. Lets us choose whether
#' to stack scenarios with outcome groups on x (`scene_x = FALSE`) or stack
#' outcome groups with scenarios on x (`scene_x = TRUE`). I do *not* provide an
#' argument to drop legend labels on purpose- it's dangerous to auto-drop them.
#' The user can always `+ theme(legend.position = 'none')` to the returned
#' object.
#'
#' @inheritParams plot_prep
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
#' @param scales facet scales, as in [ggplot2::facet_wrap()]. Default `scales =
#'   'fixed'` holds them the same, most common change will be to `scales =
#'   'free_y'` if gauges have very different flows.
#' @param transy transformation for y axis as in [ggplot2::scale_y_continuous()].
#'   Default `transy = 'identity'` just uses the data. Most common change likely
#'   `transy = 'log10`
#' @param transx transformation for x axis as in
#'   [ggplot2::scale_x_continuous()]. Default `transx = 'identity'` just uses
#'   the data. Most common change likely `transx = 'log10`
#' @param position character or `position` function, `position` arguments from
#'   [ggplot2::geom_col()] and [ggplot2::geom_point()] (depending on plot type),
#'   to change from stacked to dodged bars or jitter points. Can be character,
#'   e.g. 'jitter' or a function, e.g. `ggplot2::position_jitter(width = 0.1,
#'   height = 0)`
#' @param base_list NULL (default) or limited list of arguments to [baseline_compare()];
#'  * base_lev
#'  * comp_fun
#' * group_cols
#'  [plot_prep()] handles `zero_adjust`, and other arguments are inferred or not supported
#' @param smooth_arglist NULL (default) or limited list of arguments to [ggplot2::geom_smooth()]. If NULL and x is quantitative, defaults to straight lines.
#' Arguments
#' * method
#' * method.args
#' * se
#' * linewidth
#' If others are desired, we can develop something more general.
#' @param smooth_method `method` argument to [ggplot2::geom_smooth()]. Ignored
#'   if not smoothing
#' @param smooth_args `method.args` argument to [ggplot2::stat_smooth()]. Ignored if not smoothing.
#' @param smooth_se `se` argument to [ggplot2::geom_smooth()]. Ignored if not
#'   smoothing
#' @param underlay_list default NULL, named list (or list of named lists for
#'   multiple underlay levels) of arguments to plot a map underlying the main
#'   map data. Names define arguments, `underlay` is required, either character
#'   or an sf, `underlay_pal` do define colors, can be single color or paletteer
#'   name, and `underlay_ycol` if coloring the underlay by values. Multiple
#'   values (e.g. having an `underlay_ycol` and a palette for `underlay_pal`)
#'   only works if the main data is not the same type- we can't use different
#'   palettes for underlay fill and main data fill, for example, but can if the
#'   underlay is fill (polygons) and the main data is points.
#' @param overlay_list as `underlay_list`, but names `"overlay_*"`
#' @param setLimits sets user-supplied color/fill limits for maps or y limits for other plots. Also sets `underlay` and `overlay` limits for consistency.
#'
#' @return a ggplot stacked bar plot with standard formatting and coloring,
#'   stacking either scenarios or colorset
#' @export
#'
#' @examples
plot_outcomes <- function(outdf,
                                  y_col,
                                  y_lab = y_col,
                                  x_col = 'scenario',
                                  x_lab = x_col,
                          colorset = 'scenario',
                          pal_list = "scico::berlin",
                          colorgroups = NULL,
                          color_lab = ifelse(is.null(colorgroups), colorset, colorgroups),
                          plot_type = '2d',
                                  facet_row = NULL,
                                  facet_col = NULL,
                                  facet_wrapper = NULL,

                                  point_group = NULL,
                          sceneorder = NULL,
                                  scales = 'fixed',
                                  transy = 'identity',
                                  transx = 'identity',
                          zero_adjust = 0,

                                  position = 'stack',
                          base_list = NULL,
                                  smooth_arglist = NULL,
                                  underlay_list = NULL,
                                  overlay_list = NULL,
                                  setLimits = NULL) {


  ## Cleaning inputs

  if ((!is.null(facet_row) || !is.null(facet_col)) & !is.null(facet_wrapper)) {
    rlang::abort("asking for facet_wrap and facet_grid. Make up your mind.")
  }

  # auto-fill '.' when only rows or cols initialised
  if (is.null(facet_col) & !is.null(facet_row)) {facet_col <- '.'}
  if (is.null(facet_row) & !is.null(facet_col)) {facet_row <- '.'}

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  # Really want to shove this in plot_prep. Maybe I can now that it's in a list?
  if (!is.null(base_list)) {
    if (is.function(base_list$comp_fun) || (is.list(base_list$comp_fun) & is.function(base_list$comp_fun[[1]]))) {
      base_list$comp_fun <- as.character(substitute(base_list$comp_fun))
      if(base_list$comp_fun[1] == "c") {base_list$comp_fun <- base_list$comp_fun[2:length(base_list$comp_fun)]}
    }
  }

  # infer plot_type to cover some previous behavior
  if (x_col == 'map') {
    plot_type <- 'map'
  }


  # Prep the data
  prepped <- plot_data_prep(data = outdf, y_col = y_col,
                       sceneorder = sceneorder,
                       base_list = base_list,
                       zero_adjust = zero_adjust)

  # if y and color are the same, and we've just adjusted y, change color as well
  if (y_col == colorset) {
    colorset <- prepped$y_col
  }

  prepped <- plot_style_prep(prepped = prepped,
                             colorset, colorgroups, pal_list,
                             transy, transx, point_group)

  ## different plot types

  # Easier to define the 2d (standard) plots as NOT the other sorts
  if (!(plot_type %in% c('map', 'heatmap', 'network'))) {
    # Numeric x
    if (is.numeric(dplyr::pull(sf::st_drop_geometry(prepped$data[,x_col])))) {
      outcome_plot <- plot_numeric(prepped = prepped, x_col = x_col,
                                   x_lab = x_lab, y_lab = y_lab,
                                   position = position,
                                   transy = transy, transx = transx,
                                   smooth_arglist = smooth_arglist)
    }
    # Qualitative x
    if (!is.numeric(dplyr::pull(sf::st_drop_geometry(prepped$data[,x_col])))) {
      outcome_plot <- plot_bar(prepped = prepped, x_col = x_col,
                               x_lab = x_lab, y_lab = y_lab,
                               position = position, transy = transy)
    }

    # Adjustments to all 2d plots
      # y limits
    if (!is.null(setLimits)) {
      outcome_plot <- outcome_plot + ggplot2::coord_cartesian(ylim = setLimits)
    }
    # labels and themes and facets. Can I really get away with doing color and
    # fill here, or do they need to be more granular and inside the respective
    # functions?
    outcome_plot <- outcome_plot +
      ggplot2::labs(y = paste0(y_lab, prepped$ylab_append),
                    x = x_lab,
                    color = color_lab,
                    fill = color_lab) +
      theme_werp_toolkit()

    if (!is.null(facet_row) & !is.null(facet_col)) {

      outcome_plot <- outcome_plot +
        # reformulate is (RHS, LHS) - so, backwards to how we think about
        # row-column indexing.
        ggplot2::facet_grid(stats::reformulate(facet_col,facet_row),
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

  }

  if (plot_type == 'map') {
    if (length(pal_list) > 1) {rlang::warn(glue::glue("using first palette for {y_col}.
                                                      Splitting up palettes in maps needs more thought"))}

    # need a lot of arguments here because we might need to fully build several
    # plots in the under/overlays
    outcome_plot <- plot_map(prepped = prepped,
                             underlay_list = underlay_list, overlay_list = overlay_list,
                             y_lab = y_lab,
                             facet_wrapper = facet_wrapper,
                             facet_row = facet_row, facet_col = facet_col,
                             sceneorder = sceneorder, transy = transy,
                             setLimits = setLimits, base_list = base_list)

    # labels
    outcome_plot <- outcome_plot +
      ggplot2::labs(y = NULL,
                    x = NULL) +
      theme_werp_toolkit()
  }

  return(outcome_plot)
}
