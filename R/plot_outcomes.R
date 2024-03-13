#' Standardized plot functions for toolkit.
#'
#' Provides a common interface to plotting of an `outcome_col`, which may be
#' plotted on y or fill/color or both. Handles consistent data preparation steps
#' for that outcome variable, along with error and data checking. This keeps
#' data arrangement in-function and controlled for consistency and accuracy, no
#' matter what plot type is desired. Currently supports lines, points, bars,
#' maps, and heatmaps. All plots are just standardizatons of
#' [ggplot2::ggplot()], and return `ggplot` objects that can then be tweaked
#' with standard ggplot calls. **Unlike ggplot, arguments that would typically
#' go in [ggplot2::aes()] should not be bare names, but characters**.
#'
#' I do *not* provide an argument to drop legend labels on purpose- it's
#' dangerous to auto-drop them. The user can always `+ theme(legend.position =
#' 'none')` to the returned object. As much as possible, arguments for specific
#' plot types have been put in `*_list` arguments, which should be NULL
#' (typically the default) or lists of the arguments that the user wants to
#' change.
#'
#'
#' @param outdf dataframe of outcomes
#' @param outcome_col character, name of outcome column. This is the data of
#'   interest being plotted. All data operations in [plot_data_prep()] happen to
#'   this column, and [plot_style_prep()] bases styling on it. For 2d plots, it
#'   is the y-axis, and for maps, heatmaps, and network plots, it is color/fill.
#'   It is entirely possible to also use it as color/fill for 2d plots in
#'   addition to the y-axis.
#' @param outcome_lab character, default `outcome_col`, allows changing label of
#'   the outcome_col (y-axis or color/fill)
#' @param y_col character, default `outcome_col`. Allows separately specifying
#'   y-axis from outcome (e.g. maps, heatmaps, networks)
#' @param y_lab character, default y_col, allows changing y-label
#' @param x_col character, name of column for the x-axis. Default `'scenario'`
#' @param x_lab character, default `x_col`, allows changing x-label
#' @param facet_row NULL (default) or character for facet row. Can be `'.'` to
#'   have one row and column-facets
#' @param facet_col NULL (default) or character for facet column. Can be `'.'`
#'   to have one column and row-facets
#' @param facet_wrapper NULL (default) or character for column to use for
#'   `facet_wrap`
#' @param colorset character, column name to use for color or fill
#' @param color_lab character, default either `colorgroups` or `colorset` (if
#'   `is.null(colorgroups)`). Allows changing the color legend label
#' @param colorgroups character, column name for grouping `colorset` to allow
#'   multiple palettes. Default NULL just uses colorset to define color
#' @param point_group character, column to add additional point groupings to,
#'   e.g. if plotting color by an environmental group, but need separate
#'   lines/points for each environmental objective.
#' @param pal_list list of palettes for defining colors for `colorset`. Should
#'   be length of `colorgroups`
#' @param pal_direction vector of length pal_list, either 1 (default) or -1 (reversed) direction of the palettes
#' @param sceneorder Default NULL, otherwise, character or factor giving the
#'   order to present scenario levels
#' @param scales facet scales, as in [ggplot2::facet_wrap()]. Default `scales =
#'   'fixed'` holds them the same, most common change will be to `scales =
#'   'free_y'` if e.g. gauges have very different flows.
#' @param transoutcome transformation for outcome as in
#'   [ggplot2::scale_y_continuous()] or [ggplot2::scale_fill_continuous()].
#'   Default `transoutcome = 'identity'` just uses the data. Most common change
#'   likely `transoutcome = 'log10`
#' @param transy transformation of y-axis, *if the outcome is not y*. Default
#'   `'identity'`. Ignored if outcome_col is on the y-axis
#' @param transx transformation for x axis as in
#'   [ggplot2::scale_x_continuous()]. Default `transx = 'identity'` just uses
#'   the data. Most common change likely `transx = 'log10`.
#' @param position character or `position` function, `position` arguments from
#'   [ggplot2::geom_col()] and [ggplot2::geom_point()] (depending on plot type),
#'   to change from stacked to dodged bars or jitter points. Can be character,
#'   e.g. 'jitter' or a function, e.g. `ggplot2::position_jitter(width = 0.1,
#'   height = 0)`
#' @param map_outlinecolor color specification for the outline of filled areas on maps. Default 'grey35' seems to be the sf default. NA removes the outline.
#' @param base_list NULL (default) or named list of arguments to
#'   [baseline_compare()];
#'  * base_lev
#'  * comp_fun
#'  * group_cols
#'   [plot_data_prep()] handles `zero_adjust`, and other arguments are inferred or
#'   not supported
#'   If `comp_fun` is 'difference' or 'relative', midpoint auto-set at 0 or 1
#'   respectively unless otherwise specified with `setLimits`
#' @param smooth_arglist NULL (default) or limited list of arguments to
#'   [ggplot2::geom_smooth()]. If NULL and x is quantitative, defaults to
#'   straight lines. If just `list()`, it uses the defaults in [ggplot2::geom_smooth()].
#'   Available arguments:
#' * method
#' * method.args
#' * se
#' * linewidth
#' * alpha
#'   If others are desired, we can develop something more general.
#' @param underlay_list default NULL, otherwise named list (or list of named
#'   lists for multiple underlay levels) of arguments to plot a map underlying
#'   the main map data. Only required value is either `'underlay'` or
#'   `'underover'` (interchangeable), defining the data to be plotted. Other
#'   arguments as for the main plot. Typical examples
#'   * `underlay_list = 'basin'` simply plot the basin outline
#'   * `underlay_list = list(underlay = 'basin', pal_list = 'azure')` same, but filled
#'   * `underlay_list = list(underlay = outputs_on_sdl_scale, outcome_col = ewr_achieved, pal_list = 'scico::oslo')` plot the ewr outcomes in the sdls
#'   A named palette (e.g. variation in color/fill) only works if the main data
#'   is not the same type- we can't use different palettes for underlay fill and
#'   main data fill, for example, but can if the underlay is fill (polygons) and
#'   the main data is points.
#' @param overlay_list as `underlay_list`, but first item might be named
#'   `"overlay` or `"underover"`
#' @param contour_arglist default `NULL`
#'  * NULL builds a heatmap with [ggplot2::geom_tile()],
#'  * `list(interpolate = TRUE)` builds an interpolated heatmap with [ggplot2::geom_raster()] with `interpolate = TRUE`
#'  * `list()` builds a [ggplot2::geom_contour()] with all defaults
#'  * a named list with names other than 'interpolate' passes those as arguments to [ggplot2::geom_contour()]
#' @param setLimits sets user-supplied color/fill limits or midpoints for maps,
#'   heatmaps, and networks, or y limits for other plots. Also sets `underlay`
#'   and `overlay` limits for consistency.
#'  * NULL (default) use internally-calculated limits
#'  * length-1 numeric, sets midpoint, limits stay auto-calculated. V. useful for divergent
#'  * length-2 numeric, sets lower and upper limits
#'  * length-3 numeric, sets limits and midpoint `c(lower, mid, upper)`, with caveat that one limit may be ignored (must be symmetrical about the midpoint)
#'  * if base_list is 'difference' or 'relative', midpoints are auto-set at 0 or 1 (respectively) unless otherwise specified here.
#' @param plot_type one of '2d' (default), 'heatmap', 'contour', 'map', or
#'   'network'. Typically, '2d' can be inferred. Both 'heatmap' and 'contour'
#'   call `plot_heatmap`, but do so differently. 'network' is being held for
#'   future use (use `make_causal_plot()` in the interim)
#' @param zero_adjust adjustment of zeros in [plot_data_prep()], useful
#'   especially for axis `trans` arguments. See `?plot_data_prep`; this is not
#'   the same as for baselining, which should go in `base_list`
#'
#' @return a ggplot stacked bar plot with standard formatting and data cleaning
#' @export
#'
#' @examples
plot_outcomes <- function(outdf,
                          outcome_col,
                          outcome_lab = outcome_col,
                          y_col = outcome_col,
                          y_lab = y_col,
                          x_col = "scenario",
                          x_lab = x_col,
                          colorset = "scenario",
                          pal_list = "scico::berlin",
                          pal_direction = rep(1, length(pal_list)),
                          colorgroups = NULL,
                          color_lab = ifelse(is.null(colorgroups), colorset, colorgroups),
                          plot_type = "2d",
                          facet_row = NULL,
                          facet_col = NULL,
                          facet_wrapper = NULL,
                          point_group = NULL,
                          sceneorder = NULL,
                          scales = "fixed",
                          transoutcome = "identity",
                          transy = "identity",
                          transx = "identity",
                          zero_adjust = 0,
                          position = "stack",
                          map_outlinecolor = 'grey35',
                          base_list = NULL,
                          smooth_arglist = NULL,
                          underlay_list = NULL,
                          overlay_list = NULL,
                          contour_arglist = NULL,
                          setLimits = NULL) {
  ## Cleaning inputs

  if ((!is.null(facet_row) || !is.null(facet_col)) & !is.null(facet_wrapper)) {
    rlang::abort("asking for facet_wrap and facet_grid. Make up your mind.")
  }

  # auto-fill '.' when only rows or cols initialised
  if (is.null(facet_col) & !is.null(facet_row)) {
    facet_col <- "."
  }
  if (is.null(facet_row) & !is.null(facet_col)) {
    facet_row <- "."
  }

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  # Really want to shove this in plot_prep. Maybe I can now that it's in a list?
  if (!is.null(base_list)) {
    if (is.function(base_list$comp_fun) || (is.list(base_list$comp_fun) & is.function(base_list$comp_fun[[1]]))) {
      base_list$comp_fun <- as.character(substitute(base_list$comp_fun))
      if (base_list$comp_fun[1] == "c") {
        base_list$comp_fun <- base_list$comp_fun[2:length(base_list$comp_fun)]
      }
    }
  }

  # infer plot_type to cover some previous behavior
  if (x_col == "map") {
    plot_type <- "map"
  }

  if (!(plot_type %in% c("map", "heatmap", "network", "contour"))) {
    if (y_col != outcome_col) {
      rlang::warn("y-axis in a 2-d plot should be the outcome. If you have a case where that isn't true, please raise an issue")
    }
  }

  if ((plot_type %in% c("map", "heatmap", "network", "contour"))) {
    if (colorset != outcome_col) {
      rlang::warn("color/fill in a 3-d plot or network should be the outcome. If you have a case where that isn't true, please raise an issue")
    }
  }


  # Prep the data
  prepped <- plot_data_prep(
    data = outdf, outcome_col = outcome_col,
    sceneorder = sceneorder,
    base_list = base_list,
    zero_adjust = zero_adjust
  )

  # if y and color are the same, and we've just adjusted y, change color as well
  if (outcome_col == colorset) {
    colorset <- prepped$outcome_col
  }

  prepped <- plot_style_prep(
    prepped = prepped,
    colorset = colorset,
    colorgroups = colorgroups,
    pal_list = pal_list,
    pal_direction = pal_direction,
    transoutcome = transoutcome,
    transx = tranx,
    point_group = point_group
  )

  # We often need to know what x is
  xc <- dplyr::pull(sf::st_drop_geometry(prepped$data[, x_col]))
  xtype <- dplyr::case_when(
    is.numeric(xc) ~ "numeric",
    inherits(xc, "POSIXt") |
      inherits(xc, "Date") ~ "date",
    is.factor(xc) | is.character(xc) ~ "qual"
  )

  # We need to know what y is for heatmaps
  yc <- dplyr::pull(sf::st_drop_geometry(prepped$data[, x_col]))
  ytype <- dplyr::case_when(
    is.numeric(yc) ~ "numeric",
    inherits(yc, "POSIXt") |
      inherits(yc, "Date") ~ "date",
    is.factor(yc) | is.character(xc) ~ "qual"
  )

  ## different plot types

  # Easier to define the 2d (standard) plots as NOT the other sorts
  if (!(plot_type %in% c("map", "heatmap", "network", "contour"))) {
    # we want to use numeric x for numeric x or dates

    # Numeric x
    if (xtype %in% c("numeric", "date")) {
      outcome_plot <- plot_numeric(
        prepped = prepped, x_col = x_col,
        x_lab = x_lab, outcome_lab = outcome_lab,
        position = position,
        transoutcome = transoutcome, transx = transx,
        smooth_arglist = smooth_arglist,
        xtype = xtype
      )
    }
    # Qualitative x
    if (xtype == "qual") {
      outcome_plot <- plot_bar(
        prepped = prepped, x_col = x_col,
        x_lab = x_lab, outcome_lab = outcome_lab,
        position = position, transoutcome = transoutcome
      )
    }

    # Adjustments to all 2d plots
    # y limits
    if (!is.null(setLimits)) {
      new_y_lims <- find_limits(limcol = sf::st_drop_geometry(prepped$data[prepped$outcome_col]),
                                lims = setLimits,
                               trans = transoutcome,
                               base_list = base_list)
      outcome_plot <- outcome_plot + ggplot2::coord_cartesian(ylim = new_y_lims)
    }
    # labels and themes and facets. Can I really get away with doing color and
    # fill here, or do they need to be more granular and inside the respective
    # functions?
    outcome_plot <- outcome_plot +
      ggplot2::labs(
        y = paste0(outcome_lab, prepped$ylab_append),
        x = x_lab,
        color = color_lab,
        fill = color_lab
      ) +
      theme_werp_toolkit()

    if (!is.null(facet_row) & !is.null(facet_col)) {
      outcome_plot <- outcome_plot +
        # reformulate is (RHS, LHS) - so, backwards to how we think about
        # row-column indexing.
        ggplot2::facet_grid(stats::reformulate(facet_col, facet_row),
          # Good in theory, but often too long and blocks the plot, so not using
          # labeller = ggplot2::label_wrap_gen(),
          scales = scales
        )
    }

    if (!is.null(facet_wrapper)) {
      outcome_plot <- outcome_plot +
        ggplot2::facet_wrap(facet_wrapper,
          # Good in theory, but often too long and blocks the plot, so not using
          # labeller = ggplot2::label_wrap_gen(),
          scales = scales
        )
    }
  }

  if (plot_type == "map") {
    if (!inherits(pal_list, 'colors') && length(pal_list) > 1) {
      rlang::warn(glue::glue("using first palette for {outcome_col}.
                             Splitting up palettes in maps needs more thought"))
    }

    # need a lot of arguments here because we might need to fully build several
    # plots in the under/overlays
    outcome_plot <- plot_map(
      prepped = prepped,
      underlay_list = underlay_list,
      overlay_list = overlay_list,
      map_outlinecolor = map_outlinecolor,
      outcome_lab = outcome_lab,
      facet_wrapper = facet_wrapper,
      facet_row = facet_row, facet_col = facet_col,
      sceneorder = sceneorder, transoutcome = transoutcome,
      setLimits = setLimits, base_list = base_list
    )

    # labels
    outcome_plot <- outcome_plot +
      ggplot2::labs(
        y = NULL,
        x = NULL
      ) +
      theme_werp_toolkit()
  }

  if (plot_type %in% c("heatmap", "contour")) {
    if (length(pal_list) > 1) {
      rlang::warn(glue::glue("using first palette for {outcome_col}.
                             Splitting up palettes in heatmaps needs more thought"))
    }

    # # unlike maps, these plots do not automatically break things up by
    # geometry, and so to avoid accidentally depending on that
    prepped$data <- sf::st_drop_geometry(prepped$data)

    # Check whether we're accidentally overplotting. This either completes
    # silently or aborts with a message
    # Q: are matchign x-y with different z OK if contouring?
    test_overplotting(
      data = prepped$data,
      facet_wrapper = facet_wrapper,
      facet_row = facet_row,
      facet_col = facet_col,
      x_col = x_col,
      y_col = y_col
    )

    outcome_plot <- plot_heatmap(
      prepped = prepped,
      x_col = x_col, x_lab = x_lab,
      y_col = y_col, y_lab = y_lab,
      outcome_lab = outcome_lab,
      transoutcome = transoutcome, transx = transx, transy = transy,
      contour_arglist = contour_arglist,
      xtype = xtype, ytype = ytype,
      setLimits = setLimits, base_list = base_list
    )

    # labels
    outcome_plot <- outcome_plot +
      ggplot2::labs(
        y = y_lab,
        x = x_lab,
        fill = paste0(outcome_lab, prepped$ylab_append)
      ) +
      theme_werp_toolkit()

    # Facetting would sure be better all at once if it worked for maps.
    if (!is.null(facet_row) & !is.null(facet_col)) {
      outcome_plot <- outcome_plot +
        # reformulate is (RHS, LHS) - so, backwards to how we think about
        # row-column indexing.
        ggplot2::facet_grid(stats::reformulate(facet_col, facet_row),
          # Good in theory, but often too long and blocks the plot, so not using
          # labeller = ggplot2::label_wrap_gen(),
          scales = scales
        )
    }

    if (!is.null(facet_wrapper)) {
      outcome_plot <- outcome_plot +
        ggplot2::facet_wrap(facet_wrapper,
          # Good in theory, but often too long and blocks the plot, so not using
          # labeller = ggplot2::label_wrap_gen(),
          scales = scales
        )
    }
  }

  return(outcome_plot)
}
