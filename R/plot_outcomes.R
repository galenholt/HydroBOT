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
#' @param transx transformation for x axis as in
#'   [ggplot2::scale_x_continuous()]. Default `transx = 'identity'` just uses
#'   the data. Most common change likely `transx = 'log10`
#' @param position character or `position` function, `position` arguments from
#'   [ggplot2::geom_col()] and [ggplot2::geom_point()] (depending on plot type),
#'   to change from stacked to dodged bars or jitter points. Can be character,
#'   e.g. 'jitter' or a function, e.g. `ggplot2::position_jitter(width = 0.1,
#'   height = 0)`
#' @param smooth, logical, default `FALSE`. If plotting lines (`x_col` is
#'   numeric), do we want straight lines (the default, [ggplot2::geom_line()])
#'   or fits ([ggplot2::geom_smooth()])?
#' @param smooth_method `method` argument to [ggplot2::geom_smooth()]. Ignored
#'   if not smoothing
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
                                  zero_adjust = 0,
                                  position = 'stack',
                                  smooth = FALSE,
                                  smooth_method = NULL,
                                  smooth_se = TRUE,
                                  underlay_list = NULL,
                                  overlay_list = NULL,
                                  setLimits = NULL,
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
                       comp_fun = comp_fun,
                       zero_adjust = zero_adjust, ...)

  # That can introduce some infs and nans, often from division by zero in the relativiser
  new_nan_inf <- sum(is.nan(prepped$data[[prepped$y_col]])) + sum(is.infinite(prepped$data[[prepped$y_col]]))
  old_nan_inf <- sum(is.nan(prepped$data[[y_col]])) + sum(is.infinite(prepped$data[[y_col]]))
  if (new_nan_inf > old_nan_inf) {
    rlang::warn(glue::glue("NaN and Inf introduced in `plot_prep`, likely due to division by zero. {new_nan_inf - old_nan_inf} values were lost."))
  }

  if (!inherits(transy, 'trans') && transy %in% c('log', 'log10') & any(prepped$data[[prepped$y_col]] == 0)) {
    rlang::warn(glue::glue("`transy` takes logs, but data has
                           {sum(prepped$data[[prepped$y_col]] == 0)} zeros and
                           {sum(prepped$data[[prepped$y_col]] < 0)} less than 0,
                           which will get lost"))
  }


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

    # end-run a situation where we color and x by scenario- need to clean this up
    if (all(names(pal_list) %in% prepped$data$scenario)) {
      prepped$data <- dplyr::arrange(prepped$data, scenario)
    }

    outcome_plot <- prepped$data |>
      dplyr::filter(scenario %in% prepped$scenariofilter) |>
      ggplot2::ggplot(ggplot2::aes(x = scenario, y = .data[[prepped$y_col]],
                                   fill = forcats::fct_inorder(color))) +
      ggplot2::geom_col(position = position) +
      ggplot2::labs(y = paste0(y_lab, prepped$ylab_append), x = x_lab,
                    color = colorset) +
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
                                     group = .data$pointgroup)) +
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

    # Find limits for the color scale- allows centering diverging palettes with baseline comparisons
    # by defining this in here, it inherits comp_fun and other values
    # x is typically prepped$data[[prepped$y_col]] (because `prepped$y_col` is
    # the new y_col name if baselined). Should I just use that? Or can I call it from inside the `scale`
    # x here is a length-2 default set of limits.
    findlimits <- function(x) {
      # use hard-set user-supplied limits
      if (!is.null(setLimits)) {
        return(setLimits)
      }
      # use default limits
      if (is.null(comp_fun) || (!comp_fun %in% c('difference', 'relative')) &
          is.null(setLimits)) {return(x)}
      # Set limits to make midpoint 0 if difference
      if (comp_fun == 'difference') {
        limits <- max(abs(x), na.rm = TRUE) * c(-1, 1)
      }
      # setting midpoint at 1 for multiplicative is trickier
      if (comp_fun == 'relative') {
        logvals <- log(x)
        loglims <- max(abs(logvals), na.rm = TRUE) * c(-1, 1)
        limits <- exp(loglims)
      }

      return(limits)

    }

    ## START BUILDING THE PLOT
    outcome_plot <- ggplot2::ggplot()

    # underlay
    if (!is.null(underlay_list)) {
      # Handle multiple levels and a flat list or a list of lists-
      # Allow a short-circuit passing character, e.g. 'basin'
      if (is.character(underlay_list)) {underlay_list <- list(underlay = get(underlay_list), underlay_pal = NA)}
      # if flat put it as the first list in a length-one list for generality
      if ('underlay' %in% names(underlay_list)) {underlay_list <- list(underlay_list)}

      for (u in underlay_list) {
        # Allow passing names
        if (is.character(u$underlay)) {u$underlay <- get(u$underlay)}

        # deal with ordering scenarios if the underlay has them- otherwise the facets lose the ordering.
        # We really should be `plot_prep` ing the underlays and overlays, or
        # at least some subset of `plot_prep`
        if ('scenario' %in% names(u$underlay)) {
          # Order the scenario if I've given it an order.
          if (!is.null(sceneorder)) {
            if (inherits(sceneorder, 'factor')) {sceneorder <- levels(sceneorder)}
            u$underlay <- u$underlay |>
              dplyr::mutate(scenario = forcats::fct_relevel(scenario, sceneorder))
          }
        }

        # use `grouped_colors` to set colour groups
        # Do we need to pass separate colorgroups and colorset to the underlay? Likely
        # undercolor <- grouped_colours(underlay, underlay_pal, underlay_colorgroups, underlay_colorset)

        # catch case with multiple fills- need a single color if the main data has a fill scale
        # I need something here to catch whether underlay_pal is  a palette or a color
        # If the main data is a polygon, or if we are only passed a single color value, just use a single color value

        if (all(sf::st_is(prepped$data, c("POLYGON", "MULTIPOLYGON"))) &
            grepl("::", u$underlay_pal)) {
          rlang::warn("Trying to have fill scale for underlay and main data. Removing for underlay")
          u$underlay_pal = NA
        }

        if (u$underlay_pal %in% colors() |
            is.na(u$underlay_pal) |
            grepl("#", u$underlay_pal)) {

          outcome_plot <- outcome_plot +
            ggplot2::geom_sf(data = u$underlay, fill = u$underlay_pal)
        }

        # If the main data is points, we can use fill for the underlay, but only if underlay_pal is a palette name
        if (all(sf::st_is(prepped$data, c("POINT", "LINESTRING"))) &
            grepl("::", u$underlay_pal)) {


          if (is.numeric(dplyr::pull(sf::st_drop_geometry(u$underlay[, u$underlay_ycol])))) {
            # I don't `plot_prep` or `baseline_compare` the under- or overlays,
            # so don't use `trans` or `limit` in the scale call. If we want to
            # change that, we can, but would need to institute running
            # plot_prep, I think
            outcome_plot <- outcome_plot +
              ggplot2::geom_sf(data = u$underlay,
                               ggplot2::aes(fill = .data[[u$underlay_ycol]])) +
              paletteer::scale_fill_paletteer_c(palette = u$underlay_pal)
          } else {
            undercols <- make_pal(unique(dplyr::pull(sf::st_drop_geometry(u$underlay[,u$underlay_ycol]))), palette = u$underlay_pal)
            outcome_plot <- outcome_plot +
              ggplot2::geom_sf(data = u$underlay,
                               ggplot2::aes(fill = .data[[u$underlay_ycol]])) +
              ggplot2::scale_fill_manual(values = undercols)
          }


        }

      }
    }


    # I could use the same method as above with scale_fill_manual, e.g.
    # outcome_plot <- outcome_plot +
    #   ggplot2::geom_sf(data = prepped$data |>
    #                      dplyr::filter(scenario %in% prepped$scenariofilter),
    #                    ggplot2::aes(fill = color)) +
    #   ggplot2::scale_fill_identity(guide = 'legend',
    #                                labels = labfind,
    #                                name = color_lab) +
    #   # ahhh. reformulate is (RHS, LHS) - so, backwards to what we want.
    #   ggplot2::facet_grid(reformulate(facet_col,facet_row))
    # outcome_plot

    # but that would require more manipulation of the legend- it produces a
    # legend with value examples, and in a weird order. So we'd need to clean up
    # the names, do some sorting, etc. The issue is this is the only place I'm
    # using color/fill as a numeric, and so the `plot_prep`-->
    # `scale_fill_manual` method works, but is hackier. I might have to make
    # `plot_prep` more general at some point in the future if we do want a
    # cleaner, more consistent approach.
    if (all(sf::st_is(prepped$data, c("POINT", "LINESTRING")))) {
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = prepped$data |>
                           dplyr::filter(scenario %in% prepped$scenariofilter),
                         ggplot2::aes(color = .data[[prepped$y_col]])) +
        paletteer::scale_color_paletteer_c(palette = pal_list[[1]],
                                           trans = transy, limit = findlimits) +
        ggplot2::labs(color = paste0(y_lab, prepped$ylab_append)) +
        theme_werp_toolkit()
    }
    if (all(sf::st_is(prepped$data, c("POLYGON", "MULTIPOLYGON")))) {
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = prepped$data |>
                           dplyr::filter(scenario %in% prepped$scenariofilter),
                         ggplot2::aes(fill = .data[[prepped$y_col]])) +
        paletteer::scale_fill_paletteer_c(palette = pal_list[[1]],
                                          trans = transy, limit = findlimits) +
        ggplot2::labs(fill = paste0(y_lab, prepped$ylab_append)) +
        theme_werp_toolkit()

      # outcome_plot + ggplot2::facet_grid(reformulate(facet_col,facet_row))
      # outcome_plot <- op
    }

    # The facetting is really causing issues for maps below. Try to do the map facets here
    if (!is.null(facet_wrapper)) {
      outcome_plot <- outcome_plot +
        ggplot2::facet_wrap(facet_wrapper)
    }

    if (!is.null(facet_row) & !is.null(facet_col)) {
      outcome_plot <- outcome_plot +
        # ahhh. reformulate is (RHS, LHS) - so, backwards to what we want.
        ggplot2::facet_grid(reformulate(facet_col,facet_row))
    }


    # overlay
    if (!is.null(overlay_list)) {
      # Handle multiple levels and a flat list or a list of lists-
      # Allow a short-circuit passing character, e.g. 'bom_basin_gauges'
      # Setting the color to black because this is color and so will be a polygon outline or points
      if (is.character(overlay_list)) {overlay_list <- list(overlay = get(overlay_list), overlay_pal = 'black')}
      # if flat put it as the first list in a length-one list for generality
      if ('overlay' %in% names(overlay_list)) {overlay_list <- list(overlay_list)}

      for (o in overlay_list) {
        # Allow passing names
        if (is.character(o$overlay)) {o$overlay <- get(o$overlay)}

        # deal with ordering scenarios if the overlay has them- otherwise the facets lose the ordering.
        # We really should be `plot_prep` ing the underlays and overlays, or
        # at least some subset of `plot_prep`
        if ('scenario' %in% names(o$overlay)) {
          # Order the scenario if I've given it an order.
          if (!is.null(sceneorder)) {
            if (inherits(sceneorder, 'factor')) {sceneorder <- levels(sceneorder)}
            o$overlay <- o$overlay |>
              dplyr::mutate(scenario = forcats::fct_relevel(scenario, sceneorder))
          }
        }

        # clip to main data automatically
        if ('clip' %in% names(o) && o$clip) {
          o$overlay <- sf::st_filter(o$overlay, dplyr::distinct(overplot_test))
        }

        # catch case with multiple colors- need a single color if the main data has a color scale
        # I need something here to catch whether overlay_pal is  a palette or a color

        if (all(sf::st_is(prepped$data, "POINT")) &
            grepl("::", o$overlay_pal)) {
          if (all(sf::st_is(o$overlay, "POINT"))) {
            o$overlay_pal = 'black'
          } else {
            o$overlay_pal = NA
          }
          rlang::warn(glue::glue("Trying to have color scale for overlay and main data.
                      Removing for overlay and setting to {o$overlay_pal}"))

        }

        if (o$overlay_pal %in% colors() |
            is.na(o$overlay_pal) |
            grepl("#", o$overlay_pal)) {

          outcome_plot <- outcome_plot +
            ggplot2::geom_sf(data = o$overlay, color = o$overlay_pal, fill = NA)
        }

        # If the main data is polygons, we can use color for the overlay, but
        # only if overlay_pal is a palette name
        if (all(sf::st_is(prepped$data, c("POLYGON", "MULTIPOLYGON"))) &
            grepl("::", o$overlay_pal)) {


          if (is.numeric(dplyr::pull(sf::st_drop_geometry(o$overlay[, o$overlay_ycol])))) {
            # I don't `plot_prep` or `baseline_compare` the under- or overlays,
            # so don't use `trans` or `limit` in the scale call. If we want to
            # change that, we can, but would need to institute running
            # plot_prep, I think
            outcome_plot <- outcome_plot +
              ggplot2::geom_sf(data = o$overlay,
                               ggplot2::aes(color = .data[[o$overlay_ycol]]), fill = NA) +
              paletteer::scale_color_paletteer_c(palette = o$overlay_pal)
          } else {
            overcols <- make_pal(unique(dplyr::pull(sf::st_drop_geometry(o$overlay[,o$overlay_ycol]))),
                                 palette = o$overlay_pal)
            outcome_plot <- outcome_plot +
              ggplot2::geom_sf(data = o$overlay,
                               ggplot2::aes(color = .data[[o$overlay_ycol]]), fill = NA) +
              ggplot2::scale_color_manual(values = overcols)
          }


        }

      }
    }
  }

  ## facetting- happens in common for all plot types *except maps*- the scales
  ## argument messes up maps. I really should just separate out the maps.


  if (x_col != 'map') {
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
  }

  return(outcome_plot)
}
