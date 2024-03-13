#' Make a bar plot (qualitative x, usually)
#'
#' main function to make a standard bar plot with [ggplot2::geom_col()]
#'
#' @inheritParams plot_outcomes
#'
#' @param prepped prepared data from `plot_data_prep() |>  plot_style_prep()`
#'
#' @return
#' @export
#'
#' @examples
plot_bar <- function(prepped, x_col, x_lab, outcome_lab,
                     position, transoutcome) {
  outcome_plot <- prepped$data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[prepped$outcome_col]],
      fill = .data$color
    )) +
    ggplot2::geom_col(position = position) +
    ggplot2::scale_y_continuous(trans = transoutcome) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis())


  outcome_plot <- handle_palettes(outcome_plot,
    aes_type = "fill",
    prepped$pal_list,
    prepped$color_type,
    pal_direction = prepped$direction
  )

  return(outcome_plot)
}


#' Make a plot with a numeric x
#'
#' Often (but not always) this is used when scenario is defined numerically.
#' Uses [ggplot2::geom_line()] or [ggplot2::geom_smooth()]. Takes a
#' `smooth_arglist` argument to control that smoother
#'
#' @inheritParams plot_outcomes
#'
#' @param prepped prepared data from `plot_data_prep() |>  plot_style_prep()`
#' @param xtype character, if we're here, should be 'numeric' or 'date' (which
#'   covers several different date types). Dates can't be `trans-ed`, so need to
#'   be identified.
#'
#' @return
#' @export
#'
#' @examples
plot_numeric <- function(prepped, x_col, x_lab, outcome_lab,
                         position, transoutcome, transx,
                         smooth_arglist, xtype = "numeric") {
  if (!inherits(position, "gg")) {
    if (is.null(position) || position == "stack") {
      position <- "identity"
    }
  }


  outcome_plot <- prepped$data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[prepped$outcome_col]],
      color = .data$color,
      group = .data$pointgroup
    )) +
    ggplot2::scale_y_continuous(trans = transoutcome)

  if (xtype != "date") {
    outcome_plot <- outcome_plot +
      ggplot2::geom_point(position = position) +
      ggplot2::scale_x_continuous(trans = transx)
  }


  # THESE CONDITIONALS ARE A MESS. MAYBE JUST is.logical, is.list, is.null...

  # There must be a slick way to do this, but the rlang::exec and do.call
  # methods don't let use keep using the defaults for other args, as far as I
  # can tell.
  default_smooth <- list(
    method = NULL,
    se = TRUE,
    method.args = NULL,
    linewidth = NULL, # Odd that I have to set this.
    alpha = 0.4
  )


  # Deal with logical carryover
  if (is.logical(smooth_arglist)) {
    if (smooth_arglist) {
      smooth_arglist <- default_smooth
    } else if (!smooth_arglist) {
      smooth_arglist <- NULL
    }
  }

  if (is.null(smooth_arglist)) {
    outcome_plot <- outcome_plot +
      ggplot2::geom_line()
  } else if (!is.null(smooth_arglist)) {
    smooth_arglist <- utils::modifyList(default_smooth, smooth_arglist)

    outcome_plot <- outcome_plot +
      ggplot2::geom_smooth(
        mapping = ggplot2::aes(fill = .data$color),
        method = smooth_arglist$method,
        method.args = smooth_arglist$method.args,
        se = smooth_arglist$se,
        linewidth = smooth_arglist$linewidth,
        alpha = smooth_arglist$alpha
      )
  }


  # do I need to do this for fill as well when I have smooth?
  outcome_plot <- handle_palettes(outcome_plot,
    aes_type = "color",
    pal_list = prepped$pal_list,
    color_type = prepped$color_type,
    pal_direction = prepped$direction)

  outcome_plot <- handle_palettes(outcome_plot,
    aes_type = "fill",
    pal_list = prepped$pal_list,
    color_type = prepped$color_type,
    pal_direction = prepped$direction)

}

#' Title
#'
#' Plots a standard map, with potentially underlay and overlays, specified by
#' `underlay_list` and `overlay_list`. These are recursed through the
#' `plot_*_prep()` functions so the transes and other changes are consistent
#' across the layers. There are some inherent limitations about color/fill and
#' points/polygons due to masking and use of multiple color/fill ramps
#'
#' @inheritParams plot_outcomes
#'
#' @param prepped prepared data from `plot_data_prep() |>  plot_style_prep()`
#'
#' @return
#' @export
#'
#' @examples
plot_map <- function(prepped, underlay_list, overlay_list, map_outlinecolor = 'grey35', outcome_lab,
                     facet_wrapper, facet_row, facet_col,
                     sceneorder, transoutcome, setLimits, base_list) {
  # Check whether we're accidentally overplotting. This either completes
  # silently or aborts with a message
  test_overplotting(data = prepped$data, facet_wrapper, facet_row, facet_col)

  # we need to do different things depending on whether the main data has area
  # (polygon/multipolygon) or not (points, linestring, multi-point, etc) build
  # the plot piece by piece
  if (all(sf::st_is(prepped$data, c("POLYGON", "MULTIPOLYGON")))) {
    maindatatype <- "areal"
  } else if (all(sf::st_is(prepped$data, c("POINT", "LINESTRING", "MULTIPOINT")))) {
    maindatatype <- "point"
  }

  # Clean up the under and overlay lists
  # Underlays, if needed
  if (!is.null(underlay_list)) {
    # Handle multiple levels and a flat list or a list of lists-
    # Allow a short-circuit passing character, e.g. 'basin'
    # sets the color default to NA because this will usually be fill
    if (is.character(underlay_list)) {
      underlay_list <- list(underlay = get(underlay_list), underlay_pal = NA)
    }
    # if flat put it as the first list in a length-one list for generality
    if ("underlay" %in% names(underlay_list) | "underover" %in% names(underlay_list)) {
      underlay_list <- list(underlay_list)
    }
  }

  if (!is.null(overlay_list)) {
    # Handle multiple levels and a flat list or a list of lists-
    # Allow a short-circuit passing character, e.g. 'bom_basin_gauges'
    # Setting the color default to black because overlay shouldn't be fill and so will be a polygon outline or points
    if (is.character(overlay_list)) {
      overlay_list <- list(overlay = get(overlay_list), overlay_pal = "black")
    }
    # if flat put it as the first list in a length-one list for generality
    if ("overlay" %in% names(overlay_list) | "underover" %in% names(overlay_list)) {
      overlay_list <- list(overlay_list)
    }
  }


  # Initialise the plot
  outcome_plot <- ggplot2::ggplot()

  # layer up the underlays
  outcome_plot <- make_underover(
    underover_list = underlay_list,
    outcome_plot = outcome_plot,
    sceneorder = sceneorder,
    outcome_lab = outcome_lab,
    maindata = prepped$data,
    maindatatype = maindatatype,
    maincolorpal = prepped$pal_list,
    transoutcome = transoutcome,
    setLimits = setLimits,
    base_list = base_list, uotype = "underlay"
  )

  # Make the 'main' level. This is sort of silly in some ways, since we could
  # just have one list with the under, main, and over, but giving a level
  # primacy can be important for determining things like who wins color conflicts
  if (maindatatype == "point") {
    outcome_plot <- outcome_plot +
      ggplot2::geom_sf(
        data = prepped$data,
        ggplot2::aes(color = .data$color)
      )
    outcome_plot <- handle_palettes(outcome_plot,
      aes_type = "color",
      pal_list = prepped$pal_list,
      color_type = prepped$color_type,
      transoutcome = transoutcome,
      setLimits = setLimits,
      pal_direction = prepped$direction,
      base_list = base_list
    )

    outcome_plot <- outcome_plot + ggplot2::labs(color = paste0(outcome_lab, prepped$ylab_append))
  }
  if (maindatatype == "areal") {
    outcome_plot <- outcome_plot +
      ggplot2::geom_sf(
        data = prepped$data,
        ggplot2::aes(fill = .data$color), color = map_outlinecolor) + ggplot2::theme_bw()
    outcome_plot <- handle_palettes(outcome_plot,
      aes_type = "fill",
      pal_list = prepped$pal_list,
      color_type = prepped$color_type,
      transoutcome = transoutcome,
      setLimits = setLimits,
      pal_direction = prepped$direction,
      base_list = base_list
    )


    outcome_plot <- outcome_plot + ggplot2::labs(fill = paste0(outcome_lab, prepped$ylab_append))
  }


  # I'm not sure this still has to happen here, but it works so leave it for
  # now. Would be better to do the facet smash for all plots at the end
  # The facetting is really causing issues for maps below. Try to do the map facets here
  if (!is.null(facet_wrapper)) {
    outcome_plot <- outcome_plot +
      ggplot2::facet_wrap(facet_wrapper)
  }

  if (!is.null(facet_row) & !is.null(facet_col)) {
    outcome_plot <- outcome_plot +
      # ahhh. reformulate is (RHS, LHS) - so, backwards to what we want.
      ggplot2::facet_grid(stats::reformulate(facet_col, facet_row))
  }


  # layer up the overlays
  outcome_plot <- make_underover(
    underover_list = overlay_list,
    outcome_plot = outcome_plot,
    sceneorder = sceneorder,
    outcome_lab = outcome_lab,
    maindata = prepped$data,
    maindatatype = maindatatype,
    maincolorpal = prepped$pal_list,
    transoutcome = transoutcome,
    setLimits = setLimits,
    base_list = base_list, uotype = "overlay"
  )

  return(outcome_plot)
}


#' Helper for plot_map. Essentially recurses it to do the underlay and overlay
#' lists
#'
#' This gets run once each for under and overlays. It is itself agnostic to
#' whether it is under or over the 'main' plot.
#'
#' @inheritParams plot_outcomes
#'
#' @param underover_list List of layers to put either under or over the main layer
#' @param outcome_plot The plot, as it stands before the layers in `underover_list` are added
#' @param maindata The data going into the primary ('main') layer
#' @param maindatatype character, 'point' (point, multipoint) or 'areal' (polygon, multipolygon) used to know if the main layer uses a color (if 'point') or fill (if 'areal') aes
#' @param maincolorpal palette for the main data
#' @param uotype character, 'overlay', 'underlay', or 'internal' (default, it is an item in 'underover_list'). Not used, except for compatibility
#'
#' @return
#' @export
#'
#' @examples
make_underover <- function(underover_list,
                           outcome_plot,
                           sceneorder,
                           outcome_lab,
                           maindata,
                           maindatatype,
                           maincolorpal,
                           transoutcome,
                           setLimits,
                           base_list,
                           uotype = "internal") {
  # A full list to allow partial specification with null-filling
  full_list <- list(
    underover = NULL, outcome_col = NULL,
    pal_list = NA, # NA will show something, NULL just fails
    pal_direction = 1,
    colorgroups = NULL,
    map_outlinecolor = 'grey35',
    outcome_lab = outcome_lab, # inherit from outer, but can be overwritten
    base_list = NULL, zero_adjust = 0,
    onlyzeros = FALSE,
    transoutcome = transoutcome,
    transx = NULL,
    point_group = NULL,
    uotype = NULL,
    clip = FALSE
  )

  # To make this function generic, it can have the main piece named 'underover', 'underlay', or 'overlay'
  # but we need some errorchecking

  # We don't currently *use* the uotype entry in the underover_list, but this is
  # here to provide first backwards compatibility, but also set us up with the
  # backbone to just do all the layers in one go.
  settype <- function(x, typename) {
    if (typename %in% names(x)) {
      names(x)[names(x) == typename] <- "underover"
    }

    names(x)[names(x) %in% c("underlay_pal", "overlay_pal")] <- "pal_list"
    names(x)[names(x) %in% c("underlay_ycol", "overlay_ycol")] <- "outcome_col"

    x$uotype <- typename
    return(x)
  }



  if (uotype == "underlay") {
    underover_list <- purrr::map(underover_list, \(x) settype(x, typename = "underlay"))
  }

  if (uotype == "overlay") {
    underover_list <- purrr::map(underover_list, \(x) settype(x, typename = "overlay"))
  }

  if (uotype == "internal") {
    typeexists <- all(purrr::map_lgl(underover_list, \(x) "uotype" %in% names(x)))
    if (!typeexists) {
      rlang::abort("asking for underlay or overlay, but have not specified a uotype. Do this either with separate underlay_list and overlay_list or a single list with a specified outype item")
    }
  }


  # there might be several layers, so loop
  for (uo in underover_list) {
    # Allow passing names
    if (is.character(uo$underover)) {
      uo$underover <- get(uo$underover)
    }

    # NULL-fill slots without arguments
    uo <- modifyList(full_list, uo)

    # clip to the main geometry
    if (uo$clip) {
      uo$underover <- sf::st_filter(uo$underover, dplyr::distinct(maindata))
    }

    # get the type of this underover
    if (all(sf::st_is(uo$underover, c("POLYGON", "MULTIPOLYGON")))) {
      uo_datatype <- "areal"
    } else if (all(sf::st_is(uo$underover, c("POINT", "LINESTRING", "MULTIPOINT", "MULTILINESTRING")))) {
      uo_datatype <- "point"
    }

    # are we trying to have meaningful color or fill in two places (here and main plot?)
    # is there a better way to do this than to look for the paletteer value?
    if (maindatatype == uo_datatype &
      grepl("::", uo$pal_list) &
      (uo$pal_list != maincolorpal[[1]])) {
      rlang::warn("Trying to have meaningful fill or color scale with different palettes for underover and main data. Removing from underover")
      uo$pal_list <- NA
    }

    # data and plot prep
    uprep <- plot_data_prep(
      data = uo$underover, outcome_col = uo$outcome_col,
      sceneorder = sceneorder,
      base_list = uo$base_list,
      zero_adjust = uo$zero_adjust,
      onlyzeros = uo$onlyzeros
    )

    uprep <- plot_style_prep(
      prepped = uprep,
      colorset = uo$outcome_col,
      colorgroups = uo$colorgroups,
      pal_list = uo$pal_list,
      pal_direction = uo$pal_direction,
      transoutcome = uo$transoutcome,
      transx = uo$transx,
      point_group = uo$point_group
    )


    if (uo_datatype == "point") {
      # fixed color gets its own call
      if (uprep$color_type == "fixed") {
        outcome_plot <- outcome_plot +
          ggplot2::geom_sf(data = uprep$data, color = uprep$pal_list)
      } else {
        outcome_plot <- outcome_plot +
          ggplot2::geom_sf(
            data = uprep$data,
            ggplot2::aes(color = .data$color)
          )
        outcome_plot <- handle_palettes(outcome_plot,
          aes_type = "color",
          pal_list = uprep$pal_list,
          color_type = uprep$color_type,
          transoutcome = transoutcome,
          setLimits = setLimits,
          pal_direction = uprep$direction,
          base_list = base_list
        )

        outcome_plot <- outcome_plot +
          ggplot2::labs(color = paste0(uo$outcome_lab, uprep$ylab_append))
      }
    }
    if (uo_datatype == "areal") {
      if (uprep$color_type == "fixed") {
        outcome_plot <- outcome_plot +
          ggplot2::geom_sf(data = uprep$data, fill = uprep$pal_list,
                           color = uo$map_outlinecolor)
      } else {
        outcome_plot <- outcome_plot +
          ggplot2::geom_sf(
            data = uprep$data,
            ggplot2::aes(fill = .data$color),
            color = uo$map_outlinecolor
          )
        outcome_plot <- handle_palettes(outcome_plot,
          aes_type = "fill",
          pal_list = uprep$pal_list,
          color_type = uprep$color_type,
          transoutcome = transoutcome,
          setLimits = setLimits,
          pal_direction = uprep$direction,
          base_list = base_list
        )

        outcome_plot <- outcome_plot +
          ggplot2::labs(fill = paste0(uo$outcome_lab, uprep$ylab_append))
      }
    }
  }

  return(outcome_plot)
}

#' Make a heatmap
#'
#' @inheritParams plot_outcomes
#'
#' @param prepped prepared data from `plot_data_prep() |>  plot_style_prep()`
#' @param xtype character, 'qual', 'numeric' or 'date' (which
#'   covers several different date types). Determines how axes are handled
#' @param ytype character, 'qual', 'numeric' or 'date' (which
#'   covers several different date types). Determines how axes are handled
#'
#' @return
#' @export
#'
#' @examples
plot_heatmap <- function(prepped,
                         x_col, x_lab,
                         y_col, y_lab,
                         outcome_lab,
                         transoutcome, transx, transy,
                         contour_arglist,
                         xtype, ytype,
                         setLimits, base_list) {
  # build the base plot with x and y (z/fill depend on whether tiles or contours)
  outcome_plot <- prepped$data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]]
    ))

  # Only do the trans if the axes are numeric
  if (!ytype %in% c("date", "qual")) {
    outcome_plot <- outcome_plot +
      ggplot2::scale_y_continuous(trans = transy)
  }

  if (!(xtype %in% c("date", "qual"))) {
    outcome_plot <- outcome_plot +
      ggplot2::scale_x_continuous(trans = transx)
  }

  # tiled heatmap if no contours
  if (is.null(contour_arglist) | (!is.null(contour_arglist$interpolate) && contour_arglist$interpolate)) {
    heat_aes <- "fill"

    if (!is.null(contour_arglist$interpolate) && contour_arglist$interpolate) {
      outcome_plot <- outcome_plot +
        ggplot2::geom_raster(mapping = ggplot2::aes(fill = .data[[prepped$outcome_col]]), interpolate = TRUE)
    } else {
      outcome_plot <- outcome_plot +
        ggplot2::geom_tile(mapping = ggplot2::aes(fill = .data[[prepped$outcome_col]]))
    }
  }
  # if there are contour args, make contours
  if (!is.null(contour_arglist) & is.null(contour_arglist$interpolate)) {
    heat_aes <- "contour"
    default_contour_arglist <- list(
      bins = NULL,
      binwidth = NULL,
      breaks = NULL,
      lineend = "butt",
      linejoin = "round",
      linemitre = 10,
      na.rm = FALSE
    )

    contour_arglist <- modifyList(default_contour_arglist, contour_arglist)

    # We need to do the trans to the data itself for contours because of the way they're binned.
    if (!rlang::is_function(get(transoutcome))) {
      rlang::abort(glue::glue("`transoutcome` ({transoutcome}) is not a named function and so cannot be applied to this data.
                              If it is an acceptable trans for ggplot/scales, it should work for anything other than contour plots.\n"))
    }
    outcome_plot <- outcome_plot +
      ggplot2::geom_contour_filled(
        mapping = ggplot2::aes(z = rlang::exec(
          get(transoutcome),
          .data[[prepped$outcome_col]]
        )),
        bins = contour_arglist$bins,
        binwidth = contour_arglist$binwidth,
        breaks = contour_arglist$breaks,
        lineend = contour_arglist$lineend,
        linejoin = contour_arglist$linejoin,
        linemitre = contour_arglist$linemitre,
        na.rm = contour_arglist$na.rm
      )
  }

  # do I need to do this for fill as well when I have smooth?
  # get the number of bins (10 is default, otherwise, get the number of bins or breaks)
  if (is.null(contour_arglist)) {
    nbins <- 10
  } else {
    if (is.null(contour_arglist$breaks)) {
      if (is.null(contour_arglist$bins)) {
        # I think this is the default- it's the intervals
        nbins <- length(pretty(prepped$data[[prepped$outcome_col]], 10)) - 1
      } else {
        nbins <- contour_arglist$bins
      }
    } else if (is.numeric(contour_arglist$breaks)) {
      nbins <- length(contour_arglist$breaks)
    } else if (is.function(contour_arglist$breaks)) {
      rlang::inform("breaks for contours is a function, cannot infer number bins, ignoring palette")
      nbins <- NULL
    }
  }

  outcome_plot <- handle_palettes(outcome_plot,
    aes_type = heat_aes,
    pal_list = prepped$pal_list,
    color_type = prepped$color_type,
    transoutcome = transoutcome,
    setLimits = setLimits,
    pal_direction = prepped$direction,
    base_list = base_list,
    nbins = nbins
  )
  return(outcome_plot)
}
