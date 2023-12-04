plot_bar <- function(prepped, x_col, x_lab, y_lab,
                     position, transy) {

    outcome_plot <- prepped$data |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[x_col]],
                                   y = .data[[prepped$y_col]],
                                   fill = .data$color)) +
      ggplot2::geom_col(position = position) +
      ggplot2::scale_y_continuous(trans = transy) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis())


    outcome_plot <- handle_palettes(outcome_plot, aes_type = 'fill',
                                    prepped$pal_list, prepped$color_type)

    return(outcome_plot)

}


plot_numeric <- function(prepped, x_col, x_lab, y_lab,
                         position, transy, transx,
                         smooth_arglist) {

  if (!inherits(position, 'gg')) {
    if (is.null(position) || position == 'stack') {
      position = 'identity'
    }
  }


  outcome_plot <- prepped$data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_col]],
                                 y = .data[[prepped$y_col]],
                                 color = .data$color,

                                 group = .data$pointgroup)) +
    ggplot2::geom_point(position = position) +
    ggplot2::scale_y_continuous(trans = transy) +
    ggplot2::scale_x_continuous(trans = transx)

  # THESE CONDITIONALS ARE A MESS. MAYBE JUST is.logical, is.list, is.null...

  # There must be a slick way to do this, but the rlang::exec and do.call
  # methods don't let use keep using the defaults for other args, as far as I
  # can tell.
  default_smooth <- list(method = NULL,
                         se = TRUE,
                         method.args = NULL,
                         linewidth = NULL,# Odd that I have to set this.
                         alpha = 0.25)


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
      ggplot2::geom_smooth(mapping = ggplot2::aes(fill = .data$color),
                           method = smooth_arglist$method,
                           method.args = smooth_arglist$method.args,
                           se = smooth_arglist$se,
                           linewidth = smooth_arglist$linewidth,
                           alpha = smooth_arglist$alpha)

  }


  # do I need to do this for fill as well when I have smooth?
  outcome_plot <- handle_palettes(outcome_plot, aes_type = 'color',
                                  prepped$pal_list, prepped$color_type)
  outcome_plot <- handle_palettes(outcome_plot, aes_type = 'fill',
                                  prepped$pal_list, prepped$color_type)

}

plot_map <- function(prepped, underlay_list, overlay_list,
                     facet_wrapper, facet_row, facet_col,
                     sceneorder, transy, setLimits, base_list) {

  # this is just a check- it either completes silently or aborts with a message
  test_overplotting(data = prepped$data, facet_wrapper, facet_row, facet_col)

  # we need to do different things depending on whether the main data has area
  # (polygon/multipolygon) or not (points, linestring, multi-point, etc) build
  # the plot piece by piece
  if (all(sf::st_is(prepped$data, c("POLYGON", "MULTIPOLYGON")))) {
      maindatatype = 'areal'
  } else if (all(sf::st_is(prepped$data, c("POINT", "LINESTRING", "MULTIPOINT")))){
      maindatatype = 'point'
  }

  # Clean up the under and overlay lists
  # Underlays, if needed
  if (!is.null(underlay_list)) {
    # Handle multiple levels and a flat list or a list of lists-
    # Allow a short-circuit passing character, e.g. 'basin'
    # sets the color default to NA because this will usually be fill
    if (is.character(underlay_list)) {underlay_list <- list(underlay = get(underlay_list), underlay_pal = NA)}
    # if flat put it as the first list in a length-one list for generality
    if ('underlay' %in% names(underlay_list)) {underlay_list <- list(underlay_list)}
  }

  if (!is.null(overlay_list)) {
    # Handle multiple levels and a flat list or a list of lists-
    # Allow a short-circuit passing character, e.g. 'bom_basin_gauges'
    # Setting the color default to black because overlay shouldn't be fill and so will be a polygon outline or points
    if (is.character(overlay_list)) {overlay_list <- list(overlay = get(overlay_list), overlay_pal = 'black')}
    # if flat put it as the first list in a length-one list for generality
    if ('overlay' %in% names(overlay_list)) {overlay_list <- list(overlay_list)}
  }


  # Initialise the plot
  outcome_plot <- ggplot2::ggplot()

    # layer up the underlays
    outcome_plot <- make_underover(underlay_list, outcome_plot, sceneorder,
                                  maindatatype, maincolorpal = prepped$pal_list,
                                  transy = transy, setLimits = setLimits,
                                  base_list = base_list, uotype = 'underlay')

  # Make the 'main' level. This is sort of silly in some ways, since we could
  # just have one list with the under, main, and over, but giving a level
  # primacy can be important for determining things like who wins color conflicts
  if (maindatatype == 'point') {
    outcome_plot <- outcome_plot +
      ggplot2::geom_sf(data = prepped$data,
                      ggplot2::aes(color = .data$color))
    outcome_plot <- handle_palettes(outcome_plot, aes_type = 'color',
                                    pal_list = prepped$pal_list,
                                    color_type = prepped$color_type,
                                    transy = transy,
                                    setLimits = setLimits,
                                    base_list = base_list)
  }
  if (maindatatype == 'areal') {
    outcome_plot <- outcome_plot +
      ggplot2::geom_sf(data = prepped$data,
                      ggplot2::aes(fill = .data$color))
    outcome_plot <- handle_palettes(outcome_plot, aes_type = 'fill',
                                    pal_list = prepped$pal_list,
                                    color_type = prepped$color_type,
                                    transy = transy,
                                    setLimits = setLimits,
                                    base_list = base_list)
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
      ggplot2::facet_grid(stats::reformulate(facet_col,facet_row))
  }


    # layer up the overlays
    outcome_plot <- make_underover(overlay_list, outcome_plot, sceneorder,
                                   maindatatype, maincolorpal = prepped$pal_list,
                                   transy = transy, setLimits = setLimits,
                                   base_list = base_list, uotype = 'overlay')

    return(outcome_plot)

}


make_underover <- function(underover_list, outcome_plot, sceneorder,
                          maindatatype, maincolorpal, transy,
                          setLimits, base_list, uotype = 'internal') {

  # A full list to allow partial specification with null-filling
  full_list <- list(underover = NULL, y_col = NULL,
                    pal_list = NA, # NA will show something, NULL just fails
                    colorgroups = NULL,
                    base_list = NULL, zero_adjust = 0,
                    onlyzeros = FALSE,
                    transy = transy,
                    transx = NULL,
                    point_group = NULL,
                    uotype = NULL)

  # To make this function generic, it can have the main piece named 'underover', 'underlay', or 'overlay'
  # but we need some errorchecking

  # We don't currently *use* the uotype entry in the underover_list, but this is
  # here to provide first backwards compatibility, but also set us up with the
  # backbone to just do all the layers in one go.

  settype <- function(x, typename) {
    if (typename %in% names(x)) {
      names(x)[names(x) == typename] <- 'underover'
    }

    names(x)[names(x) %in% c('underlay_pal', 'overlay_pal')] <- 'pal_list'
    names(x)[names(x) %in% c('underlay_ycol', 'overlay_ycol')] <- 'y_col'

    x$uotype <- typename
    return(x)
  }

  if (uotype == 'underlay') {
    underover_list <- purrr::map(underover_list, \(x) settype(x, typename = 'underlay'))
  }

  if (uotype == 'overlay') {
    underover_list <- purrr::map(underover_list, \(x) settype(x, typename = 'overlay'))
  }

  if (uotype == 'internal') {
    typeexists <- all(purrr::map_lgl(underover_list, \(x) 'uotype' %in% names(x)))
    if (!typeexists) {
      rlang::abort("asking for underlay or overlay, but have not specified a uotype. Do this either with separate underlay_list and overlay_list or a single list with a specified outype item")
    }
  }


  # there might be several layers, so loop
  for (uo in underover_list) {
    # Allow passing names
    if (is.character(uo$underover)) {uo$underover <- get(uo$underover)}

    # NULL-fill slots without arguments
    uo <- modifyList(full_list, uo)

    # get the type of this underover
    if (all(sf::st_is(uo$underover, c("POLYGON", "MULTIPOLYGON")))) {
      uo_datatype = 'areal'
    } else if (all(sf::st_is(uo$underover, c("POINT", "LINESTRING", "MULTIPOINT")))){
      uo_datatype = 'point'
    }

    # are we trying to have meaningful color or fill in two places (here and main plot?)
    # is there a better way to do this than to look for the paletteer value?
    if (maindatatype == uo_datatype &
        grepl("::", uo$pal_list) &
        (uo$pal_list != maincolorpal[[1]])) {
      rlang::warn("Trying to have meaningful fill or color scale with different palettes for underover and main data. Removing from underover")
      uo$pal_list = NA
    }

    # data and plot prep
    uprep <- plot_data_prep(data = uo$underover, y_col = uo$y_col,
                            sceneorder = sceneorder,
                            base_list = uo$base_list,
                            zero_adjust = uo$zero_adjust,
                            onlyzeros = uo$onlyzeros)

    uprep <- plot_style_prep(prepped = uprep,
                               colorset = uo$y_col,
                             colorgroups = uo$colorgroups,
                             pal_list = uo$pal_list,
                               transy = uo$transy,
                             transx = uo$transx,
                             point_group = uo$point_group)


    if (uo_datatype == 'point') {
      # fixed color gets its own call
      if (uprep$color_type == 'fixed') {
        outcome_plot <- outcome_plot +
          ggplot2::geom_sf(data = uprep$data, color = uprep$pal_list)
      } else {
        outcome_plot <- outcome_plot +
          ggplot2::geom_sf(data = uprep$data,
                           ggplot2::aes(color = .data$color))
        outcome_plot <- handle_palettes(outcome_plot, aes_type = 'color',
                                        pal_list = uprep$pal_list,
                                        color_type = uprep$color_type,
                                        transy = transy,
                                        setLimits = setLimits,
                                        base_list = base_list)
      }


    }
    if (uo_datatype == 'areal') {
      if (uprep$color_type == 'fixed') {
        outcome_plot <- outcome_plot +
          ggplot2::geom_sf(data = uprep$data, fill = uprep$pal_list)
      } else {
      outcome_plot <- outcome_plot +
        ggplot2::geom_sf(data = uprep$data,
                        ggplot2::aes(fill = .data$color))
      outcome_plot <- handle_palettes(outcome_plot, aes_type = 'fill',
                                      pal_list = uprep$pal_list,
                                      color_type = uprep$color_type,
                                      transy = transy,
                                      setLimits = setLimits,
                                      base_list = base_list)
      }
    }



  }

  return(outcome_plot)

  }


