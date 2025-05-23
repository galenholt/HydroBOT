#' Helper to find the type of color specification, so we can call the right `ggplot::scale_color_*`
#'
#' Four options:
#' 1. pal_list is a named `colors` object
#' return an indication to use scale_*manual(values = pal_list)
#' 2. pal_list is a paletteer palette name
#' return an indication to use scale_paletteer_*(pal_list)
#' 3. pal_list is a list, and we need to deal with grouped colors
#' find the colors to use, return those in the data along with an indication to use scale_*identity
#' 4. pal_list is a single color value
#' Does not automate other things like making the colors
#' object from data and pal_list; not necessary and is fraught.
#' @param pal_list as in [plot_outcomes()]
#'
#' @return character, one of 'colorobj', 'paletteer_c', 'paletteer_d', 'grouped', 'fixed' that help other functions know what to do wiht color/fill
#' @keywords internal
find_color_type <- function(pal_list) {

  # if it's a color object
  if (inherits(pal_list, 'colors')) {
    return('colorobj')
  }

  # Two options need the paletteer names
  cnames <- paletteer::palettes_c_names |>
    dplyr::mutate(formatted = stringr::str_c(.data$package, .data$palette, sep = '::')) |>
    dplyr::select("formatted") |> dplyr::pull()

  dnames <- paletteer::palettes_d_names |>
    dplyr::mutate(formatted = stringr::str_c(.data$package, .data$palette, sep = '::')) |>
    dplyr::select("formatted") |> dplyr::pull()

  if (length(pal_list) == 1 & pal_list[[1]] %in% cnames) {
    return('paletteer_c')
  }

  if (length(pal_list) == 1 & pal_list[[1]] %in% dnames) {
    return('paletteer_d')
  }

  # grouped colors (e.g. a list of palettes)
  # End up getting explicitly assigned in plot_style_prep and scale_*_identity()
  if (length(pal_list) > 1) {
    return('grouped')
  }

  # just a single color
  if (length(pal_list) == 1 &&
      pal_list %in% grDevices::colors() |
      grepl("#", pal_list) |
      is.na(pal_list) |
      is.null(pal_list)) {
    return('fixed')
  }

}

#' Makes the calls to `scale_fill_*` and `scale_color_*`, depending on whether color/fill and the palette type needed
#'
#' Uses `aes_type` to know the color/fill, and `color_type` (from [find_color_type()]) to know what ggplot function to use to add the color. Needs to handle any color trans as well with `transoutcome`
#'
#' @inheritParams plot_outcomes
#'
#' @param ggobj The in-construction ggplot object
#' @param aes_type 'color' or 'fill'
#' @param color_type from [find_color_type()]
#' @param nbins number of bins for contours
#'
#' @return modified ggplot object
#' @export
#'

handle_palettes <- function(ggobj, aes_type, pal_list, color_type,
                            transoutcome = 'identity', setLimits = NULL,
                            pal_direction = rep(1, length(pal_list)),
                            base_list = NULL,
                            nbins = 10) {

  # If nbins is NULL and this is a contour, we cannot infer (currently), so just return
  if (is.null(nbins) & aes_type == 'contour') {
    return(ggobj)
  }

  if (color_type == 'colorobj') {
    if (aes_type == 'fill' | aes_type == 'contour') {
      ggobj <- ggobj +
        ggplot2::scale_fill_manual(values = pal_list)
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        ggplot2::scale_color_manual(values = pal_list)
    }

  }


  if (color_type == 'paletteer_c') {

    # If the above causes issues, it should also work to have \(x) find_limits(x, lims = setLimits, trans = 'identity', base_list = base_list)
    if (aes_type == 'fill') {
      ggobj <- ggobj +
        paletteer::scale_fill_paletteer_c(palette = pal_list[[1]],
                                          trans = transoutcome,
                                          limit = \(x) find_limits(x, setLimits, 'identity', base_list),
                                          direction = pal_direction)
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        paletteer::scale_color_paletteer_c(palette = pal_list[[1]],
                                           trans = transoutcome,
                                           limit = \(x) find_limits(x, setLimits, 'identity', base_list),
                                           direction = pal_direction)
    }
    if (aes_type == 'contour') {
      # contours are actually bins, despite usually being drawn from a continuous palette.
      binpal <- paletteer::paletteer_c(palette = pal_list[[1]], n = nbins,
                                       direction = pal_direction)
      ggobj <- ggobj +
        ggplot2::scale_fill_manual(values = binpal)
    }

  }

  if (color_type == 'paletteer_d') {
    if (aes_type == 'fill') {
      ggobj <- ggobj +
        paletteer::scale_fill_paletteer_d(palette = pal_list[[1]],
                                          direction = pal_direction)
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        paletteer::scale_color_paletteer_d(palette = pal_list[[1]],
                                           direction = pal_direction)
    }
    if (aes_type == 'contour') {
      # contours are actually bins, despite usually being drawn from palettes.
      binpal <- paletteer::paletteer_d(palette = pal_list[[1]], n = nbins,
                                       direction = pal_direction)
      ggobj <- ggobj +
        ggplot2::scale_fill_manual(values = binpal)
    }

  }

  if (color_type == 'grouped') {

    # finds labels that match scale_color_identity
    labfind <- function(breaks, data) {
      if (inherits(data, 'sf')) {data <- sf::st_drop_geometry(data)}
      matched_vals <- unique(data[,c('colordef', 'color')])
      # make sure everything lines up
      m_ind <- match(breaks, matched_vals$color)
      return(dplyr::pull(matched_vals[, 'colordef', drop = FALSE])[m_ind])
    }

    if (aes_type == 'fill') {
      ggobj <- ggobj +
        ggplot2::scale_fill_identity(guide = 'legend',
                                     labels = \(x) labfind(x, ggobj$data))
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        ggplot2::scale_color_identity(guide = 'legend',
                                     labels = \(x) labfind(x, ggobj$data))
    }
  }

  if (color_type == 'fixed') {

    # No guides for fixed colors
    if (aes_type == 'fill') {
      ggobj <- ggobj +
        ggplot2::scale_fill_identity()
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        ggplot2::scale_color_identity()
    }
  }

  return(ggobj)
}

#' Check silent overplotting
#'
#' Fill in maps and heatmaps can silently plot multiple values on top of each
#' other. THis checks data dimension to ensure only a single value is plotted
#'
#' @inheritParams plot_outcomes
#'
#' @param data the prepped data to be plotted
#'
#' @return TRUE if there's no overplotting, otherwise aborts
#' @keywords internal
#'

test_overplotting <- function(data, facet_wrapper, facet_row, facet_col, x_col = NULL, y_col = NULL) {

  # Get the geometry column if there is one (will be NULL otherwise)
  geom_col_name <- attr(data, "sf_column")

  groupcols <- c(geom_col_name, facet_wrapper, facet_row, facet_col, x_col, y_col)

  # NULLs above get dropped, but facet_col and facet_row can be '.' to indicate
  # nothing on that dimension. Drop that.
  groupcols <- groupcols[groupcols != '.']

  data <- data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(groupcols)))

  data <- data |>
    dplyr::summarise(nrows = dplyr::n()) |>
    dplyr::ungroup()

  if (!all(data$nrows == 1)) {
    rlang::abort(glue::glue("Trying to plot multiple values
                   (up to {max(data$nrows)}) in single polygons.
                   Something is duplicated-
                   do you need more facetting or filtering?"))
  }

  return(invisible(TRUE))
}




#' Finds limits for y-axis and colors
#'
#' @param limcol vector of the y-values, typically `sf::st_drop_geometry(prepped$data[prepped$outcome_col])`
#' @param lims desired limits. see `setLimits` in [plot_outcomes()]
#' @param base_list as in [plot_outcomes()]
#' @param trans transform if any
#'
#' @return length-2 limits (or NULL)
#' @keywords internal
find_limits <- function(limcol, lims, trans, base_list) {
  # If null, keep it that way
  if (is.null(lims)) {
    if (is.null(base_list)) {
      # in a plot function, limcol will be the default limits. for y, NULL gives the defaults and limcol is a vector
      if (length(limcol) == 2) {
        new_lims <- limcol
      } else {
        new_lims <- NULL
      }
    }
    if (!is.null(base_list)) {
      if (base_list$comp_fun == 'difference') {
        # lims <- 0 # not new_lims yet, let the later conditionals handle these.
        lims <- max(abs(limcol), na.rm = TRUE) * c(-1,1)
      }
      if (base_list$comp_fun == 'relative') {
        # lims <- 1
        logvals <- log(limcol)
        loglims <- max(abs(logvals), na.rm = TRUE) * c(-1,1)
        lims <- exp(loglims)
      }
      if (!base_list$comp_fun %in% c('difference', 'relative')) {
        if (length(limcol) == 2) {
          new_lims <- limcol
        } else {
          new_lims <- NULL
        }
      }
    }
  }

  # if they are just the lims, leave it
  if (length(lims) == 2) {
    new_lims <- lims
  }

  # if there is 1 or 3, deal with centering which will need to know about trans
  if (!is.null(lims) & length(lims) %in% c(1, 3)) {

    if (length(lims) == 1) {
      if (!rlang::is_function(get(trans))) {
        rlang::abort(glue::glue("`transoutcome` ({trans}) is not a named function and so cannot be applied to find y-scales.
                              If it is an acceptable trans for ggplot/scales, it should be fine, but you will need to set your setLimits more simply (using NULL, or a length-2 vector).\n"))
      }
      transdat <- rlang::exec(
        get(trans),
        limcol)
    }

    minval <- ifelse(length(lims) == 1, min(transdat, na.rm = TRUE), lims[1])
    maxval <- ifelse(length(lims) == 1, max(transdat, na.rm = TRUE), lims[3])
    mid <- ifelse(length(lims) == 1, lims, lims[2])

    middown <- abs(mid-minval)
    middup <- abs(maxval - mid)
    distfrommid <- max(middown, middup)

    if (middown != middup) {
      rlang::inform(glue::glue("Limits {lims} not symmetrical about the midpoint.
                               ignoring the one with the smallest difference.
                               Making them {distfrommid} up and down from the midpoint {mid}."))
    }


    lower <- mid - distfrommid
    upper <- mid + distfrommid
    new_lims <- c(lower, upper)
  }

  return(new_lims)
}
