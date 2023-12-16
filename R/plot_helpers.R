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
#'
find_color_type <- function(pal_list) {

  # if it's a color object
  if (inherits(pal_list, 'colors')) {
    return('colorobj')
  }

  # Two options need the paletteer names
  cnames <- paletteer::palettes_c_names |>
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = '::')) |>
    dplyr::select(formatted) |> dplyr::pull()

  dnames <- paletteer::palettes_d_names |>
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = '::')) |>
    dplyr::select(formatted) |> dplyr::pull()

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
#' @return
#' @export
#'
#' @examples
handle_palettes <- function(ggobj, aes_type, pal_list, color_type,
                            transoutcome = 'identity', setLimits = NULL,
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
    if (aes_type == 'fill') {
      ggobj <- ggobj +
        paletteer::scale_fill_paletteer_c(palette = pal_list[[1]],
                                          trans = transoutcome,
                                          limit = \(x) findlimits(x,
                                                                  lims = setLimits,
                                                                  base_list = base_list))
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        paletteer::scale_color_paletteer_c(palette = pal_list[[1]],
                                           trans = transoutcome,
                                           limit = \(x) findlimits(x,
                                                                   lims = setLimits,
                                                                   base_list = base_list))
    }
    if (aes_type == 'contour') {
      # contours are actually bins, despite usually being drawn from a continuous palette.
      binpal <- paletteer::paletteer_c(palette = pal_list[[1]], n = nbins)
      ggobj <- ggobj +
        ggplot2::scale_fill_manual(values = binpal)
    }

  }

  if (color_type == 'paletteer_d') {
    if (aes_type == 'fill') {
      ggobj <- ggobj +
        paletteer::scale_fill_paletteer_d(palette = pal_list[[1]])
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        paletteer::scale_color_paletteer_d(palette = pal_list[[1]])
    }
    if (aes_type == 'contour') {
      # contours are actually bins, despite usually being drawn from palettes.
      binpal <- paletteer::paletteer_d(palette = pal_list[[1]], n = nbins)
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
      return(dplyr::pull(matched_vals[, 'colordef'])[m_ind])
    }

    if (aes_type == 'fill') {
      ggobj <- ggobj +
        ggplot2::scale_fill_identity(guide = 'legend',
                                     labels = \(x) labfind(x, ggobj$data))
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        ggplot2::scale_color_identity(guide = 'legend',
                                     labels = labfind,
                                     name =  \(x) labfind(x, ggobj$data))
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
#' @return
#' @export
#'
#' @examples
test_overplotting <- function(data, facet_wrapper, facet_row, facet_col, x_col = NULL, y_col = NULL) {

  # Get the geometry column if there is one (will be NULL otherwise)
  geom_col_name <- attr(data, "sf_column")

  # This way is WAY cleaner than below, but there must be a reason I was using
  # the convoluted .data stuff?
  groupcols <- c(geom_col_name, facet_wrapper, facet_row, facet_col, x_col, y_col)

  # NULLs above get dropped, but facet_col and facet_row can be '.' to indicate
  # nothing on that dimension. Drop that.
  groupcols <- groupcols[groupcols != '.']

  data <- data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(groupcols)))


  # This works, but seems unecessary? Keep until tests pass clean.
  # # group by geometry if an sf- allows use for heatmaps
  # if (inherits(data, 'sf')) {
  #   data <- data |>
  #     dplyr::group_by(geometry)
  # }
  #
  # # heatmaps need to group by the x-y pairs instead of geometry
  # if (!inherits(data, 'sf')) {
  #   data <- data |>
  #     dplyr::group_by(.data[[x_col]]) |>
  #     dplyr::group_by(.data[[y_col]], .add = TRUE)
  # }
  #
  #
  # if (!is.null(facet_wrapper)) {
  #   data <- data |>
  #     dplyr::group_by(.data[[facet_wrapper]], .add = TRUE)
  # }
  # if (!is.null(facet_row) && facet_row != '.') {
  #   data <- data |>
  #     dplyr::group_by(.data[[facet_row]], .add = TRUE)
  # }
  # if (!is.null(facet_col) && facet_col != '.') {
  #   data <- data |>
  #     dplyr::group_by(.data[[facet_col]], .add = TRUE)
  # }

  data <- data |>
    dplyr::summarise(nrows = dplyr::n()) |>
    dplyr::ungroup()

  if (!all(data$nrows == 1)) {
    rlang::abort(glue::glue("Trying to plot multiple values
                   (up to {max(data$nrows)}) in single polygons.
                   Something is duplicated-
                   do you need more facetting or filtering?"))
  }
}


#' Finds limits for the color scale, accounting for prepped data and baselining.
#'
#' allows (by crude inference) centering diverging palettes with baseline comparisons
#'
#' @param x the default data range passed in by the palette
#' @param lims desired limits
#' @param base_list as in [plot_outcomes()]
#'
#' @return
#' @export
#'
#' @examples
findlimits <- function(x, lims, base_list) {
  # use hard-set user-supplied limits
  if (!is.null(lims)) {
    return(lims)
  }
  # use default limits
  if (is.null(base_list) || (!base_list$comp_fun %in% c('difference', 'relative')) &
      is.null(lims)) {return(x)}
  # Set limits to make midpoint 0 if difference
  if (!is.null(base_list) && base_list$comp_fun == 'difference') {
    limits <- max(abs(x), na.rm = TRUE) * c(-1, 1)
  }
  # setting midpoint at 1 for multiplicative is trickier
  if (!is.null(base_list) && base_list$comp_fun == 'relative') {
    logvals <- log(x)
    loglims <- max(abs(logvals), na.rm = TRUE) * c(-1, 1)
    limits <- exp(loglims)
  }

  return(limits)

}
