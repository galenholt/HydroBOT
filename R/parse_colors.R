find_color_type <- function(pal_list) {
  # Four options:

  # 1. pal_list is a named `colors` object
    # return an indication to use scale_*manual(values = pal_list)
  # 2. pal_list is a paletteer palette name
    # return an indication to use scale_paletteer_*(pal_list)
  # 3. pal_list is a list, and we need to deal with grouped colors
    # find the colors to use, return those in the data along with an indication to use scale_*identity
  # 4. pal_list is a single color value

  # I don't want to deal with automating other things like making the colors
  # object from data and pal_list; I don't think it's necessary and is fraught.



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

  if (length(pal_list) > 1) {
    return('grouped')
  }

  if (length(pal_list) == 1 &&
      pal_list %in% grDevices::colors() |
      grepl("#", pal_list) |
      is.na(pal_list) |
      is.null(pal_list)) {
    return('fixed')
  }
}

handle_palettes <- function(ggobj, aes_type, pal_list, color_type,
                            transy = 'identity', setLimits = NULL,
                            base_list = NULL) {

  if (color_type == 'colorobj') {
    if (aes_type == 'fill') {
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
                                          trans = transy,
                                          limit = \(x) findlimits(x,
                                                                  lims = setLimits,
                                                                  base_list = base_list))
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        paletteer::scale_color_paletteer_c(palette = pal_list[[1]],
                                           trans = transy,
                                           limit = \(x) findlimits(x,
                                                                   lims = setLimits,
                                                                   base_list = base_list))
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

test_overplotting <- function(data, facet_wrapper, facet_row, facet_col) {

  # group by geometry if an sf- allows use for heatmaps
  if (inherits(data, 'sf')) {
    data <- data |>
      dplyr::group_by(geometry)
  }


  if (!is.null(facet_wrapper)) {
    data <- data |>
      dplyr::group_by(.data[[facet_wrapper]], .add = TRUE)
  }
  if (!is.null(facet_row) && facet_row != '.') {
    data <- data |>
      dplyr::group_by(.data[[facet_row]], .add = TRUE)
  }
  if (!is.null(facet_col) && facet_col != '.') {
    data <- data |>
      dplyr::group_by(.data[[facet_col]], .add = TRUE)
  }

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

# Find limits for the color scale- needs to be a function, so write it here so it inherits variables.
# allows centering diverging palettes with baseline comparisons
# by defining this in here, it inherits base_list$comp_fun and other values
# x is typically prepped$data[[prepped$y_col]] (because `prepped$y_col` is
# the new y_col name if baselined). Should I just use that? Or can I call it from inside the `scale`
# x here is a length-2 default set of limits.
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
