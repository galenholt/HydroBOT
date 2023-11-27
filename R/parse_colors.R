find_color_type <- function(pal_list) {
  # Three options:

  # 1. pal_list is a named `colors` object
    # return an indication to use scale_*manual(values = pal_list)
  # 2. pal_list is a paletteer palette name
    # return an indication to use scale_paletteer_*(pal_list)
  # 3. pal_list is a list, and we need to deal with grouped colors
    # find the colors to use, return those in the data along with an indication to use scale_*identity

  # I don't want to deal with automating other things like making the colors
  # object from data and pal_list; I don't think it's necessary and is fraught.



  if (inherits(pal_list, 'colors')) {
    return('colorobj')
  }

  # The other two options need the paletteer names
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
}

handle_palettes <- function(ggobj, aes_type, pal_list, color_type) {

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
        paletteer::scale_fill_paletteer_c(palette = pal_list[[1]])
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        paletteer::scale_color_paletteer_c(palette = pal_list[[1]])
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
    if (aes_type == 'fill') {
      ggobj <- ggobj +
        ggplot2::scale_fill_identity(guide = 'legend',
                                     labels = labfind,
                                     name = color_lab)
    }
    if (aes_type == 'color') {
      ggobj <- ggobj +
        ggplot2::scale_color_identity(guide = 'legend',
                                     labels = labfind,
                                     name = color_lab)
    }
  }

  return(ggobj)
}
