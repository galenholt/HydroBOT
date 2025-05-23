#' Set colors of nodes or edges
#'
#' Given a set of palettes and a column of data to use to determine color (and
#' possibly a grouping column), this assigns color to each node. Can also be
#' short-circuited by passing a scalar color or column of color names. Edge
#' tibbles get a `color` column, while nodes get `fillcolor`, matching
#' `DiagrammeR` attribute names. The `fontcolor` attribute is also set for nodes
#' as black or white depending on the `fillcolor`
#'
#' @param df a tibble or dataframe, nodes or edges
#' @param pal_list a named list of paletteer palettes or a scalar character
#'   specifying color.
#'   * named list: names should match the values in the `colorgroups` column, which determine which palettes apply to which rows. If `colorgroups = NULL`, the name doesn't matter, but often clearest to match `colorset`
#'   * character (scalar or vector): short-circuits the palette finding and creates a color column with the given colors.
#' @param pal_direction vector of length pal_list, either 1 (default) or -1
#'   (reversed) direction of the palettes
#' @param colorgroups NULL (the default) or length-1 character vector specifying
#'   a grouping column, with different palettes applied to the different groups.
#'   If NULL, one palette is applied across all of the rows
#' @param colorset a length-1 character vector specifying the column to use to
#'   define color. Type doesn't matter, but behaviour differs. If numeric it
#'   will space the colors for each row according to value, if not numeric,
#'   colors for each row are spaced evenly along the palette.
#' @param setLimits NULL (default) or length-2 numeric vector to force limits of
#'   the color scale.

#' @return an edge tibble with a `color` column or a node tibble with
#'   `fillcolor` and `fontcolor`
#' @export
#'
causal_colors_general <- function(df, pal_list,
                                  pal_direction = rep(1, length(pal_list)),
                                  colorgroups = NULL,
                                  colorset = NULL,
                                  setLimits = NULL) {
  # Auto-create some colordefs in specific ways for causal networks
  df <- make_colorcol(df, colorset)
  # and change the colorset to 'colordef' since we just auto-set

  # Then call the general `grouped_colors`
  dfcolor <- grouped_colors(df,
    pal_list = pal_list,
    pal_direction = pal_direction,
    colorgroups = colorgroups,
    colorset = "colordef",
    setLimits = setLimits
  )

  # names have to be different for nodes and edges. and nodes need text color
  # This is annoying we need the switch, and I don't like making it depend on
  # column names
  if ("NodeType" %in% names(df)) {
    dfcolor <- tweak_node_color(dfcolor)
  }

  return(dfcolor)
}

# helper to make the color column, auto-detects whether edges or nodes
make_colorcol <- function(df, colorset) {
  # These are all dplyr::mutates to make sure we can re-join the outcome
  # This is tricky because has to match pal_list
  # If colorgroup is null, just use names?

  # auto-check whether node or edges
  if ("NodeType" %in% names(df)) {
    namecol <- "Name"
    typecol <- "NodeType"
  } else if ("from" %in% names(df)) {
    namecol <- "from"
    typecol <- "fromtype"
  }

  # deal with some options for the color column (NULL, passed defaults, or the
  # argument)
  if (is.null(colorset)) {
    dfgroup <- df |>
      dplyr::mutate(colordef = .data[[namecol]])
  } else if (colorset == "werp") {
    # Set up a default set of groupings for werp
    dfgroup <- df |>
      dplyr::mutate(colordef = dplyr::case_when(
        .data[[typecol]] == "ewr_code" ~
          stringr::str_extract(.data[[namecol]], "^[A-Z]+"),
        .data[[typecol]] == "env_obj" ~
          stringr::str_extract(.data[[namecol]], "^[A-Z]+"),
        .data[[typecol]] == "Specific_goal" ~ .data[[namecol]],
        .data[[typecol]] == "Target" ~ .data[[namecol]],
        stringr::str_detect(.data[[typecol]], "target_") ~
          stringr::str_extract(.data[[typecol]], "[0-9]_year")
      ))
  } else {
    # This is silly to dplyr::rename the col, but safer
    dfgroup <- df |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(colorset),
                                  identity, .names = "colordef"))
  }

  return(dfgroup)
}

# Find the font color as white or black, dependent on fillcolor
fontcol <- function(boxcol) {
  # replace failures with white
  boxcol[boxcol == "character(0)"] <- "#FFFFFFFF"
  pl <- grDevices::convertColor(t(grDevices::col2rgb(boxcol)) / 255,
                                from = "sRGB", to = "Luv")
  l <- pl[, 1]
  fc <- ifelse(l > 50, "black", "white")
  return(fc)
}


# Get the proportional position in the range
propor <- function(x, minx = min(x), maxx = max(x)) {
  (x - minx) / (maxx - minx)
}

# If nodes, change name and add a fontcolor
# annoying that Diagrammer uses different color attribute names for nodes and
# edges
tweak_node_color <- function(df) {
  df <- df |>
    dplyr::rename(fillcolor = "color") |>
    # create the fontcolor
    dplyr::mutate(fontcolor = fontcol(.data$fillcolor))
}
