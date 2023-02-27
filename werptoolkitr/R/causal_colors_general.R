#' Set colours of nodes or edges
#'
#' Given a set of palettes and a column of data to use to determine colour (and
#' possibly a grouping column), this assigns colour to each node. Can also be
#' short-circuited by passing a scalar color or column of colour names. Edge
#' tibbles get a `color` column, while nodes get `fillcolor`, matching
#' `DiagrammeR` attribute names. The `fontcolor` attribute is also set for nodes
#' as black or white depending on the `fillcolor`
#'
#' @param df a tibble or dataframe, nodes or edges
#' @param pal_list a named list of paletteer palettes or a scalar character specifying colour.
#'   * named list: names should match the values in the `colorgroups` column, which determine which palettes apply to which rows. If `colorgroups = NULL`, the name doesn't matter, but often clearest to match `colorset`
#'   * character (scalar or vector): short-circuits the palette finding and creates a color column with the given colors.
#' @param colorgroups NULL (the default) or length-1 character vector specifying a grouping column, with different palettes applied to the different groups. If NULL, one palette is applied across all of the rows
#' @param colorset a length-1 character vector specifying the column to use to define colour. Type doesn't matter, but behaviour differs. If numeric it will space the colours for each row according to value, if not numeric, colours for each row are spaced evenly along the palette.
#'
#' @return an edge tibble with a `color` column or a node tibble with `fillcolor` and `fontcolor`
#' @export
#'
#' @examples
#'
causal_colors_general <- function(df, pal_list,
                                  colorgroups = NULL,
                                  colorset = NULL) {

  # Auto-create some colordefs in specific ways for causal networks
  df <- make_colorcol(df, colorset)
  # and change the colorset to 'colordef' since we just auto-set

  # Then call the general `grouped_colours`
  dfcolor <- grouped_colours(df, pal_list, colorgroups, 'colordef')

  # names have to be different for nodes and edges. and nodes need text color
  # This is annoying we need the switch, and I don't like making it depend on column names
  if ('NodeType' %in% names(df)) {
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
  if ('NodeType' %in% names(df)) {
    namecol <- 'Name'
    typecol <- 'NodeType'
  } else if ('from' %in% names(df)) {
    namecol <- 'from'
    typecol <- 'fromtype'
  }

  # deal with some options for the color column (NULL, passed defaults, or the argument)
  if (is.null(colorset)) {
    dfgroup <- df %>%
      dplyr::mutate(colordef = .data[[namecol]])
  } else if (colorset == 'werp') {
    # Set up a default set of groupings for werp
    dfgroup <- df %>%
      dplyr::mutate(colordef = case_when(.data[[typecol]] == 'ewr_code' ~ stringr::str_extract(.data[[namecol]], '^[A-Z]+'),
                                  .data[[typecol]] == 'env_obj' ~ stringr::str_extract(.data[[namecol]], '^[A-Z]+'),
                                  .data[[typecol]] == 'Specific_goal' ~ .data[[namecol]],
                                  .data[[typecol]] == 'Target' ~ .data[[namecol]],
                                  stringr::str_detect(.data[[typecol]], 'target_') ~ stringr::str_extract(.data[[typecol]], '[0-9]_year')))

  } else {
    # This is silly to dplyr::rename the col, but safer
    dfgroup <- df %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(colorset), identity, .names = 'colordef'))
  }

  return(dfgroup)
}

# Find the font color as white or black, dependent on fillcolor
fontcol <- function(boxcol) {
  # replace failures with white
  boxcol[boxcol == 'character(0)'] <- '#FFFFFFFF'
    # colorspace defines rgb differently to base. this was
    # colorspace::hex2RGB(boxcol), but that didn't handle named colors.
  r <- colorspace::sRGB(t(col2rgb(boxcol))/255)
  pl <- as(r, 'polarLUV')
  l <- colorspace::coords(pl)[,1]
  fc <- ifelse(l > 50, 'black', 'white')
}


# Get the proportional position in the range
propor <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

# If nodes, change name and add a fontcolor
# annoying that Diagrammer uses different color attribute names for nodes and edges
tweak_node_color <- function(df) {
    df <- df %>%
      dplyr::rename(fillcolor = color) %>%
      # create the fontcolor
      dplyr::mutate(fontcolor = fontcol(fillcolor))
}
