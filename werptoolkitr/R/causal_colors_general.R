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
  # colorgroups is a grouping column defining palettes, so different palettes can be mapped to each group, e.g. color the NF, OS, etc within the env_objs from a different palette than the specific objectives. If it's NULL, the same palette applies to all values in the column defined by colorset
  # colorset is the column defining individual colors. It can be null, a column name (character) or the short-circuit 'werp' to choose the default

  # short-circuit if pal_list is just a color name- accepts single values or a vector of length nrow(df)
  if (!is.list(pal_list) & is.character(pal_list)) {
    df$color <- pal_list

    # names have to be different for nodes and edges. and nodes need text color
    # This is annoying we need the switch, and I don't like making it depend on column names
    if ('NodeType' %in% names(df)) {
      df <- tweak_node_color(df)
    }

    return(df)
  }

  # need some name references to know what function to use
  cnames <- paletteer::palettes_c_names %>%
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = '::')) %>%
    dplyr::select(formatted) %>% dplyr::pull()

  dnames <- paletteer::palettes_d_names %>%
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = '::')) %>%
    dplyr::select(formatted) %>% dplyr::pull()


  # deal with grouping colors.
  dfcolor <- make_colorcol(df, colorset)

  # Make a df without dups and dplyr::left_join
  dfcols <- dfcolor %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(colorgroups))) %>%
    dplyr::distinct(colordef) %>%
    dplyr::mutate(palname = ifelse(is.null(colorgroups),
                            unlist(pal_list),
                            pal_list[[.data[[colorgroups]][1]]]))

  # We get the color value by telling the palette how many colors and then
  # grabbing an index. If colordef is a non-numeric, we should choose as many
  # values as there are unique entries and then just get the indices in order.
  # If colordef is numeric, we want to make a long palette and then grab the
  # color in the right spot.
  if (is.numeric(dfcols$colordef)) {
    dfcols <- dfcols %>%
      dplyr::mutate(pallength = 100,
             palindex = round(propor(colordef)*100))
  } else {
    dfcols <- dfcols %>%
      dplyr::mutate(pallength = dplyr::n(),
             palindex = dplyr::row_number())
  }


  # Need to determine `paletteer_c` vs `paletteer_d` dependent on the name. This
  # should be doable with case_when but it won't run in a dplyr::mutate. Loop over
  # rows, getting the correct color index for the correct palette.
  colmap <- foreach::foreach(i = 1:nrow(dfcols)) %do% {
    thispal <- dfcols$palname[i]
    if (thispal %in% cnames) {
      thiscol <- paletteer::paletteer_c(thispal, n = dfcols$pallength[i])[dfcols$palindex[i]]
    } else if (thispal %in% dnames) {
      thiscol <- paletteer::paletteer_d(thispal, n = dfcols$pallength[i])[dfcols$palindex[i]]
    }
  }


  # column arrangement
  dfcols <- dplyr::bind_cols(dfcols, color = as.character(colmap)) %>%
    dplyr::select(-c(palname, pallength, palindex))


  # names have to be different for nodes and edges. and nodes need text color
  # This is annoying we need the switch, and I don't like making it depend on column names
  if ('NodeType' %in% names(df)) {
    dfcols <- tweak_node_color(dfcols)
  }


  # join back to the main data by the color column. Don't use a by, because the group is set wth {{}}
  suppressMessages(dfcolor <- dplyr::left_join(dfcolor, dfcols))

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
  r <- colorspace::hex2RGB(boxcol)
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
