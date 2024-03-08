#' Sets up colors within groups according to separate palettes
#'
#' @param df input dataframe
#' @param pal_list list of palettes, or color name or color object that maps to `colorgroups`, `colorset`, or `scenario`. Typically a list of names of {palletteer} palettes of the same length as unique values in `colorgroups`
#' @param pal_direction vector of length pal_list, either 1 (default) or -1 (reversed) direction of the palettes
#' @param colorgroups character column name of column defining the groups that define different palettes to use
#' @param colorset  character column name of the values to assign colors from those palettes
#' @param setLimits NULL (default) or length-2 numeric vector to force limits of the color scale.

#' @return a dataframe with new column `color` with hex colors
#' @export
#'
#' @examples
grouped_colors <- function(df, pal_list,
                           pal_direction = rep(1, length(pal_list)),
                           colorgroups = NULL,
                           colorset = NULL,
                           setLimits = NULL) {
  # short-circuit if pal_list is just a color name or a named color object-
  # accepts single values, a named object of class color or a vector of length
  # nrow(df)

  # downstream functions expect a 'colordef' column that allows us to not
  # explicitly track variable names, so add that.
  if (!is.list(pal_list) & is.character(pal_list)) {
    if (inherits(pal_list, "colors")) {
      coltib <- tibble::tibble(name = names(pal_list), color = pal_list)
      # which column does the pal_list match?
      colnames <- names(pal_list)
      if (any(colnames %in% df$scenario)) {
        namematch <- "scenario"
      }
      if (!is.null(colorgroups) && any(colnames %in%
        dplyr::pull(df, colorgroups))) {
        namematch <- colorgroups
      }
      if (!is.null(colorset) && any(colnames %in%
        dplyr::pull(df, colorset))) {
        namematch <- colorset
      }
      names(coltib)[1] <- namematch
      df <- dplyr::left_join(df, coltib, by = namematch) |>
        dplyr::mutate(colordef = .data[[namematch]])
      return(df)
    }
    if (is.character(pal_list) & (length(pal_list) == 1 | length(pal_list == nrow(df)))) {
      # expects a colordef col later, just make one
      df$colordef <- NA
      df$color <- pal_list

      return(df)
    }
  }

  # need some name references to know what function to use
  cnames <- paletteer::palettes_c_names |>
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = "::")) |>
    dplyr::select(formatted) |>
    dplyr::pull()

  dnames <- paletteer::palettes_d_names |>
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = "::")) |>
    dplyr::select(formatted) |>
    dplyr::pull()

  # For consistency, rename the column to a fixed name `colordef`
  df <- df |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(colorset), identity, .names = "colordef"))

  # pal_direction needs to be named as pal_list
  if (!is.null(pal_direction) && is.null(names(pal_direction))) {
    names(pal_direction) <- names(pal_list)
  }

  # Make a df without dups and dplyr::left_join
  dfcols <- df |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(colorgroups))) |>
    dplyr::distinct(colordef) |>
    dplyr::mutate(palname = ifelse(is.null(colorgroups),
      unlist(pal_list),
      pal_list[[.data[[colorgroups]][1]]]
    )) |>
    dplyr::mutate(paldir = ifelse(is.null(colorgroups),
      pal_direction,
      pal_direction[.data[[colorgroups]][1]]
    ))

  # We get the color value by telling the palette how many colors and then
  # grabbing an index. If colordef is a non-numeric, we should choose as many
  # values as there are unique entries and then just get the indices in order.
  # If colordef is numeric, we want to make a long palette and then grab the
  # color in the right spot.
  if (is.numeric(dfcols$colordef)) {
    if (is.null(setLimits)) {
      setLimits <- c(min(dfcols$colordef), max(dfcols$colordef))
    }
    # The funny indexing here is because propor has x-min(x) on top, yielding
    # 0-indexed values, so we need to shift and add.

    # simple case with two limits, or complex with 3 (middle is centerpoint for diverging)
    if (length(setLimits) == 2) {
      dfcols <- dfcols |>
        dplyr::mutate(
          pallength = 1000,
          palindex = round(propor(colordef, minx = setLimits[1], maxx = setLimits[2]) * (pallength - 1)) + 1
        )
    } else if (length(setLimits) == 3) {
      dfcols <- dfcols |>
        dplyr::mutate(
          pallength = 1000,
          palindex = dplyr::case_when(
            colordef == setLimits[2] ~ pallength / 2,
            colordef < setLimits[2] ~ round(propor(colordef,
              minx = setLimits[1],
              maxx = setLimits[2]
            ) * ((pallength / 2) - 1)) + 1,
            colordef > setLimits[2] ~ round(propor(colordef,
              minx = setLimits[2],
              maxx = setLimits[3]
            ) * ((pallength / 2) - 1)) + (pallength / 2) + 1
          )
        )
    }
  } else {
    dfcols <- dfcols |>
      dplyr::mutate(
        pallength = dplyr::n(),
        palindex = dplyr::row_number()
      )
  }


  # Need to determine `paletteer_c` vs `paletteer_d` dependent on the name. This
  # should be doable with case_when but it won't run in a dplyr::mutate. Loop over
  # rows, getting the correct color index for the correct palette.
  # this is *very* close to `make_pal`, and could be combined, but the issue
  # is the rowwise loop. Just need a bit more thinking there
  colmap <- foreach::foreach(i = 1:nrow(dfcols)) %do% {
    thispal <- dfcols$palname[i]
    if (thispal %in% cnames) {
      thiscol <- paletteer::paletteer_c(thispal,
        n = dfcols$pallength[i],
        direction = dfcols$paldir[i]
      )[dfcols$palindex[i]]
    } else if (thispal %in% dnames) {
      thiscol <- paletteer::paletteer_d(thispal,
        n = dfcols$pallength[i],
        direction = dfcols$paldir[i]
      )[dfcols$palindex[i]]
    } else {
      # try to inform a bit- I could auto-set palettes this way, but I'd rather
      # make the user do it right
      paltest <- grepl(thispal, c(cnames, dnames), ignore.case = TRUE)
      if (any(paltest)) {
        rlang::abort(glue::glue("Requested palette not present, likely because wrong case or missing letters.
                   Try `{c(cnames, dnames)[which(paltest)]}`"))
      } else {
        rlang::abort("Requested palette not available in paletteer")
      }
    }
  }


  # column arrangement
  dfcols <- dplyr::bind_cols(dfcols, color = as.character(colmap)) |>
    dplyr::select(-c(palname, pallength, palindex))

  # join back to the main data by the color column. Don't use a by, because the group is set wth {{}}
  suppressMessages(df <- dplyr::left_join(df, dfcols))

  return(df)
}
