#' Iterative aggregation along theme and spatial dimensions
#'
#' Wraps [spatial_aggregate()] and [theme_aggregate()] within a loop over lists
#' of aggregation levels and functions to apply at each level. Includes some
#' small dataprep and cleanup depending on arguments for what the output should
#' look like
#'
#' @inheritParams theme_aggregate
#' @inheritParams spatial_aggregate
#'
#' @param dat input dataframe. Must be sf if `aggsequence` includes any spatial
#'   aggregation. Otherwise, as in [theme_aggregate()] and [spatial_aggregate()]
#' @param causal_edges causal links between all theme levels included in
#'   `aggsequence`, though can also include others, which are ignored. Creates
#'   the theme grouping
#' @param groupers as in [general_aggregate()], with the note that these should
#'   be all grouping columns *except* theme and spatial groupings. These are
#'   both automatically added to `groupers` according to `aggsequence` before
#'   passing to [general_aggregate()].
#' @param group_until named list of groupers and the step to which they should
#'   be retained. Default NA (retain all groupers for all steps). *FOR EWR USE,
#'   best option is* `group_until = list(planning_unit_name = is_notpoint, gauge
#'   = is_notpoint)`. This groups by planning unit and gauge until larger
#'   spatial grouping has happened. Leaving 'gauge' off is mathematically safe,
#'   since the gauge geometry forces that grouping, but then the 'gauge' column
#'   gets dropped. Step can be an index, name, or a function that evaluates to
#'   TRUE or FALSE when run on the aggregation sequence. Named list does not
#'   need to contain all groupers, but if so, those that persist throughout
#'   should be given NA or numeric values longer than aggsequence. Vectors the
#'   length of groupers usually work, but are less-well supported.
#' @param aggsequence a named list of aggregation steps in the order to apply
#'   them. Entries for theme aggregation should be character vectors- e.g. `name
#'   = c('from_theme', 'to_theme')`. Entries for spatial aggregation should be
#'   the sf polygon to aggregate to, e.g. `name = sfpolygons` or a length-1
#'   character, e.g. `name = "sfpolygons"`. The latter requires the object to be
#'   available with `get("sfpolygons")`, but allows passing characters rather
#'   than objects. Not requiring names and is high on the list of improvements.
#'   If we want to be able to re-run from auto-saved metadata params, we need
#'   the names of the spatial levels to match the object, e.g. basin: basin.
#' @param funsequence a list of aggregation functions to apply in the order to
#'   apply them. Each list entry can be one value, e.g. a character or bare
#'   name, or can be multiple if mulitiple aggregations should be done at that
#'   step, e.g. `c('ArithmeticMean', 'LimitingFactor')`. The entries can also be
#'   lists themselves, useful for passing functions with arguments, e.g `list(wm
#'   = ~weighted.mean(., w = area, na.rm = TRUE))`. *Important:* as of {dplyr}
#'   1.1, if these are anonymous functions that refer to data variables (like
#'   the `w = area` argument in the [weighted.mean()] example), that list needs
#'   to be wrapped in [rlang::quo()], e.g. `rlang::quo(list(wm =
#'   ~weighted.mean(., w = area, na.rm = TRUE))`. And we can no longer mix
#'   character and other forms in the same sub-list (single aggregation step).
#' @param pseudo_spatial a character or numeric vector giving the names or
#'   indices of aggsequence that should have 'psuedo-spatial' aggregation. This
#'   is when we go from one spatial data level to another, but do the join and
#'   aggregation with a non-spatial [dplyr::left_join()]. It is developed for
#'   the EWR situation, where the incoming data is indexed to gauges and
#'   planning units, but has gauge point geometry, and spatial joining to
#'   planning units is not appropriate, because single gauges affect multiple
#'   units. So it would join to the `planning_units` by name instead of
#'   spatially, and then aggregate according to those units.
#' @param saveintermediate logical, default `FALSE`. * `FALSE` (the default):
#'   Save only the final result as a tibble or sf * `TRUE`: Save every step of
#'   the aggregation as a tibble or sf in a list
#' @param namehistory logical, default `TRUE`.
#'   * `TRUE` (the default): The name of the aggregated column(s) retain the full aggregation history of the form
#'   `agglevelN_aggfunctionN_...agglevel1_aggfunction1_originalcolumn`. This is
#'   ugly, but saves memory and says exactly what the values in each column are.
#'   * `FALSE`: The aggregation history is moved out of the column names and
#'   into new columns that define it using [agg_names_to_cols()]. The column
#'   name(s) become(s) the original column name(s) specified by `aggCols`. This
#'   is far cleaner and easier for analysis (e.g. filtering on aggregation
#'   functions at a particular step), but increases the size of the dataset and
#'   the meaning of the values in the aggregation column have to be interpreted
#'   with the values in the new columns defining history.
#' @param auto_ewr_PU logical, default `FALSE`. Auto-detect EWRs and enforce
#'   appropriate theme and spatial scaling related to gauges and planning units,
#'   as defined in [theme_aggregate()] and [spatial_aggregate()]. Specifically, if `TRUE`, this automatically manages the `group_until` and `pseudo_spatial` arguments.
#'
#' @return either a tibble or sf of aggregated values at the final level (if
#'   `saveintermediate = FALSE`) or a list of tibbles or sfs with aggregated
#'   values at each step (`saveintermediate = TRUE`)
#' @export
#'
#' @examples
multi_aggregate <- function(dat,
                            causal_edges = NULL,
                            groupers = "scenario",
                            group_until = rep(NA, length(groupers)),
                            aggCols,
                            aggsequence,
                            funsequence,
                            saveintermediate = FALSE,
                            namehistory = TRUE,
                            keepAllPolys = FALSE,
                            failmissing = TRUE,
                            auto_ewr_PU = FALSE,
                            pseudo_spatial = NULL) {
  # Check for common sources of errors
  if (!inherits(aggsequence, "list") || !inherits(funsequence, "list")) {
    rlang::abort("aggsequence and funsequence should both be lists, even if there is only one item. Otherwise iterating over their length causes unexpected behaviour.")
  }

  # start with the input data
  # Name the starting data 'input'
  if (saveintermediate) {
    datlist <- list(dat) |>
      setNames('agg_input')
    # if (is.character(aggsequence[[1]])) {
    #   names(datlist) <- aggsequence[[1]][1]
    # } else {
    #   names(datlist) <- deparse(substitute(dat))
    # }
  }

  # `get` from characters if needed
  aggsequence <- purrr::map(aggsequence, parse_geo)

  # For now, throw an informative error if trying to aggregate with backsteps in
  # the causal network. Will want to implement that at some point, probably.
  causalsteps <- aggsequence[purrr::map_lgl(aggsequence, is.character)]
  if (any(duplicated(purrr::map_chr(causalsteps, \(x) x[1])))) {
    thedups <- causalsteps[duplicated(purrr::map_chr(causalsteps, \(x) x[1]))] |>
      purrr::map_chr(\(x) x[1])
    dupfind <- function(x) {
      if (x[1] %in% thedups) {
        return(x)
      } else {
        return(NULL)
      }
    }
    dupped <- purrr::map(causalsteps, \(x) dupfind(x))
    rlang::abort(glue::glue("Aggregating multiple times from the same causal level(s):
                            {glue::glue_collapse(dupped[!purrr::map_lgl(dupped, is.null)], sep = ',\n')}.
                            This non-nested causal aggregation is currently not supported.
                            Until it is, do multiple aggregations to get to the
                            multiple outcome levels.
                            And please raise an issue on github."))
  }


  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  # and now we have to loop over the funsequence and the names are even harder to extract
  namefunsequence <- vector(mode = "list", length = length(funsequence))
  for (i in 1:length(funsequence)) {
    funlist <- funsequence[[i]]
    if (!rlang::is_quosure(funlist) &&
      (is.function(funlist) ||
        (is.list(funlist) &
          is.function(funlist[[1]])))) {
      if (length(substitute(funsequence)) == 1) {
        rlang::abort("Cannot infer names of funsequence, likely because it's a named object. Use something other than a pre-built list of bare function names.")
      }
      funlist <- as.character(substitute(funsequence)[[i + 1]]) # The +1 is because the first item is 'list'.
      if (funlist[1] == "c") {
        funlist <- funlist[2:length(funlist)]
      }
      namefunsequence[[i]] <- funlist
    }
    rm(funlist)
  }

  notnull <- which(purrr::map_lgl(namefunsequence, ~ !is.null(.)))

  if (length(notnull) > 0) {
    funsequence[notnull] <- namefunsequence[notnull]
  }


  # I *think* this is safe, because dat should never GROW groups, only lose
  # them. So turning this to characters here will be the superset of what's ever
  # encountered. I've left a tidyselect parsing version commented out in case
  # I'm wrong
  groupers <- selectcreator(rlang::enquo(groupers), dat, failmissing)
  # we need to handle different ways of getting `group_until`
  group_indices <- parse_group_until(
    group_until = group_until,
    groupers = groupers,
    aggsequence = aggsequence
  )

  # a simple fix to leaving group_until out of groupers, but won't work if groupers is fancy
  # If groupers is a character, we can just add the missing
  if (is.character(groupers) && !(all(names(group_indices) %in% groupers))) {
    groupers <- c(groupers, names(group_indices)[!(names(group_indices) %in% groupers)])
  }
  # # If groupers is *not* a character, we'll have to enforce. We have to check against basegroupers, since that will have the characters
  # if (!is.character(groupers) && !(all(names(group_indices) %in% basegroupers))) {
  #   rlang::abort("If not using characters for groupers, groupers needs to cover everything in group_until")
  # }

  # get the index for any pseudo-spatial joins and aggs
  pseudo_indices <- purrr::map_int(pseudo_spatial, \(x) parse_aggnum(x, aggsequence = aggsequence))


  # Get the dimension we're moving across at each step
  stepdim <- identify_dimension(aggsequence,
                                causal_edges)

  # There's no safe way to auto-catch theme-grouping if everything is spatial or temporal and there's no causal network passed in.
  if (all(stepdim != 'theme')) {
    causalnames <- purrr::map(causal_edges, names) |> unlist()
    if (!any(groupers %in% causalnames)) {
      rlang::warn(c("Theme grouping difficult to infer without theme steps.",
                    "Groupers does not appear to contain theme names.",
                    "Ensure theme grouping is in `groupers` if desired."))
    }
    themegroup <- NULL
  } else {
    # retain the first theme-level groupings
    themegroup <- aggsequence[min(which(stepdim == 'theme'))][[1]][1]
  }

  # We should be able to auto-catch time.
  timecols <- purrr::map_lgl(dat, \(x) lubridate::is.Date(x) | lubridate::is.POSIXt(x))
  if (any(timecols)) {
    timegroup <- names(dat)[which(timecols)]
  } else {
    timegroup <- NULL
  }

  # We do the spatial group-catch internally to the time and theme aggregation functions due to speed issues and stripping the geometry.

  # need to track the grouping when switch between theme and spatial- we want to
  # do theme groupings within the current spatial unit, and spatial groupings
  # within the current theme level. Spatial groups (geometry column) are kept
  # automatically, and so continue to affect later theme grouping (e.g. once we
  # put data in polygons, it stays there) but we have to tell it the other
  # spatial info to keep (e.g. polygon name and other attributes). This assumes
  # the other spatial info is 1:1 with geometry in the to-polygons. The theme
  # col needs to be explictly grouped_by, since we want to keep the theme
  # groupings when we do spatial.

  spatial_to_info <- NULL

  # Don't use a foreach even though it's tempting, since the loop is dependent-
  # the outcome of the first agg needs to be the input of the second

  for (i in 1:length(aggsequence)) {
    # theme aggregations will be defined as from-to pairs of character vectors,
    # spatial aggs are the polygons being agged into. That allows autodetect

    # turn the groupers and aggcols into character vectors however they came in
    # need to do this in the loop because dat changes and so available cols will too
    thisgroup <- selectcreator(rlang::enquo(groupers), dat, failmissing)
    thisagg <- selectcreator(
      rlang::expr(tidyselect::ends_with(!!aggCols)),
      dat, failmissing
    )

    # Deal with the group_until
    dropgroups <- names(group_indices)[group_indices <= i]
    # If we drop groups, we have to allow them to not be found in selectcreator
    # Why not just change groupers? Because it might be tidyselect
    if (length(dropgroups) > 0) {
      failmissing <- FALSE
    }
    thisgroup <- thisgroup[!thisgroup %in% dropgroups]


    if (stepdim[i] == 'theme') {
      # If the aggsequence isn't named, name it. This is less obviously doable
      # for sf. deal with that later. There three different ways unnamed lists
      # can end up here, and so need to deal with them all
      if (is.null(names(aggsequence[i])) ||
        is.na(names(aggsequence[i])) ||
        names(aggsequence[i]) == "") {
        names(aggsequence)[[i]] <- aggsequence[[i]][2]
      }

      # Will need to be able to go get a previous `dat` here if we want to do
      # nonnested aggsequences.
      dat <- theme_aggregate(
        dat = dat,
        from_theme = aggsequence[[i]][1],
        to_theme = aggsequence[[i]][2],
        groupers = c(thisgroup, timegroup),
        aggCols = thisagg, # Does not need the tidyselect::ends_with here because it happens in theme_aggregate
        funlist = funsequence[[i]],
        causal_edges = causal_edges,
        geonames = spatial_to_info,
        failmissing = FALSE,
        auto_ewr_PU = auto_ewr_PU
      ) # Don't fail if no gauge col

      # Update theme grouping the current level so we don't drop it during spatial
      themegroup <- aggsequence[[i]][2]

    } else if (stepdim[i] == 'spatial') {
      dat <- spatial_aggregate(
        dat = dat,
        to_geo = aggsequence[[i]],
        groupers = c(thisgroup, themegroup, timegroup),
        aggCols = thisagg,
        funlist = funsequence[[i]],
        prefix = paste0(names(aggsequence)[i], "_"),
        failmissing = failmissing,
        keepAllPolys = keepAllPolys,
        joinby = ifelse(i %in% pseudo_indices, 'nonspatial', 'spatial'),
        auto_ewr_PU = auto_ewr_PU
      )

      spatial_to_info <- names(aggsequence[[i]])[names(aggsequence[[i]]) != "geometry"]

    } else if (stepdim[i] == 'temporal') {
      dat <- temporal_aggregate(
        dat = dat,
        breaks = aggsequence[[i]],
        timecol = timegroup, # identified above
        groupers = c(thisgroup, themegroup), # retain theme grouping, spatial is retained inside
        aggCols = thisagg,
        funlist = funsequence[[i]],
        geonames = spatial_to_info,
        # prefix = ,
        failmissing = FALSE,
        auto_ewr_PU = auto_ewr_PU
      ) # Don't fail if no gauge col

      # Update theme grouping the current level so we don't drop it during spatial, though it can get dropped here.
      timecols <- purrr::map_lgl(dat, \(x) lubridate::is.Date(x) | lubridate::is.POSIXt(x))
      if (any(timecols)) {
        timegroup <- names(dat)[which(timecols)]
      } else {
        timegroup <- NULL
      }
    }



    # add each level to a list if saving
    if (saveintermediate) {
      # namehistory saves history in the names, otherwise in a set of columns
      # The assignment for namehistory fails
      if (!namehistory) {
        datlist[[names(aggsequence)[i]]] <- agg_names_to_cols(dat,
          aggsequence = names(aggsequence[1:i]),
          funsequence = funsequence[1:i],
          aggCols = aggCols
        )
      } else {
        # aggsequence[[i]][2]
        thisname <- names(aggsequence)[i]
        if (is.null(names(aggsequence)[i])) {
          thisname <- length(datlist) + 1
        }
        datlist[[thisname]] <- dat
      }
    }
  }

  # Determine what to return
  if (saveintermediate) {
    return(datlist)
  } else {
    # history in columns if namehistory is FALSE
    if (!namehistory) {
      dat <- agg_names_to_cols(dat,
        aggsequence = names(aggsequence),
        funsequence = funsequence,
        aggCols = aggCols
      )
    }

    return(dat)
  }
}
