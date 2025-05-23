#' Aggregate along spatial dimension
#'
#' Takes geographic data (points or polygons), and aggregates into polygons,
#' while retaining theme-level information. This function sets up the data with
#' specific prep for the way the spatial dimension works, and then wraps
#' [general_aggregate()]. Many of the arguments here are just passed through to
#' general aggregate.
#'
#' @inheritParams general_aggregate
#'
#' @param dat sf of values to aggregate with any necessary non-spatial grouping
#'   information (e.g. scenario, theme)
#' @param to_geo sf polygon or multipolygon that provides the desired spatial
#'   level to group into. This uses the intersection [sf::st_intersection()], so
#'   if `dat` and `to_geo` are both polygons, they do not have to be nested.
#' @param groupers as in [general_aggregate()], with the note that these should
#'   be all grouping columns *except* the polygons in `to_geo`, which are
#'   automatically added to `groupers` before passing to [general_aggregate()].
#' @param whichcrs desired coordinate reference system, easiest is just the
#'   numeric EPSG code, but could a full crs definition. See [sf::st_crs()]
#' @param keepAllPolys logical, default `FALSE`. Should polygons in `to_geo`
#'   that have no values in `dat` be retained? The default `FALSE` keeps NA
#'   polygons from cluttering things up, but `TRUE` can be useful to not lose
#'   them, especially for later plotting. However, it is typically best from a
#'   data and cleanliness perspective to use `FALSE` here and use the bare set
#'   of polys as an `underlay` in [plot_outcomes()].
#' @param prefix character, differs from [general_aggregate()] in that default
#'   is `'spatial_'` instead of `'agg_'`.
#' @param joinby character, default 'spatial' performs the expected spatial join using geometry, 'nonspatial' performs a [dplyr::left_join()] by common column names, typically as a result of calling [multi_aggregate()] with `pseudo_spatial = 'planning_units'`.
#' @param auto_ewr_PU logical, default FALSE. If TRUE, automatically infers
#'   whether this is an EWR dataset is undergoing gauge to sdl or planning unit aggregation.
#'   If so, joins data non-spatially (sets `joinby = 'nonspatial'`). The preferred solution is
#'   to use `joinby` in [spatial_aggregate()] or `pseudo_spatial` in [multi_aggregate()].
#'   If none of those solutions happen, though, it aborts to
#'   prevent incorrectly spatial joining of gauges to planning units.
#'
#' @return an `sf` with columns for the grouping variables aggregated into the
#'   polygons in `to_geo` and retaining desired theme-level information
#' @export
#'
spatial_aggregate <- function(dat,
                              to_geo,
                              groupers,
                              aggCols,
                              funlist,
                              ...,
                              whichcrs = sf::st_crs(to_geo),
                              keepAllPolys = FALSE,
                              failmissing = TRUE,
                              prefix = "spatial_",
                              joinby = "spatial",
                              auto_ewr_PU = FALSE) {
  # making valid and adding polyID here and not inside spatial_joiner because
  # need to be valid with IDs later too. I should really do these with the input
  # polygons, not here. I need the to_geo again later, but not the dat. And
  # the from_ is more likely to have a ton of duplication, so do it's cleaning
  # after cutting to unique polys in spatial_joiner
  to_geo <- to_geo |>
    crs_clean(whichcrs) |>
    sf::st_make_valid() |>
    add_polyID()

  # make the intersected df for aggregating
  # usually spatial join, but sometimes we want to do a traditional left join (e.g. EWR gauges to planning units)

  # and we might want to automate for EWRs (or at least catch)
  # If we have a df with 'gauge', and we're trying to go to either planning_unit or sdl_unit
  isewrgaugepu <- any(grepl("ewr", names(dat))) &
    any(grepl("gauge", names(dat))) &
    any(grepl("planning_unit_name|SWSDLName", names(to_geo)))

  if (isewrgaugepu & joinby != "nonspatial") {
    if (!auto_ewr_PU) {
      rlang::warn(c("!", "EWR gauge to sdl units or planning units detected without `pseudo_spatial`!",
        "i" = "Gauges inform multiple SDLs and PUs; this will be lost.",
        "i" = "EWR outputs should be joined to `sdl_units` (or `planning_units`) pseudo-spatially (by column names), not with a spatial join",
        "i" = "Best to explicitly use `pseudo_spatial = 'sdl_units'` in `multi_aggregate()` or `read_and_agg()`.",
        "i" = "Lower-level processing should include as `joinby = 'nonspatial'` in `spatial_aggregate()`\n"
      ))
    } else {
      rlang::inform(c("i" = "EWR gauges joined to larger units pseudo-spatially.",
                      "*" = "Done automatically because `auto_ewr_PU = TRUE`",
                      "*" = "Non-spatial join needed because gauges may inform areas they are not within",
        # "*" = "EWR outputs should be joined to `sdl_units` (or `planning_units`) pseudo-spatially (by column names), not with a spatial join",
        "*" = "Best to explicitly use `pseudo_spatial = 'sdl_units'` in `multi_aggregate()` or `read_and_agg()`.\n"
      ))
      joinby <- "nonspatial"
    }
  }

  if (joinby == "spatial") {
    fromto_pair <- spatial_joiner(from_geo = dat, to_geo = to_geo, whichcrs = whichcrs)
  } else if (joinby == "nonspatial") {
    fromto_pair <- pseudo_spatial_joiner(from_geo = dat, to_geo = to_geo, prefix = prefix)
  } else {
    rlang::abort("code set up to pass column names with joinby, but not fully. If needed, write the last bits to make it work generally.")
    # fromto_pair <- dplyr::left_join(sf::st_drop_geometry(dat),
    #                                 sf::st_drop_geometry(to_geo),
    #                                 by = joinby)
  }

  # Clean up groupers and aggCols from various formats and ensure only present
  # columns are included.
  groupers <- selectcreator(rlang::enquo(groupers), fromto_pair, failmissing)
  aggCols <- selectcreator(rlang::enquo(aggCols), fromto_pair, failmissing)

  # check time- this should come in from outside, but check here too
  timecols <- purrr::map_lgl(dat, \(x) lubridate::is.Date(x) | lubridate::is.POSIXt(x))
  if (any(timecols)) {
    timegroup <- names(dat)[which(timecols)]
  } else {
    timegroup <- NULL
  }

  # Force time-aggregation to be explicit
  if (!is.null(timegroup) && !timegroup %in% groupers) {
    rlang::abort(c(
      glue::glue("The time column {timegroup} is not included as a grouper for spatial aggregation"),
      "Aggregation must explicitly specify dimensions"
    ))
  }


  # the code typically drops polygons that have no data. but we might want to keep them for plotting.

  if (keepAllPolys) {
    # get those without data
    unusedPolys <- to_geo[which(!(to_geo$polyID %in% fromto_pair$polyID)), ]

    if (nrow(unusedPolys) > 0) {
      # need to save for each combo of grouping variable
      allgroups <- fromto_pair |> dplyr::distinct(dplyr::across({{ groupers }}))
      # combine
      unusedPolys <- dplyr::cross_join(unusedPolys, allgroups)
    }
  }

  # add the polygon id being grouped into to the grouping variables
  groupers <- c(groupers, "polyID")

  # aggCols and funlist might change (see theme_agg_multi) if we have a multi-style wrapper. same with prefix
  # could just use bare aggCols, but it throws warnings. Could use tidyselect::ends_with too, as in theme.

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  # Have to specifically exclude quosures to avoid rlang warning, but this conditional is a mess.
  if (!rlang::is_quosure(funlist) &&
    (is.function(funlist) ||
      (is.list(funlist) &
        is.function(funlist[[1]])))) {
    funlist <- as.character(substitute(funlist))
    if (funlist[1] == "c") {
      funlist <- funlist[2:length(funlist)]
    }
  }

  agged <- general_aggregate(fromto_pair,
    groupers = groupers,
    aggCols = tidyselect::ends_with(!!aggCols),
    funlist = funlist,
    failmissing = failmissing,
    prefix = prefix
  )


  # glue back onto the polygons. sf is lost because the x doesn't have the
  # geometry.
  # The original polys get added with a join using polyID so it doesn't matter if
  # the dataframe gets shuffled or if there were lost areas in the intersection
  # step.

  # if data comes in with group_until, it sometimes has groupers that need to persist.
  commonnames <- groupers[groupers %in% names(to_geo)]
  aggPoly <- dplyr::left_join(agged, to_geo, by = commonnames) |>
    sf::st_as_sf()

  # add the NAs on if we want
  if (keepAllPolys) {
    aggPoly <- dplyr::bind_rows(aggPoly, unusedPolys)
  }


  return(aggPoly)
}
