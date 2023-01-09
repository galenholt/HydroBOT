#' Aggregate along spatial dimension
#'
#' Takes geographic data (points or polygons), and aggregates into polygons,
#' while retaining theme-level information. This function sets up the data with
#' specific prep for the way the spatial dimension works, and then wraps
#' `general_aggregate`. Many of the arguments here are just passed through to
#' general aggregate.
#'
#' @inheritParams general_aggregate
#'
#' @param dat sf of values to aggregate with any necessary non-spatial
#'   grouping information (e.g. scenario, theme)
#' @param to_geo sf polygon or multipolygon that provides the desired spatial
#'   level to group into. This uses the intersection [sf::st_intersection()], so
#'   if `dat` and `to_geo` are both polygons, they do not have to be
#'   nested.
#' @param groupers as in [`general_aggregate`], with the note that these should
#'   be all grouping columns *except* the polygons in `to_geo`, which are
#'   automatically added to `groupers` before passing to `general_aggregate`.
#' @param whichcrs desired coordinate reference system, easiest is just the
#'   numeric EPSG code, but could a full crs definition. See [sf::st_crs()]
#' @param keepAllPoly logical, default `FALSE`. Should polygons in `to_geo` that
#'   have no values in `dat` be retained? The default `FALSE` keeps NA
#'   polygons from cluttering things up, but `TRUE` is very useful to not lose
#'   them, especially for later plotting.
#' @param prefix character, differs from `general_aggregate` in that default is
#'   `'spatial_'` instead of `'agg_'`.
#'
#' @return an `sf` with columns for the grouping variables aggregated into the
#'   polygons in `to_geo` and retaining desired theme-level information
#' @export
#'
#' @examples
spatial_aggregate <- function(dat, to_geo, groupers,
                             aggCols, funlist, ...,
                           whichcrs = sf::st_crs(to_geo),
                           keepAllPolys = FALSE,
                           failmissing = TRUE,
                           prefix = 'spatial_') {


  # making valid and adding polyID here and not inside spatial_joiner because
  # need to be valid with IDs later too. I should really do these with the input
  # polygons, not here. I need the to_geo again later, but not the dat. And
  # the from_ is more likely to have a ton of duplication, so do it's cleaning
  # after cutting to unique polys in spatial_joiner
  to_geo <- to_geo %>%
    crs_clean(whichcrs) %>%
    sf::st_make_valid() %>%
    add_polyID()

  # make the intersected df for aggregating
  fromto_pair <- spatial_joiner(dat, to_geo, whichcrs = whichcrs)

  # Clean up groupers and aggCols from various formats and ensure only present
  # columns are included.
  groupers <- selectcreator(rlang::enquo(groupers), fromto_pair, failmissing)
  aggCols <- selectcreator(rlang::enquo(aggCols), fromto_pair, failmissing)

  # the code typically drops polygons that have no data. but we might want to keep them for plotting.

  if (keepAllPolys) {
    # get those without data
    unusedPolys <- to_geo[which(!(to_geo$polyID %in% fromto_pair$polyID)), ]

    if (nrow(unusedPolys) > 0) {
      # need to save for each combo of grouping variable
      allgroups <- fromto_pair %>% dplyr::distinct(dplyr::across({{groupers}}))
      # combine
      unusedPolys <- full_join(unusedPolys, allgroups, by = character())
    }

  }

  # add the polygon id being grouped into to the grouping variables
  groupers <- c(groupers, 'polyID')

  # aggCols and funlist might change (see theme_agg_multi) if we have a multi-style wrapper. same with prefix
  # could just use bare aggCols, but it throws warnings. Could use tidyselect::ends_with too, as in theme.

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that
  if (is.function(funlist)) {
    funlist <- as.character(substitute(funlist))
  }

  agged <- general_aggregate(fromto_pair,
                              groupers = groupers,
                              aggCols = tidyselect::all_of(!!aggCols),
                              funlist = funlist,
                              failmissing = failmissing,
                              prefix = prefix)


  # glue back onto the polygons. sf is lost because the x doesn't have the
  # geometry.
  # The original polys get added with a join using polyID so it doesn't matter if
  # the dataframe gets shuffled or if there were lost areas in the intersection
  # step.
  aggPoly <- dplyr::left_join(agged, to_geo, by = 'polyID') %>%
    sf::st_as_sf()

  # add the NAs on if we want
  if (keepAllPolys) {
    aggPoly <- dplyr::bind_rows(aggPoly, unusedPolys)
  }


  return(aggPoly)

}




