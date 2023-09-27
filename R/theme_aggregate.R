#' Aggregate along theme dimension
#'
#' Aggregation along the theme dimension uses causal mappings to aggregate from
#' lower to higher levels. This function sets up the data with specific prep for
#' the way the theme dimension works, and then wraps [general_aggregate()]. Many
#' of the arguments here are just passed through to general aggregate.
#'
#' @inheritParams general_aggregate
#'
#' @param dat sf or tibble or dataframe of values to aggregate, with necessary
#'   grouping information for non-theme axis (e.g. scenario, location). *Not*
#'   necessarily the same as `data` in [general_aggregate()]- this function makes
#'   necessary adjustments to the data before calling [general_aggregate()]
#' @param from_theme character, column name of the theme level the data is
#'   currently in
#' @param to_theme character, column name of the theme level to aggregate *to*
#' @param groupers as in [general_aggregate()], with the note that these should
#'   be all grouping columns *except* the theme column specified by `to_theme`
#'   and any spatial information. These are both automatically added to
#'   `groupers` before passing to [general_aggregate()].
#' @param causal_edges the causal links between theme levels. Needs to include
#'   `from_theme` and `to_theme`, but can also include others. Creates the theme
#'   grouping
#' @param geonames additional columns of information to persist through the
#'   aggregation. Cannot change with the main `groupers`. Developed to persist
#'   information about polygons through subsequent theme aggregations, but could
#'   be more general
#' @param ... passed to [general_aggregate()]
#'
#' @return a dataframe in the same format as read-in (sf or tibble), aggregated
#'   to the `to_theme` level and retaining desired geographic information if
#'   included
#' @export
#'
#' @examples
theme_aggregate <- function(dat,
                            from_theme,
                            to_theme,
                            groupers,
                            aggCols,
                            funlist,
                            causal_edges,
                            geonames = NULL,
                            failmissing = TRUE,
                            ...) {

  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  if (is.function(funlist) || (is.list(funlist) & is.function(funlist[[1]]))) {
    funlist <- as.character(substitute(funlist))
    if(funlist[1] == "c") {funlist <- funlist[2:length(funlist)]}
  }

  # including geometry in non-geometric
  # aggregates takes forever, drop and re-pair if present

  # this is now getting even closer to spatial with the drop/add of geometry.
  # can they be the same function? probably.
  spatialflag <- FALSE
  polyflag <- FALSE
  if ('sf' %in% class(dat)) {
    spatialflag <- TRUE
    if (!all(sf::st_is(dat, 'POINT'))) {polyflag <- TRUE}

    if (!('polyID' %in% names(dat))) {
      dat <- dat %>%
        add_polyID(failduplicate = FALSE)
    }

    geodat <- dat %>%
      dplyr::select(polyID, tidyselect::all_of(geonames)) %>%
      dplyr::group_by(polyID) %>%
      dplyr::slice(1) %>% # usual use of dplyr::distinct() checks the polys factorially. slice just indexes.
      dplyr::ungroup()

    dat <- sf::st_drop_geometry(dat)
    groupers <- c(groupers, 'polyID')
  }

  # may not need this, but too many conditionals
  g2p <- extract_gauge_pu(causal_edges)

  # clean up the edges to only relevant
  # auto-generate the edges if a full list has been passed in
  if (!inherits(causal_edges, 'data.frame') & inherits(causal_edges, 'list')) {
    causal_edges <- make_edges(causal_edges, list(c(from_theme, to_theme)))
  }
  causal_edges <- causal_edges %>%
    dplyr::filter(fromtype == from_theme & totype == to_theme) %>%
    dplyr::select(tidyselect::where(~!all(is.na(.))))

  # check and dplyr::rename
  if (length(unique(causal_edges$fromtype)) > 1 | length(unique(causal_edges$totype)) > 1) {
    stop("trying to aggregate into multiple groupings")
  } else {
    names(causal_edges)[names(causal_edges) == 'to'] <- causal_edges$totype[1]
    names(causal_edges)[names(causal_edges) == 'from'] <- causal_edges$fromtype[1]
  }


  # the theme-level outcomes are defined at gauges. we want to map back to that
  # until we've done spatial aggregation into something larger. this conditional
  # is really ugly though

  if (spatialflag && polyflag) {
    pairdat <- dat %>%
      dplyr::left_join(causal_edges, relationship = "many-to-many")
  } else {
    pairdat <- dat %>%
      dplyr::left_join(g2p, relationship = "many-to-many") %>%
      dplyr::left_join(causal_edges, relationship = "many-to-many")
  }


  # The core aggregation function. the !! needs to happen here
  # because aggCols needs to be evaluated to a character vector, not passed in
  # as an object name. Annoying.
  agged <- general_aggregate(pairdat,
                           groupers = c(groupers, to_theme), # Group by the groupers plus the level we're grouping into
                           aggCols = tidyselect::ends_with(!!aggCols),
                           funlist = funlist,
                           prefix = paste0(to_theme, '_'),# replace the general 'theme' prefix with the specific theme level being aggregated to
                           failmissing = failmissing,
                           ...)

  if (spatialflag) {
    dat <- dplyr::left_join(agged, geodat, by = 'polyID') %>%
      sf::st_as_sf()
  } else {
    dat <- agged
  }

  return(dat)

}
