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
#'   necessarily the same as `data` in [general_aggregate()]- this function
#'   makes necessary adjustments to the data before calling
#'   [general_aggregate()]
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
#' @param auto_ewr_PU logical, default FALSE. If TRUE, automatically infers
#'   whether this is an EWR dataset and has not yet been spatially aggregated.
#'   If so, applies grouping by 'planning_unit_name' and SWSDLName' since EWRs
#'   are defined in those units. The preferred solution is to include it in
#'   `groupers` here (for on-off), or in [multi_aggregate()] to use
#'   `group_until`. If none of those solutions happen, though, it aborts to
#'   prevent incorrect pooling over rows in different planning units and sdl units.
#' @param ... passed to [general_aggregate()]
#'
#' @return a dataframe in the same format as read-in (sf or tibble), aggregated
#'   to the `to_theme` level and retaining desired geographic information if
#'   included
#' @export
#'
theme_aggregate <- function(dat,
                            from_theme,
                            to_theme,
                            groupers,
                            aggCols,
                            funlist,
                            causal_edges,
                            geonames = NULL,
                            failmissing = TRUE,
                            auto_ewr_PU = FALSE,
                            ...) {
  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple
  # bare names.
  if (is.function(funlist) || (is.list(funlist) & is.function(funlist[[1]]))) {
    funlist <- as.character(substitute(funlist))
    if (funlist[1] == "c") {
      funlist <- funlist[2:length(funlist)]
    }
  }

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
      glue::glue("The time column {timegroup} is not included as a grouper for theme aggregation"),
      "Aggregation must explicitly specify dimensions"
    ))
  }


  # including geometry in non-geometric
  # aggregates takes forever, drop and re-pair if present

  # We need to greate a new column and modify groupers here. We don't for things
  # like temporal, where any relevant time column already exists and we can pass
  # it in in groupers.
  # this is now getting even closer to spatial with the
  # drop/add of geometry. can they be the same function? probably.
  spatialflag <- is_sf(dat)
  polyflag <- is_notpoint(dat)

  if (spatialflag) {
    if (!("polyID" %in% names(dat))) {
      dat <- dat |>
        add_polyID(failduplicate = FALSE)
    }

    geodat <- dat |>
      dplyr::select("polyID", tidyselect::all_of(geonames)) |>
      dplyr::group_by(.data$polyID) |>
      dplyr::slice(1) |> # usual use of dplyr::distinct() checks the polys factorially. slice just indexes.
      dplyr::ungroup()

    dat <- sf::st_drop_geometry(dat)
    groupers <- c(groupers, "polyID")
  }

  # clean up the edges to only relevant
  # auto-generate the edges if a full list has been passed in
  if (!inherits(causal_edges, "data.frame") & inherits(causal_edges, "list")) {
    causal_edges <- make_edges(causal_edges, list(c(from_theme, to_theme)))
  }
  causal_edges <- causal_edges |>
    dplyr::filter(.data$fromtype == from_theme & .data$totype == to_theme) |>
    dplyr::select(tidyselect::where(~ !all(is.na(.))))

  # check and dplyr::rename
  if (length(unique(causal_edges$fromtype)) > 1 | length(unique(causal_edges$totype)) > 1) {
    stop("trying to aggregate into multiple groupings")
  } else {
    names(causal_edges)[names(causal_edges) == "to"] <- causal_edges$totype[1]
    names(causal_edges)[names(causal_edges) == "from"] <- causal_edges$fromtype[1]
  }

  # EWRs are defined at gauges and planning units (often many-to-many, e.g.
  # gauges might contribute to EWRs in multiple PUs, and PUs might include
  # several gauges). we want to map back to that until we've done spatial
  # aggregation into something larger. This is only true of ewrs. So check if
  # we're not yet in polygons (since then we're at least to planning_units), and
  # if these are EWRs. If yes, then add gauge and planning unit to groupers. We
  # don't have to do something similar for spatial, because this gets taken care
  # of as soon as we're above the Planning Unit scale (though I suppose it's
  # possible to aggregate to something smaller than a PU and larger than a
  # gauge, that's an edge case to deal with later)
  if (!polyflag) {
    # Infer EWR from presence in causal_ewr
    ewrnames <- purrr::map(HydroBOT::causal_ewr, names) |> unlist()
    isewr <- to_theme %in% ewrnames
    # ewrs are defined at the pu scale, so keep them, gauges, and sdl units until safe to drop
    if (isewr & !("planning_unit_name" %in% groupers & "SWSDLName" %in% groupers)) {
      if (!auto_ewr_PU) {
        rlang::warn(c(
          "!" = "EWR outputs detected without `group_until`!",
          "i" = "EWR outputs should be grouped by `SWSDLName`, `planning_unit_name` and `gauge` until aggregated to larger spatial areas.",
          "i" = "Preferred method of addressing this is with `group_until` in `multi_aggregate()` or `read_and_agg()`.",
          "i" = "Lower-level processing should include as `grouper` in `theme_aggregate()`"
        ))
      } else {
        rlang::inform(c("i" = "EWR outputs auto-grouped",
          "*" = "EWRs should be grouped by `SWSDLName`, `planning_unit_name`, and `gauge` until aggregated to larger spatial areas.",
          "*" = "gauge is less important, since it has the geometry, but the gauge column will be lost otherwise.",
          "*" = "Preferred method of addressing this is with `group_until` in `multi_aggregate()` or `read_and_agg()`.",
          "*" = "Lower-level processing handles by including as `grouper` in `theme_aggregate()`, which is being done automatically because `auto_ewr_PU = TRUE`."
        ))
        # add gauge and plannng unit name if available.
        groupers <- unique(c(groupers, intersect(c("gauge", "planning_unit_name", "SWSDLName"), names(causal_edges))))
      }
    }
  }

  # join to causal_edges

  # A bit of a hacky check
  extragroups <- groupers[!groupers %in% c("scenario", "polyID", timegroup)]
  if (any(!(extragroups %in% names(causal_edges)))) {
    rlang::warn(c(
      "Causal network does not have all groupers.",
      glue::glue("Joining {from_theme} to {to_theme}"),
      glue::glue("Groupers are {paste0(groupers, collapse = ', ')}."),
      glue::glue("expect causal network to have {paste0(extragroups, collapse = ', ')}; it has {paste0(names(causal_edges), collapse = ', ')}."),
      "Do you need to use `group_until`? Or is your network missing columns?"
    ))
  }

  # Quiet down the joins
  commonnames <- names(dat)[names(dat) %in% names(causal_edges)]
  pairdat <- dat |>
    dplyr::left_join(causal_edges, relationship = "many-to-many", by = commonnames)


  # Check for NA in the causal network
  ergroups <- c(from_theme, groupers[groupers != "scenario"])
  napairs <- pairdat |>
    dplyr::filter(is.na(.data[[to_theme]])) |>
    dplyr::select(tidyselect::any_of(ergroups)) |>
    dplyr::distinct()

  if (nrow(napairs) > 0) {
    nacount <- napairs |> dplyr::summarise(ntimes = dplyr::n(), .by = tidyselect::any_of(from_theme))

    if (ncol(napairs) > 1) {
      groupinform <- glue::glue("Groups with issues: {unique(napairs[2])}")
    } else {
      groupinform <- NULL
    }

    rlang::inform(c(
      "!" = "Unmatched links in causal network",
      "*" = glue::glue("{nrow(nacount)} from {from_theme} to {to_theme}") #,
      # groupinform # This was way too noisy.
    ))

    # Now delete the NA
    pairdat <- pairdat |>
      dplyr::filter(!is.na(.data[[to_theme]]))
  }

  # The core aggregation function. the !! needs to happen here
  # because aggCols needs to be evaluated to a character vector, not passed in
  # as an object name. Annoying.
  agged <- general_aggregate(pairdat,
    groupers = c(groupers, to_theme), # Group by the groupers plus the level we're grouping into
    aggCols = tidyselect::ends_with(!!aggCols),
    funlist = funlist,
    prefix = paste0(to_theme, "_"), # replace the general 'theme' prefix with the specific theme level being aggregated to
    failmissing = failmissing,
    ...
  )

  if (spatialflag) {
    # if data comes in with group_until, it sometimes has groupers that need to persist.
    commonnames <- groupers[groupers %in% names(geodat)]
    dat <- dplyr::left_join(agged, geodat, by = commonnames) |>
      sf::st_as_sf()
  } else {
    dat <- agged
  }

  return(dat)
}
