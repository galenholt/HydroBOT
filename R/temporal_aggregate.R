#' Aggregate along the time dimension
#'
#' Takes data with a column in a time format (posix or date) and aggregates into
#' groups defined by `breaks` while retaining theme and spatial information.
#' Much of the setup and checks mirrors that of [theme_aggregate()] and
#' [spatial_aggregate()], and then passes to [general_aggregate()].
#'
#' @inheritParams general_aggregate
#' @inheritParams theme_aggregate
#'
#' @param dat sf or tibble or dataframe of values to aggregate, with necessary
#'   grouping information for non-temporal axes (e.g. scenario, location,
#'   theme). *Not* necessarily the same as `data` in [general_aggregate()]- this
#'   function makes necessary adjustments to the data before calling
#'   [general_aggregate()]
#' @param breaks breaks for the time groupings. default 'all_time' collapses
#'   over the whole period, otherwise as in [base::cut.POSIXt()] (takes the
#'   character options there, or time objects)
#' @param timecol name of the column with time in it. Default 'infer' infers
#'   which it is by looking for POSIXt or Date types.
#' @param groupers as in [general_aggregate()], with the note that these should
#'   be all grouping columns *except* any spatial information. Spatial
#'   information is automatically added to `groupers` before passing to
#'   [general_aggregate()].
#' @param geonames additional columns of information to persist through the
#'   aggregation. Cannot change with the main `groupers`. Developed to persist
#'   information about polygons through subsequent theme aggregations, but could
#'   be more general
#' @param prefix prefix character, differs from [general_aggregate()] in that
#'   default is `'temporal_'` instead of `'agg_'`.
#'
#' @return a tibble (possibly `sf` if fed an `sf`) with columns for the grouping
#'   variables aggregated into the time periods specified by `breaks`
#' @export
#'
temporal_aggregate <- function(dat,
                               breaks = "all_time",
                               timecol = "infer",
                               groupers,
                               aggCols,
                               funlist,
                               geonames = NULL,
                               prefix = "temporal_",
                               failmissing = TRUE,
                               auto_ewr_PU = FALSE,
                               ...) {
  if (timecol == "infer") {
    timecol <- purrr::map_lgl(dat, \(x) lubridate::is.Date(x) | lubridate::is.POSIXt(x)) |>
      which() |>
      names()
  }
  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  if (is.function(funlist) || (is.list(funlist) & is.function(funlist[[1]]))) {
    funlist <- as.character(substitute(funlist))
    if (funlist[1] == "c") {
      funlist <- funlist[2:length(funlist)]
    }
  }

  # As in theme_aggregate, we need to  deal with spatial dataframes

  # including geometry in non-geometric
  # aggregates takes forever, drop and re-pair if present

  # this is now getting even closer to spatial with the drop/add of geometry.
  # can they be the same function? probably.
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

  # EWRs are defined at gauges and planning units (often many-to-many, e.g.
  # gauges might contribute to EWRs in multiple PUs, and PUs might include
  # several gauges). we want to map back to that until we've done spatial
  # aggregation into something larger. This is only true of ewrs. So check if
  # we're not yet in polygons (since then we're at least to planning_units), and
  # if these are EWRs. If yes, then add gauge and planning unit to groupers. We
  # don't have to do something similar for spatial, because this gets taken care
  # of as soon as we're above hte Planning Unit scale (though I suppose it's
  # possible to aggregate to something smaller than a PU and larger than a
  # gauge, that's an edge case to deal with later)
  if (!polyflag) {
    # Infer EWR from groupers
    ewrnames <- purrr::map(HydroBOT::causal_ewr, names) |> unlist()
    isewr <- any(groupers %in% ewrnames)
    # If ewr, we need to be grouping by both planning unit and SWSDLname until we're into polygons
    if (isewr & !("planning_unit_name" %in% groupers & "SWSDLName" %in% groupers)) {
      if (!auto_ewr_PU) {
        rlang::warn(c(
          "!" = "EWR outputs detected without `group_until`!",
          "i" = "EWR outputs should be grouped by `SWSDLName`, `planning_unit_name`, and `gauge` until aggregated to larger spatial areas.",
          "i" = "Best to explicitly use `group_until` in `multi_aggregate()` or `read_and_agg()`.",
          "i" = "Lower-level processing should include as `grouper` in `temporal_aggregate()`\n"
        ))
      } else {
        rlang::inform(c("i" = "EWR outputs auto-grouped",
                        "*" = "Done automatically because `auto_ewr_PU = TRUE`",
          "*" = "EWRs should be grouped by `SWSDLName`, `planning_unit_name`, and `gauge` until aggregated to larger spatial areas.",
          "*" = "Rows will collapse otherwise, silently aggregating over the wrong dimension",
          "*" = "Best to explicitly use `group_until` in `multi_aggregate()` or `read_and_agg()`.\n"
        ))
        # add gauge, planning unit, and SWSDL if available to prevent premature collapse.
        groupers <- unique(c(groupers, c("gauge", "planning_unit_name", "SWSDLName")))
      }
    }
  }


  # make the groupings for the time dimension
  if (is.character(breaks) && grepl("all", breaks)) {
    dat$time_group <- 1
    retime <- FALSE
  } else {
    dat <- dat |>
      dplyr::mutate(time_group = cut(.data[[timecol]], breaks = breaks))
    retime <- TRUE
  }

  groupers <- c(groupers, "time_group")

  agged <- general_aggregate(dat,
    groupers = groupers,
    aggCols = tidyselect::ends_with(!!aggCols),
    funlist = funlist,
    failmissing = failmissing,
    prefix = prefix
  )


  # Drop the time group if we only have one.
  if (!retime) {
    agged <- agged |>
      dplyr::select(-"time_group")
  }

  if (retime) {
    agged <- agged |>
      dplyr::mutate({{ timecol }} := as.Date(as.character(.data$time_group))) |>
      dplyr::select(-"time_group")
  }

  if (spatialflag) {
    # if data comes in with group_until, it sometimes has groupers that need to
    # persist.
    commonnames <- groupers[groupers %in% names(geodat)]
    dat <- dplyr::left_join(agged, geodat, by = commonnames) |>
      sf::st_as_sf()
  } else {
    dat <- agged
  }

  dat <- dplyr::ungroup(dat)

  return(dat)
}
