#' Joins spatial from and to `sf` objects with needed data prep for the
#' aggregate functions
#'
#' This ensures the polygons have the same crs, are valid, optimises the
#' `sf::st_intersection`, calculates an `area` column for future use, and drops
#' geometry while retaining the `polyID` from the `from_geo` as a grouper for
#' further aggregation and later re-joining to the geometry
#'
#' @param from_geo `sf` of the input data to be aggregated
#' @param to_geo `sf` of the polygons or multipolygons to aggregate into
#' @param whichcrs desired crs. Either the numeric EPSG or a crs definition, as
#'   from `sf::st_crs()`
#'
#' @return tibble (*not* `sf`) with a `polyID` column mapping each row to it's
#'   relevant polygon in `polyID` and an `area` column for the area of each
#'   polygon resulting from the intersection of `from_geo` and `to_geo`. Rows do
#'   not match `to_geo` because `to_geo` is intersected with `from_geo`, and so
#'   each row is a unique combination of the two (`from_geo` rows may be
#'   duplicated across several `to_geo` polygons). Each of these unique polygons
#'   has the `area` calculated and added as a column. The geometry is dropped
#'   for speed, and expected to add back in later with a `dplyr::left_join` to `to_geo`
#'   after aggregation or other processing.
#' @keywords internal
#'

spatial_joiner <- function(from_geo, to_geo, whichcrs) {
  if (!("polyID" %in% names(from_geo))) {
    # we expect there may be duplicates here
    from_geo <- from_geo |>
      add_polyID(failduplicate = FALSE)
  }

  # To speed things up, only do a spatial join on *unique* polygons, and join
  # the rest back on later with a join on the polyID.
  from_poly <- from_geo |>
    dplyr::select(polyID_f = "polyID") |>
    dplyr::group_by(.data$polyID_f) |>
    dplyr::slice(1) |> # usual use of dplyr::distinct() checks the polys factorially. slice just indexes.
    dplyr::ungroup() |>
    # Do the cleaning here to avoid duplication
    crs_clean(whichcrs) |>
    sf::st_make_valid()

  to_poly <- to_geo |>
    dplyr::select(polyID_t = "polyID") |>
    dplyr::group_by(.data$polyID_t) |>
    dplyr::slice(1) |> # usual use of dplyr::distinct() checks the polys factorially. slice just indexes.
    dplyr::ungroup()

  from_data <- sf::st_drop_geometry(from_geo) |>
    dplyr::rename(polyID_f = "polyID")
  to_data <- sf::st_drop_geometry(to_geo) |>
    dplyr::rename(polyID_t = "polyID")


  # This conditional is annoying, and st_intersection works for points, but
  # it's much slower. And this also lets us auto-calculate area only when needed
  # (e.g. when the input scale *has* area)
  if (all(sf::st_is(from_geo, "POINT"))) {
    fromto_pair <- sf::st_join(from_poly, to_poly) |>
      sf::st_drop_geometry()
  } else {
    # The complexity of the polygons makes a huge difference in the timing, and
    # it matters which is in the x- vs y-position of st_intersection. So,
    # calculate complexity, warn if high, and arrange the x and y positions. We
    # could auto-simplify if we added a line for st_simplify and passed
    # dTolerance, but I think better not to?

    fromverts <- vertcount(from_poly)
    toverts <- vertcount(to_poly)

    # Need to test more vertices, 100k is a rough guess based on limited testing.
    if (fromverts > 100000) {
      warning(glue::glue("from_geo has {fromverts} vertices. Consider simplifying (see `st_simplify` dTolerance) for faster performance"))
    }

    if (toverts > 100000) {
      warning(glue::glue("to_geo has {toverts} vertices. Consider simplifying (see `st_simplify` dTolerance) for faster performance"))
    }

    # st_intersection leaves only the intersection, NOT the full area of either
    # polygon. That's what we want here- this gets all the pieces that fall in
    # each polygon pair, and then aggregates them into the to-polygon (because
    # we've attached those IDs). Those values then get put back into the
    # complete to-polygons (ie not chopped-up by the intersection), because we
    # join onto the originals with the id. This both ensures we're not losing
    # areas that don't fall in any from-poly, and dramatically speeds up the
    # `summarise`

    # The conditional is annoying, but gives dramatic speedup because the
    # spatial indexing on x means putting the low-vertex set of polygons in x is
    # MUCH faster
    # muffle the constant sf warnings about attributes being spatially constant
    if (toverts <= fromverts) {
      fromto_pair <- withCallingHandlers(
        warning = function(cnd) {
          if ((grepl('attribute variables are assumed to be spatially constant', cnd$message))) {
            rlang::cnd_muffle(cnd)
          }
        },
        sf::st_intersection(to_poly, from_poly)
      )
    } else {
      fromto_pair <- withCallingHandlers(
        warning = function(cnd) {
          if ((grepl('attribute variables are assumed to be spatially constant', cnd$message))) {
            rlang::cnd_muffle(cnd)
          }
        },
        sf::st_intersection(from_poly, to_poly)
      )
    }

    # calculate area and drop geometry
    fromto_pair$area <- as.numeric(sf::st_area(fromto_pair))
    fromto_pair <- sf::st_drop_geometry(fromto_pair)
  }

  # join the data back on from the relevant polyIDs
  fromto_data <- from_data |>
    dplyr::left_join(fromto_pair, by = "polyID_f", relationship = "many-to-many")|>
    dplyr::left_join(to_data, by = "polyID_t", relationship = "many-to-many") |>
    dplyr::select(tidyselect::everything(), polyID = "polyID_t", -"polyID_f")

  return(fromto_data)
}


#' Join two spatial dataframes non-spatially
#'
#' This is really just a left join that bypasses the geometry, and has some
#' checks.
#'
#' @param from_geo `sf` of the input data to be aggregated
#' @param to_geo `sf` to aggregate into
#' @param prefix from spatial_aggregate, only used in error-checking
#'
#' @return an sf on the scale of to_geo
#'
#' @keywords internal
pseudo_spatial_joiner <- function(from_geo, to_geo, prefix) {
  from_geo <- from_geo |> dplyr::select(-tidyselect::any_of("polyID"))

  # There are some situations where the join isnt' present. Normally, this
  # returns NA for unmatched columns, which should be expected behaviour, but we
  # should warn about it, especially because it yields NA geometries.
  commonnames <- intersect(names(from_geo), names(to_geo))
  commonnames <- commonnames[!commonnames %in% c("polyID", "geometry")]

  for (i in 1:length(commonnames)) {
    uniquefrom_geo <- from_geo |>
      sf::st_drop_geometry() |>
      dplyr::select(tidyselect::all_of(commonnames[i])) |>
      dplyr::distinct() |>
      dplyr::pull()

    uniqueto <- to_geo |>
      sf::st_drop_geometry() |>
      dplyr::select(tidyselect::all_of(commonnames[i])) |>
      dplyr::distinct() |>
      dplyr::pull()

    # this is one-way; check anything in from_geo is going to be lost. There can
    # easily be things in to_geo that don't have anything fed to them.
    missing_to <- setdiff(uniquefrom_geo, uniqueto)

    if (length(missing_to) > 0) {
      rlang::warn(c(
        "Missing matches in join",
        "!" = glue::glue("{missing_to} is present in {commonnames} of the input from_geo, but is not present in the to_geo being joined (likely {prefix})"),
        "i" = "This will yield NA for joined columns (as it should), but because the from_geo is spatial, this can cause later issues due to lacking geometry.",
        "i" = "Because this is expected behaviour from a join, we leave it to the user to decide to pre-drop this from_geo, expand the joining from_geo set to include the needed links, or otherwise address it."
      ))
    }
  }

  commonnames <- names(from_geo)[names(from_geo) %in% names(to_geo)]
  commonnames <- commonnames[commonnames != 'geometry']
  fromto_pair <- dplyr::left_join(
    sf::st_drop_geometry(from_geo),
    sf::st_drop_geometry(to_geo),
    by = commonnames
  )

  return(fromto_pair)
}
