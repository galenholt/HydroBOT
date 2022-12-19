#' Joins spatial from and to `sf` objects with needed data prep for the
#' aggregate functions
#'
#' This ensures the polygons have the same crs, are valid, optimises the
#' `st_intersection`, calculates an `area` column for future use, and drops
#' geometry while retaining the `polyID` from the `from_geo` as a grouper for
#' further aggregation and later re-joining to the geometry
#'
#' @param from_geo `sf` of the input data to be aggregated
#' @param to_geo `sf` of the polygons or multipolygons to aggregate into
#' @param whichcrs desired crs. Either the numeric EPSG or a crs definition, as
#'   from `st_crs()`
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
#' @export
#'
#' @examples
spatial_joiner <- function(from_geo, to_geo, whichcrs) {
  
  if (!('polyID' %in% names(from_geo))) {
    # we expect there may be duplicates here
    from_geo <- from_geo %>% 
      add_polyID(failduplicate = FALSE)
  }
  
  from_poly <- from_geo %>% 
    dplyr::select(polyID_f = polyID) %>% 
    dplyr::group_by(polyID_f) %>% 
    slice(1) %>% # usual use of dplyr::distinct() checks the polys factorially. slice just indexes.
    dplyr::ungroup() %>% 
    # Do the cleaning here to avoid duplication
    crs_clean(whichcrs) %>% 
    st_make_valid()
  
  to_poly <- to_geo  %>% 
    dplyr::select(polyID_t = polyID) %>% 
    dplyr::group_by(polyID_t) %>% 
    slice(1) %>% # usual use of dplyr::distinct() checks the polys factorially. slice just indexes.
    dplyr::ungroup()
  
  from_data <- st_drop_geometry(from_geo) %>% 
    dplyr::rename(polyID_f = polyID)
  to_data <- st_drop_geometry(to_geo) %>% 
    dplyr::rename(polyID_t = polyID)
  
  
  # This conditional is annoying, and st_intersection works for points, but
  # it's much slower. And this also lets us auto-calculate area only when needed
  # (e.g. when the input scale *has* area)
  if (all(st_is(from_geo, 'POINT'))) {
    fromto_pair <- st_join(from_poly, to_poly) %>%
      st_drop_geometry()
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
    if (toverts <= fromverts) {
      fromto_pair <- st_intersection(to_poly, from_poly)
    } else {
      fromto_pair <- st_intersection(from_poly, to_poly)
    }
    
    # calculate area and drop geometry
    fromto_pair <- fromto_pair %>%
      dplyr::mutate(area = as.numeric(st_area(.))) %>% 
      st_drop_geometry()
  }
    
    # join the data back on from the relevant polyIDs
    fromto_data <- from_data %>% 
      dplyr::left_join(fromto_pair, by = 'polyID_f') %>% 
      dplyr::left_join(to_data, by = 'polyID_t') %>% 
      dplyr::select(everything(), polyID = polyID_t, -polyID_f)
  
  return(fromto_data)
}
  
