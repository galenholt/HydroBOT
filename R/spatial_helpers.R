# Helpers ------------------------------------------------------------------

# st_intersection is super slow with complex polygons. Let's at least warn and arrange the order
vertcount <- function(polyg) {
  verts <- polyg |>
    sf::st_geometry() |>
    sf::st_cast("MULTIPOINT") |>
    sapply(length) |>
    # purrr::map_dbl(length) |>
    sum()
}

# Prep the to polygons
add_polyID <- function(geosf, failduplicate = TRUE) {
  # Make unique IDs for  the polygons being aggregated into. They probably have
  # something unique, but this ensures it rather than assumes, and gives it a
  # standard name
  geosf <- geosf |>
    dplyr::mutate(polyID = lwgeom::st_geohash(.data$geometry, precision = 11))

  # Check
  # I could throw this in a while loop and increase precision, but if they
  # aren't unique at a precision of 11, it's possible they're identical and so
  # the loop would go infinite
  # Could also probably do something with st_equals to check that, I guess. But
  # I don't have a good test case handy
  if (failduplicate & any(duplicated(geosf$polyID))) {
    stop('polygons not unique at a precision of 11')
  }

  return(geosf)
}

# crs cleanup
crs_clean <- function(geo, whichcrs) {
  # Turn numbers for the crs into real crss
  if (is.numeric(whichcrs)) {
    whichcrs <- sf::st_crs(whichcrs)
  }

  if (sf::st_crs(geo) != whichcrs) {
    geo <- sf::st_transform(geo, whichcrs)
  }

  return(geo)
}


