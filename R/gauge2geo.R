#' Pairs gauge numbers with geographic locations to make an `sf`
#'
#'Typically used for EWR outputs, but anything with a `gauge` column with gauge numbers as characters will work.
#'
#' @param gaugedf A tibble or dataframe with a `gauge` column with gauge numbers as characters
#' @param gaugelocs sf or path to the csv with gauge numbers paired with lat-long.
#' @param whichcrs desired crs of the output. input crs assumed to be WGS84, EPSG 4326
#'
#' @return an `sf` dataframe with gauge locations as points in the geometry
#' @export
#'
gauge2geo <- function(gaugedf, gaugelocs, whichcrs = 4283) {
  # assume since lat/long this is wgs84 but could be some other geodetic datum
  # the parsing failure warnings are because of the undefineds that i then remove

  # Get filetype if needed to read-in
  if(!inherits(gaugelocs, 'sf')) {
    if (grepl('*.csv', gaugelocs)) {filetype <- 'csv'}
    if (grepl('*.shp', gaugelocs)) {filetype <- 'shp'}
    if (!grepl('\\.', gaugelocs)) {filetype <- 'object'}

    if (filetype == 'csv') {
      gaugelocs <- readr::read_csv(gaugelocs) |>
        dplyr::rename(site = 'site name', gauge = 'gauge number') |>
        # lat an long come in as chr because there is a line for 'undefined'
        dplyr::filter(.data$site != 'undefined') |>
        dplyr::mutate(lat = as.numeric(.data$lat),
                      lon = as.numeric(.data$lon)) |>
        sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326)
    }

    if (filetype == 'shp') {
      gaugelocs <- sf::read_sf(gaugelocs)
    }

    if (filetype == 'object') {
      gaugelocs <- get(gaugelocs)
    }

  }

  # # handle either an sf of gauge locations or a character filepath.
  # if (!inherits(gaugelocs, 'sf')) {
  #   if (grepl('*.csv', gaugelocs)) {
  #     gaugelocs <- readr::read_csv(gaugelocs) |>
  #       dplyr::rename(site = 'site name', gauge = 'gauge number') |>
  #       # lat an long come in as chr because there is a line for 'undefined'
  #       dplyr::filter(.data$site != 'undefined') |>
  #       dplyr::mutate(lat = as.numeric(.data$lat),
  #                     lon = as.numeric(.data$lon)) |>
  #       sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326)
  #   }
  #
  #   if (grepl('*.shp', gaugelocs)) {
  #     gaugelocs <- sf::read_sf(gaugelocs)
  #   }
  # }

  gaugedf <- dplyr::left_join(gaugedf, gaugelocs, by = 'gauge') |>
    sf::st_as_sf() |>
    sf::st_transform(whichcrs)
}

