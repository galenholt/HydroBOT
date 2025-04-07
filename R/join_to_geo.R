#' Pairs gauge numbers with geographic locations to make an `sf`
#'
#'Typically used for EWR outputs, but anything with a `gauge` column with gauge numbers as characters will work.
#'
#' @param data_df A tibble or dataframe with a `gauge` column with gauge numbers as characters
#' @param spatial_locs sf or path to the csv with gauge numbers paired with lat-long.
#' @param whichcrs desired crs of the output. input crs assumed to be WGS84, EPSG 4326
#'
#' @return an `sf` dataframe with gauge locations as points in the geometry
#' @keywords internal
#'
join_to_geo <- function(data_df, spatial_locs, whichcrs = 4283) {
  # assume since lat/long this is wgs84 but could be some other geodetic datum
  # the parsing failure warnings are because of the undefineds that i then remove

  # Get filetype if needed to read-in
  if(!inherits(spatial_locs, 'sf')) {
    if (grepl('*.csv', spatial_locs)) {filetype <- 'csv'}
    if (grepl('*.shp', spatial_locs)) {filetype <- 'shp'}
    if (!grepl('\\.', spatial_locs)) {filetype <- 'object'}

    if (filetype == 'csv') {
      spatial_locs <- readr::read_csv(spatial_locs)

      # In general, we assume the user prepares their data, but this is specific to the BOM gauges.
      if ('site name' %in% names(spatial_locs)) {
        spatial_locs <- spatial_locs |>
          dplyr::rename(site = 'site name', gauge = 'gauge number') |>
          # lat an long come in as chr because there is a line for 'undefined'
          dplyr::filter(.data$site != 'undefined') |>
          dplyr::mutate(lat = as.numeric(.data$lat),
                        lon = as.numeric(.data$lon)) |>
          sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326)
        }

    }

    if (filetype == 'shp') {
      spatial_locs <- sf::read_sf(spatial_locs)
    }

    if (filetype == 'object') {
      spatial_locs <- get(spatial_locs)
    }

  }

  commonnames <- names(data_df)[names(data_df) %in% names(spatial_locs)]
  data_df <- dplyr::left_join(data_df, spatial_locs, by = commonnames) |>
    sf::st_as_sf() |>
    sf::st_transform(whichcrs)

  return(data_df)
}

