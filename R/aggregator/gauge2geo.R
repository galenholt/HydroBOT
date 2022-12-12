#' Pairs gauge numbers with geographic locations to make an `sf`
#'
#'Typically used for EWR outputs, but anything with a `gauge` column with gauge numbers as characters will work.
#'
#' @param gaugedf A tibble or dataframe with a `gauge` column with gauge numbers as characters 
#' @param gaugelocfile Path to the csv with gauge numbers paired with lat-long. Currently assumes the BOM gauge file.
#' @param whichcrs desired crs of the output. input crs assumed to be WGS84, EPSG 4326
#'
#' @return an `sf` dataframe with gauge locations as points in the geometry
#' @export
#'
#' @examples
gauge2geo <- function(gaugedf, gaugelocfile, whichcrs = 4283) {
  # assume since lat/long this is wgs84 but could be some other geodetic datum
  # the parsing failure warnings are because of the undefineds that i then remove
  gaugelocs <- read_csv(gaugelocfile) %>% 
    rename(site = 'site name', gauge = 'gauge number') %>%
    # lat an long come in as chr because there is a line for 'undefined'
    filter(site != 'undefined') %>%
    mutate(lat = as.numeric(lat),
           lon = as.numeric(lon)) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326)
  
  gaugedf <- left_join(gaugedf, gaugelocs, by = 'gauge') %>% 
    st_as_sf() %>% 
    st_transform(whichcrs)
}

