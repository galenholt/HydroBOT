gauges <- get_ewr_table()
gauges <- gauges |>
  dplyr::select(gauge = Gauge) |>
  dplyr::distinct()

test_that("csv read-in works", {
  # data-raw only exists in dev, not build.
  gaugecsv <- system.file('data-raw/bom_gauge_locations/bom_gauge_data.csv', package = 'HydroBOT')
  skip_if_no_file(gaugecsv)
  spatialewr <- join_to_geo(gauges,
                          spatial_locs = gaugecsv)
  expect_s3_class(spatialewr, 'sf')
})

test_that("shp read-in works", {
  gaugeshp <- system.file('data-raw/cleanspatial/bom_gauges.shp', package = 'HydroBOT')
  skip_if_no_file(gaugeshp)
  spatialewr <- join_to_geo(gauges,
                          spatial_locs = gaugeshp)
  expect_s3_class(spatialewr, 'sf')
})

test_that("sf direct works", {
  spatialewr <- join_to_geo(gauges,
                          spatial_locs = bom_basin_gauges)
  expect_s3_class(spatialewr, 'sf')
})
