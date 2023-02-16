test_that("csv read-in works", {
  spatialewr <- gauge2geo(summary_ewr_output,
                          gaugelocs = system.file('data-raw/bom_gauge_locations/bom_gauge_data.csv', package = 'werptoolkitr'))
  expect_s3_class(spatialewr, 'sf')
})

test_that("shp read-in works", {
  spatialewr <- gauge2geo(summary_ewr_output,
                          gaugelocs = system.file('extdata/bom_gauges.shp', package = 'werptoolkitr'))
  expect_s3_class(spatialewr, 'sf')
})

test_that("sf direct works", {
  spatialewr <- gauge2geo(summary_ewr_output,
                          gaugelocs = bom_basin_gauges)
  expect_s3_class(spatialewr, 'sf')
})
