test_that("csv read-in works", {
  # data-raw only exists in dev, not build.
  gaugecsv <- system.file("data-raw/bom_gauge_locations/bom_gauge_data.csv", package = "werptoolkitr")
  skip_if_no_file(gaugecsv)
  print(Sys.getenv())
  spatialewr <- gauge2geo(summary_ewr_output,
    gaugelocs = gaugecsv
  )
  expect_s3_class(spatialewr, "sf")
})

test_that("shp read-in works", {
  spatialewr <- gauge2geo(summary_ewr_output,
    gaugelocs = system.file("extdata/bom_gauges.shp", package = "werptoolkitr")
  )
  expect_s3_class(spatialewr, "sf")
})

test_that("sf direct works", {
  spatialewr <- gauge2geo(summary_ewr_output,
    gaugelocs = bom_basin_gauges
  )
  expect_s3_class(spatialewr, "sf")
})
