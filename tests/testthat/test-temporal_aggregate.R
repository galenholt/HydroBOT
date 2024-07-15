ewr_to_agg <- make_test_ewr_prepped()

test_that("full time works, nongeom", {
  # no need to load the demo/test data since it's in /data
  agged <- temporal_aggregate(ewr_to_agg |> sf::st_drop_geometry(),
                           time_groups = 'all',
                           groupers = c('scenario', 'gauge', 'ewr_code'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           auto_ewr_PU = TRUE)


  expect_equal(names(agged), c('scenario', 'gauge', 'ewr_code', 'planning_unit_name', 'temporal_mean_ewr_achieved'))
  expect_s3_class(agged, 'data.frame')
  expect_equal(nrow(agged), 356)
})


test_that("time intervals work, nongeom", {
  time_breaks <- c('2014-01-01', '2016-01-01', '2018-01-01', '2020-01-01')
  tg <- lubridate::ymd(time_breaks)

  agged <- temporal_aggregate(ewr_to_agg |> sf::st_drop_geometry(),
                              breaks = tg,
                              groupers = c('scenario', 'gauge', 'ewr_code'),
                              aggCols = 'ewr_achieved',
                              funlist = 'mean',
                              auto_ewr_PU = TRUE)


  expect_equal(names(agged), c('scenario', 'gauge', 'ewr_code', 'planning_unit_name', 'temporal_mean_ewr_achieved', 'date'))
  expect_s3_class(agged, 'data.frame')
  expect_equal(nrow(agged), 890)
  expect_equal(sum(is.na(agged$date)), 89)

  expect_snapshot_value(unique(agged$date), style = 'deparse')

})

test_that("both above, sf", {
  # no need to load the demo/test data since it's in /data
  agged_sf_all <- temporal_aggregate(ewr_to_agg,
                              time_groups = 'all',
                              groupers = c('scenario', 'gauge', 'ewr_code'),
                              aggCols = 'ewr_achieved',
                              funlist = 'mean',
                              auto_ewr_PU = TRUE)


  expect_equal(names(agged_sf_all), c('scenario', 'gauge', 'ewr_code', 'polyID', 'planning_unit_name', 'temporal_mean_ewr_achieved', 'geometry'))
  expect_s3_class(agged_sf_all, 'sf')
  expect_equal(nrow(agged_sf_all), 356)

  time_breaks <- c('2014-01-01', '2016-01-01', '2018-01-01', '2020-01-01')
  tg <- lubridate::ymd(time_breaks)

  agged_sf_t <- temporal_aggregate(ewr_to_agg,
                              breaks = tg,
                              groupers = c('scenario', 'gauge', 'ewr_code'),
                              aggCols = 'ewr_achieved',
                              funlist = 'mean',
                              auto_ewr_PU = TRUE)


  expect_equal(names(agged_sf_t), c('scenario', 'gauge', 'ewr_code', 'polyID', 'planning_unit_name', 'temporal_mean_ewr_achieved', 'date', 'geometry'))
  expect_s3_class(agged_sf_t, 'sf')
  expect_equal(nrow(agged_sf_t), 890)
  expect_equal(sum(is.na(agged_sf_t$date)), 89)

  expect_snapshot_value(unique(agged_sf_t$date), style = 'deparse')

})

