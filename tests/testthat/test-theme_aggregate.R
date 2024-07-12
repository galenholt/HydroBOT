ewr_to_agg <- make_test_ewr_prepped()

test_that("ewr-obj works, nongeom", {
  # no need to load the demo/test data since it's in /data
  agged <- theme_aggregate(ewr_to_agg |> sf::st_drop_geometry(),
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = make_edges(causal_ewr, list(c('ewr_code_timing', 'ewr_code'))),
                           auto_ewr_PU = TRUE)
  expect_equal(names(agged), c('scenario', 'gauge', 'planning_unit_name', 'ewr_code', 'ewr_code_mean_ewr_achieved'))
  expect_s3_class(agged, 'data.frame')
})

test_that("auto-generating causal_edges works", {
  agged <- theme_aggregate(ewr_to_agg |> sf::st_drop_geometry(),
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = causal_ewr,
                           auto_ewr_PU = TRUE)
  expect_equal(names(agged), c('scenario', 'gauge', 'planning_unit_name', 'ewr_code', 'ewr_code_mean_ewr_achieved'))
  expect_s3_class(agged, 'data.frame')
})

test_that("spatial input data works", {
  agged <- theme_aggregate(ewr_to_agg,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = causal_ewr,
                           auto_ewr_PU = TRUE)

  expect_equal(names(agged), c('scenario', 'gauge', 'polyID', 'planning_unit_name',
                               'ewr_code', 'ewr_code_mean_ewr_achieved',
                               'geometry'))
  expect_s3_class(agged, 'data.frame')
  expect_s3_class(agged, 'sf')

  # Not usually how geonames will be used, but it does work as a test
  agged <- theme_aggregate(ewr_to_agg,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = causal_ewr,
                           geonames = 'site',
                           auto_ewr_PU = TRUE)
  expect_equal(names(agged), c('scenario', 'gauge', 'polyID', 'planning_unit_name',
                               'ewr_code', 'ewr_code_mean_ewr_achieved',
                               'site',
                               'geometry'))
  expect_s3_class(agged, 'data.frame')
  expect_s3_class(agged, 'sf')
})

test_that("bare functions", {
  agged <- theme_aggregate(ewr_to_agg,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = mean,
                           causal_edges = causal_ewr,
                           auto_ewr_PU = TRUE)
  expect_equal(names(agged), c('scenario', 'gauge', 'polyID', 'planning_unit_name',
                               'ewr_code', 'ewr_code_mean_ewr_achieved',
                               'geometry'))
  expect_s3_class(agged, 'data.frame')
})

test_that("list functions", {
  agged <- theme_aggregate(ewr_to_agg,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = list(mean = ~mean(., na.rm = TRUE)),
                           causal_edges = causal_ewr,
                           auto_ewr_PU = TRUE)
  expect_equal(names(agged), c('scenario', 'gauge', 'polyID', 'planning_unit_name',
                               'ewr_code', 'ewr_code_mean_ewr_achieved',
                               'geometry'))
  expect_s3_class(agged, 'data.frame')
})

test_that("multiple functions", {
  # Character
  agged_c <- theme_aggregate(ewr_to_agg,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = c('mean', 'sd'),
                           causal_edges = causal_ewr,
                           auto_ewr_PU = TRUE)
  expect_equal(names(agged_c), c('scenario', 'gauge', 'polyID', 'planning_unit_name',
                               'ewr_code',
                               'ewr_code_mean_ewr_achieved',
                               'ewr_code_sd_ewr_achieved',
                               'geometry'))
  expect_s3_class(agged_c, 'data.frame')

  # bare
  agged_b <- theme_aggregate(ewr_to_agg,
                             from_theme = 'ewr_code_timing',
                             to_theme = 'ewr_code',
                             groupers = c('scenario', 'gauge'),
                             aggCols = 'ewr_achieved',
                             funlist = c(mean, sd),
                             causal_edges = causal_ewr,
                             auto_ewr_PU = TRUE)
  expect_equal(names(agged_b), c('scenario', 'gauge', 'polyID', 'planning_unit_name',
                                 'ewr_code',
                                 'ewr_code_mean_ewr_achieved',
                                 'ewr_code_sd_ewr_achieved',
                                 'geometry'))
  expect_s3_class(agged_b, 'data.frame')

  # List
  agged_l <- theme_aggregate(ewr_to_agg,
                             from_theme = 'ewr_code_timing',
                             to_theme = 'ewr_code',
                             groupers = c('scenario', 'gauge'),
                             aggCols = 'ewr_achieved',
                             funlist = list(mean = ~mean(., na.rm = TRUE),
                                            sd = ~sd(., na.rm = TRUE)),
                             causal_edges = causal_ewr,
                             auto_ewr_PU = TRUE)
  expect_equal(names(agged_l), c('scenario', 'gauge', 'polyID', 'planning_unit_name',
                                 'ewr_code',
                                 'ewr_code_mean_ewr_achieved',
                                 'ewr_code_sd_ewr_achieved',
                                 'geometry'))
  expect_s3_class(agged_l, 'data.frame')
})

# different arguments (bar, char, tidyselect) ----------------------------- I
# think do these *after* I make sure spatial_ and multi_ (and maybe run_and_agg) are working, so I can
# actually test across everything

# test_that("tidyselect groupers and aggcols works", {
#   agged <- theme_aggregate(ewr_to_agg,
#                            from_theme = 'ewr_code_timing',
#                            to_theme = 'ewr_code',
#                            groupers = tidyselect::any_of(c('scenario', 'gauge')),
#                            aggCols = tidyselect::contains('achieved'),
#                            funlist = 'mean',
#                            causal_edges = causal_ewr)
#   expect_equal(names(agged), c('scenario', 'gauge', 'planning_unit_name', 'ewr_code', 'ewr_code_mean_ewr_achieved'))
#   expect_s3_class(agged, 'data.frame')
# })


# todo --------------------------------------------------------------------

# bare names groupers and aggcols
# multiple aggcols
# multiple groupers (e.g. year as in annual data)
# different theme levels- easier once we generate some test data
# ...

