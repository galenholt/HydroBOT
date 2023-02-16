test_that("ewr-obj works, nongeom", {
  # no need to load the demo/test data since it's in /data
  agged <- theme_aggregate(summary_ewr_output,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = make_edges(causal_ewr, list(c('ewr_code_timing', 'ewr_code'))))
  expect_equal(names(agged), c('scenario', 'gauge', 'ewr_code', 'ewr_code_mean_ewr_achieved'))
  expect_s3_class(agged, 'data.frame')
})

test_that("auto-generating causal_edges works", {
  agged <- theme_aggregate(summary_ewr_output,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = causal_ewr)
  expect_equal(names(agged), c('scenario', 'gauge', 'ewr_code', 'ewr_code_mean_ewr_achieved'))
  expect_s3_class(agged, 'data.frame')
})

test_that("spatial input data works", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)
  agged <- theme_aggregate(sumspat,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = causal_ewr)
  expect_equal(names(agged), c('scenario', 'gauge', 'polyID',
                               'ewr_code', 'ewr_code_mean_ewr_achieved',
                               'geometry'))
  expect_s3_class(agged, 'data.frame')
  expect_s3_class(agged, 'sf')

  # Not usually how geonames will be used, but it does work as a test
  agged <- theme_aggregate(sumspat,
                           from_theme = 'ewr_code_timing',
                           to_theme = 'ewr_code',
                           groupers = c('scenario', 'gauge'),
                           aggCols = 'ewr_achieved',
                           funlist = 'mean',
                           causal_edges = causal_ewr,
                           geonames = 'site')
  expect_equal(names(agged), c('scenario', 'gauge', 'polyID',
                               'ewr_code', 'ewr_code_mean_ewr_achieved',
                               'site',
                               'geometry'))
  expect_s3_class(agged, 'data.frame')
  expect_s3_class(agged, 'sf')
})


# different arguments (bar, char, tidyselect) ----------------------------- I
# think do these *after* I make sure spatial_ and multi_ (and maybe run_and_agg) are working, so I can
# actually test across everything

# test_that("tidyselect groupers and aggcols works", {
#   agged <- theme_aggregate(summary_ewr_output,
#                            from_theme = 'ewr_code_timing',
#                            to_theme = 'ewr_code',
#                            groupers = tidyselect::any_of(c('scenario', 'gauge')),
#                            aggCols = tidyselect::contains('achieved'),
#                            funlist = 'mean',
#                            causal_edges = causal_ewr)
#   expect_equal(names(agged), c('scenario', 'gauge', 'ewr_code', 'ewr_code_mean_ewr_achieved'))
#   expect_s3_class(agged, 'data.frame')
# })


# todo --------------------------------------------------------------------

# bare names groupers and aggcols
# funlist as list and bare
# multiple aggcols
# multiple functions
# ...

