test_that("summary works", {
  ewrpath <- system.file("extdata/testsmall/module_output/EWR", package = 'werptoolkitr')
  sumdat <- get_ewr_output(ewrpath, type = 'summary')
  # The names as they exist
  namestring <- c('scenario_path', 'scenario', 'gauge', 'planning_unit',
                  'multigauge', 'ewr_code', 'ewr_code_timing', 'event_years',
                  'frequency', 'target_frequency', 'achievement_count',
                  'achievement_per_year', 'event_count', 'event_count_all',
                  'events_per_year', 'events_per_year_all',
                  'average_event_length', 'threshold_days',
                  'max_inter_event_years', 'no_data_days', 'total_days',
                  'ewr_achieved')
  expect_equal(names(sumdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(sumdat$scenario))
  expect_true(is.character(sumdat$gauge))
  expect_true(is.logical(sumdat$ewr_achieved))
})

test_that("annual works", {
  # Annual currently *doesn't* work from the EWR tool, so can't write or run this.
  # ewrpath <- system.file("extdata/testsmall/module_output/EWR", package = 'werptoolkitr')
  # andat <- get_ewr_output(ewrpath, type = 'annual')
  # # The names as they exist
  # namestring <- c()
  # expect_equal(names(andat), namestring)
  # # a couple critical checks of the bits we use
  # expect_true(is.character(andat$scenario))
  # expect_true(is.character(andat$gauge))
})
