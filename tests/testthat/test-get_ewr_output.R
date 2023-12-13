
temp_parent_dir <- '_test_data'
# create dir so building makes sense
make_temp_hydro()

# all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

ewroutlist <- list('summary',
                   'yearly',
                   'all_events',
                   'all_successful_events',
                   # 'all_interEvents',
                   'all_successful_interEvents')

ewr_out <- prep_run_save_ewrs(hydro_dir = file.path(temp_parent_dir, 'hydrographs'),
                              output_parent_dir = temp_parent_dir,
                              outputType = ewroutlist,
                              datesuffix = FALSE,
                              returnType = ewroutlist)

# The path to those
ewrpath <- file.path(temp_parent_dir, 'module_output', 'EWR')

test_that("summary works", {


  sumdat <- get_any_ewr_output(ewrpath, type = 'summary')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit',
                  'ewr_code', 'ewr_code_timing', 'multigauge', 'event_years',
                  'frequency', 'target_frequency', 'achievement_count',
                  'achievement_per_year', 'event_count', 'event_count_all',
                  'events_per_year', 'events_per_year_all',
                  'average_event_length', 'threshold_days',
                  'max_inter_event_years', 'no_data_days', 'total_days')
  expect_equal(names(sumdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(sumdat$scenario))
  expect_true(is.character(sumdat$gauge))
})

test_that("yearly works", {
  ewrdat <- get_any_ewr_output(ewrpath, type = 'yearly')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('year', 'event_years', 'num_achieved', 'num_events',
                  'num_events_all', 'event_length', 'event_length_achieved',
                  'total_event_days', 'total_event_days_achieved',
                  'max_event_days', 'max_rolling_events', 'max_rolling_achievement',
                  'missing_days', 'total_possible_days', 'ewr_code',
                  'ewr_code_timing', 'scenario', 'gauge', 'planning_unit', 'multigauge',
                  'rolling_max_inter_event', 'rolling_max_inter_event_achieved')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_events works", {
  ewrdat <- get_any_ewr_output(ewrpath, type = 'all_events')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit', 'ewr_code', 'ewr_code_timing',
                  'water_year', 'start_date', 'end_date', 'event_duration',
                  'event_length', 'multigauge')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_successful_events works", {
  ewrdat <- get_any_ewr_output(ewrpath, type = 'all_successful_events')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit', 'ewr_code', 'ewr_code_timing',
                  'water_year', 'start_date', 'end_date', 'event_duration',
                  'event_length', 'multigauge')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_successful_interEvents works", {
  ewrdat <- get_any_ewr_output(ewrpath, type = 'all_successful_interEvents')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit', 'ewr_code', 'ewr_code_timing',
                  'start_date', 'end_date', 'inter_event_length')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})


test_that("assessment works", {
  yeardat <- get_any_ewr_output(ewrpath, type = 'yearly')
  sumdat <- get_any_ewr_output(ewrpath, type = 'summary')

  assessed <- assess_ewr_achievement(yeardat, sumdat)

  expect_equal(names(assessed), c('ewr_code', 'ewr_code_timing', 'gauge',
                                  'scenario', 'planning_unit', 'ewr_achieved', 'ewr_achieved_timeframe'))
})



test_that("making assessment tibble works", {
  assessed <- get_ewr_output(ewrpath)

  expect_equal(names(assessed), c('ewr_code', 'ewr_code_timing', 'gauge',
                                  'scenario', 'planning_unit', 'ewr_achieved', 'ewr_achieved_timeframe'))
})

test_that("passing in a list from memory works", {
  ewr_out <- make_test_ewr_output()

  ewrprepped <- get_ewr_output(ewr_out, type = 'achievement')
})
