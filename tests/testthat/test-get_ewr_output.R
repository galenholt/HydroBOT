proj_dir <- system.file("extdata/testsmall", package = 'werptoolkitr')
hydro_dir <- system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')

temp_hydro_dir = '_test_data/temp_one/hydrographs'
temp_parent_dir = '_test_data/temp_one'

temp_hydro_multi = '_test_data/temp_multi/hydrographs'

# Make sure the test dirs are blank
destroy_temp_hydro(temp_parent_dir)

# build a test set
# create dir so building makes sense
make_temp_hydro(temp_hydro_dir)

# all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

ewroutlist <- list('summary',
                   'yearly',
                   'all_events',
                   'all_successful_events',
                   # 'all_interEvents',
                   'all_successful_interEvents')

ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                              output_parent_dir = temp_parent_dir,
                              outputType = ewroutlist,
                              datesuffix = FALSE,
                              returnType = ewroutlist)

# The path to those
ewrpath <- file.path(temp_parent_dir, 'module_output', 'EWR')

test_that("summary works", {


  sumdat <- get_ewr_output(ewrpath, type = 'summary')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit',
                  'ewr_code', 'ewr_code_timing', 'multigauge', 'event_years',
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

test_that("yearly works", {
  ewrdat <- get_ewr_output(ewrpath, type = 'yearly')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit', 'ewr_code',
                  'ewr_code_timing', 'multigauge', 'event_years', 'frequency',
                  'target_frequency', 'achievement_count', 'achievement_per_year',
                  'event_count', 'event_count_all', 'events_per_year',
                  'events_per_year_all', 'average_event_length', 'threshold_days',
                  'max_inter_event_years', 'no_data_days', 'total_days')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_events works", {
  ewrdat <- get_ewr_output(ewrpath, type = 'all_events')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'pu', 'ewr_code', 'ewr_code_timing',
                  'water_year', 'start_date', 'end_date', 'event_duration',
                  'event_length', 'multigauge')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_successful_events works", {
  ewrdat <- get_ewr_output(ewrpath, type = 'all_successful_events')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'pu', 'ewr_code', 'ewr_code_timing',
                  'water_year', 'start_date', 'end_date', 'event_duration',
                  'event_length', 'multigauge')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_successful_interEvents works", {
  ewrdat <- get_ewr_output(ewrpath, type = 'all_successful_interEvents')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'pu', 'ewr_code', 'ewr_code_timing',
                  'start_date', 'end_date', 'inter_event_length')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})
