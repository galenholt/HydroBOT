
temp_parent_dir <- '_test_data'
# create dir so building makes sense
make_temp_hydro()

# all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

ewroutlist <- list('summary',
                   'yearly',
                   'all_events',
                   'all_successful_events',
                   'all_interEvents',
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
  namestring <- c('scenario', 'gauge', 'planning_unit_name',
                  'ewr_code', 'multigauge', 'event_years',
                  'frequency', 'target_frequency', 'achievement_count',
                  'achievement_per_year', 'event_count', 'event_count_all',
                  'events_per_year', 'events_per_year_all',
                  'average_event_length', 'threshold_days',
                  'max_inter_event_years', 'no_data_days', 'total_days', 'ewr_code_timing')
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
                  'scenario', 'gauge', 'planning_unit_name', 'multigauge',
                  'rolling_max_inter_event', 'rolling_max_inter_event_achieved', 'ewr_code_timing')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_events works", {
  ewrdat <- get_any_ewr_output(ewrpath, type = 'all_events')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit_name', 'ewr_code',
                  'water_year', 'start_date', 'end_date', 'event_duration',
                  'event_length', 'multigauge', 'ewr_code_timing')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_successful_events works", {
  ewrdat <- get_any_ewr_output(ewrpath, type = 'all_successful_events')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit_name', 'ewr_code',
                  'water_year', 'start_date', 'end_date', 'event_duration',
                  'event_length', 'multigauge', 'ewr_code_timing')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("all_successful_interEvents works", {
  ewrdat <- get_any_ewr_output(ewrpath, type = 'all_successful_interEvents')
  # The names as they exist
  # stringr::str_flatten(names(sumdat), "', '")
  namestring <- c('scenario', 'gauge', 'planning_unit_name', 'ewr_code',
                  'start_date', 'end_date', 'inter_event_length', 'ewr_code_timing')
  expect_equal(names(ewrdat), namestring)
  # a couple critical checks of the bits we use
  expect_true(is.character(ewrdat$scenario))
  expect_true(is.character(ewrdat$gauge))
})

test_that("assessment works", {
  yeardat <- get_any_ewr_output(ewrpath, type = 'yearly')
  yeardat <- clean_yearly(yeardat)

  # use a 3-year roll since the data only has 5 years
  assessed <- assess_ewr_achievement(yeardat, year_roll = 3)

  expect_equal(names(assessed), c('scenario', 'year', 'date', 'gauge',
                                  'planning_unit_name', 'ewr_code',
                                  'ewr_code_timing', 'event_years', 'ewr_achieved', 'interevent_achieved'))
})


test_that("making assessment tibble works", {
  assessed <- get_ewr_output(ewrpath)

  expect_equal(names(assessed), c('scenario', 'year', 'date', 'gauge',
                                  'planning_unit_name', 'ewr_code',
                                  'ewr_code_timing', 'event_years', 'ewr_achieved', 'interevent_achieved'))
  expect_equal(sum(is.na(assessed$planning_unit_name)), 0)
})

test_that("year_roll is rolling correctly", {
  # default is 1 if < 10, so have to set manually for the test data
  assessed <- get_ewr_output(ewrpath, year_roll = 3)

  expect_equal(names(assessed), c('scenario', 'year', 'date', 'gauge',
                                  'planning_unit_name', 'ewr_code',
                                  'ewr_code_timing', 'event_years', 'ewr_achieved', 'interevent_achieved'))
  expect_equal(sum(is.na(assessed$ewr_achieved)), 882)
})


test_that("passing in a list from memory works", {
  # ewr_out <- make_test_ewr_output(build_dirs = FALSE)

  ewrprepped <- get_ewr_output(ewr_out, type = 'achievement')
  expect_equal(names(ewrprepped), c('scenario', 'year', 'date', 'gauge',
                                    'planning_unit_name', 'ewr_code',
                                    'ewr_code_timing', 'event_years', 'ewr_achieved', 'interevent_achieved'))
  expect_equal(sum(is.na(ewrprepped$planning_unit_name)), 0)
})

test_that("ewr_code separation works", {
  # The whole EWR table (simplified down)
  et <- get_ewr_table() |>
    tibble::tibble() |>
    dplyr::mutate(ewr_code = Code) |>
    dplyr::select(ewr_code) |>
    dplyr::distinct()

  ewrsep <- separate_ewr_codes(et)

  ues <- unique(ewrsep$ewr_code)

  expect_snapshot_value(as.list(ues))



  # causal straight from data-raw. This isn't ideal and might have to be skipped because it wont be available to data-raw
  rawcausalpath <- 'data-raw/causal_networks/ewr_obj_codes_nsw/obj_codes_dec22.csv'
  skip_if_no_file(rawcausalpath)
    causalewrs <- readr::read_csv(rawcausalpath, show_col_types = FALSE)  |>
      dplyr::rename(ewr_code = EWR) |>
      dplyr::select(ewr_code) |>
      dplyr::distinct()

    causalsep <- separate_ewr_codes(causalewrs)
    ucs <- unique(causalsep$ewr_code)

    ucs[!ucs %in% ues]
    expect_snapshot_value(as.list(ucs))
})

test_that("making MAX scenario works", {
  assessed <- get_ewr_output(ewrpath)|>
    dplyr::filter(scenario == "MAX")
  expect_equal(nrow(assessed)> 0,TRUE)
})

test_that("roll_frequency rolls correctly", {
  a <- c(0,1,1,0,1,1,1,1,0,0,1)
 rolled <- roll_frequency(a, year_roll = 3)
 an <- c(0,1,1,0,1,NA,1,1,0,0,1)
 rolled_na <- roll_frequency(an, year_roll = 3)
 rolled_nap <- roll_frequency(an, year_roll = 3, pad_initial = TRUE)
 expect_snapshot_value(rolled, style = 'deparse')
 expect_snapshot_value(rolled_na, style = 'deparse')
 expect_snapshot_value(rolled_nap, style = 'deparse')


})

