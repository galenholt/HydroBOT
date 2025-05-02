
rlang::local_options(lifecycle_verbosity = "error")

temp_parent_dir <- '_test_data'
# create dir so building makes sense
make_temp_hydro()


ewroutlist <- list('summary',
                   'yearly',
                   'all_events',
                   'all_successful_events',
                   'all_interEvents',
                   'all_successful_interEvents')

ewr_out <- prep_run_save_ewrs(hydro_dir = file.path(temp_parent_dir,
                                                    'hydrographs'),
                              output_parent_dir = temp_parent_dir,
                              outputType = ewroutlist,
                              returnType = ewroutlist)

# The path to those
ewrpath <- file.path(temp_parent_dir, 'module_output', 'EWR')

test_that("assessment works", {
  yeardat <- get_module_output(ewrpath, type = 'yearly')
  yeardat <- cleanewrs(yeardat)
  yeardat <- clean_ewr_yearly(yeardat)

  # use a 3-year roll since the data only has 5 years
  assessed <- assess_ewr_achievement(yeardat, year_roll = 3)

  expect_equal(names(assessed), c('scenario', 'year', 'date', 'gauge',
                                  'planning_unit_name', 'state', 'SWSDLName',
                                  'ewr_code',
                                  'ewr_code_timing', 'event_years',
                                  'frequency_achieved', 'interevent_achieved',
                                  'ewr_achieved'))
})


test_that("making assessment tibble works", {
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)
  assessed <- prep_ewr_output(datain)

  expect_equal(names(assessed), c('scenario', 'year', 'date', 'gauge',
                                  'planning_unit_name', 'state', 'SWSDLName',
                                  'ewr_code',
                                  'ewr_code_timing', 'event_years',
                                  'frequency_achieved', 'interevent_achieved',
                                  'ewr_achieved', 'geometry'))
  expect_equal(sum(is.na(assessed$planning_unit_name)), 0)
})

test_that("making assessment tibble from in-memory ewr output works", {
  # datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)
  assessed <- ewr_out$yearly |>
    prep_ewr_output()

  expect_equal(names(assessed), c('scenario', 'year', 'date', 'gauge',
                                  'planning_unit_name', 'state', 'SWSDLName',
                                  'ewr_code',
                                  'ewr_code_timing', 'event_years',
                                  'frequency_achieved', 'interevent_achieved',
                                  'ewr_achieved', 'geometry'))
  expect_equal(sum(is.na(assessed$planning_unit_name)), 0)
})

test_that("year_roll is rolling correctly", {
  # default is 1 if < 10, so have to set manually for the test data
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)

  assessed <- prep_ewr_output(datain, year_roll = 3)

  expect_equal(names(assessed), c('scenario', 'year', 'date', 'gauge',
                                  'planning_unit_name', 'state', 'SWSDLName',
                                  'ewr_code',
                                  'ewr_code_timing', 'event_years',
                                  'frequency_achieved', 'interevent_achieved',
                                  'ewr_achieved', 'geometry'))
  expect_equal(sum(is.na(assessed$ewr_achieved)), 1296)
})


test_that("passing in a list from memory works", {
  # ewr_out <- make_test_ewr_output(build_dirs = FALSE)
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)
  ewrprepped <- prep_ewr_output(datain, type = 'achievement')
  expect_equal(names(ewrprepped), c('scenario', 'year', 'date', 'gauge',
                                    'planning_unit_name', 'state', 'SWSDLName',
                                    'ewr_code',
                                    'ewr_code_timing', 'event_years',
                                    'frequency_achieved', 'interevent_achieved',
                                    'ewr_achieved', 'geometry'))
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
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)
  assessed <- prep_ewr_output(datain)|>
    dplyr::filter(scenario == "MAX")
  expect_equal(nrow(assessed)> 0,TRUE)
})

test_that("roll_frequency rolls correctly", {
  a <- c(0,1,1,0,1,1,1,1,0,0,1)
  rolled <- roll_frequency(a, year_roll = 3, na.rm = FALSE)
  an <- c(0,1,1,0,1,NA,1,1,0,0,1)
  rolled_na <- roll_frequency(an, year_roll = 3, na.rm = FALSE)
  rolled_narm <- roll_frequency(an, year_roll = 3, na.rm = TRUE)
  rolled_nap <- roll_frequency(an, year_roll = 3, pad_initial = TRUE, na.rm = TRUE)

  ani <- c(NA,1,1,0,1,NA,1,1,0,0,1)
  rolled_nai <- roll_frequency(ani, year_roll = 3, na.rm = FALSE)
  rolled_nairm <- roll_frequency(ani, year_roll = 3, na.rm = TRUE)

  expect_snapshot_value(rolled, style = 'deparse')
  expect_snapshot_value(rolled_na, style = 'deparse')
  expect_snapshot_value(rolled_nap, style = 'deparse')
  expect_snapshot_value(rolled_narm, style = 'deparse')
  expect_snapshot_value(rolled_nai, style = 'deparse')
  expect_snapshot_value(rolled_nairm, style = 'deparse')
})

test_that("interevents works", {
  # inter_success <- get_module_output(ewrpath, type = 'all_successful_interEvents')
  inter_all <- get_module_output(ewrpath, type = 'all_interEvents') |>
    cleanewrs()

  assessed <- assess_ewr_interevents(inter_all)

  expect_equal(names(assessed), c('scenario', 'gauge', 'planning_unit_name',
                                  'state', 'SWSDLName', 'ewr_code', 'start_date',
                                  'inter_event_length', 'ewr_code_timing',
                                  'max_interevent', 'exceedance_days',
                                  'interevent_ratio', 'exceedance_ratio',
                                  'exceedance', 'exceedance_only',
                                  'days_in_exceeding'))

  datain <- read_and_geo(ewrpath, type = 'all_interEvents',
                         geopath = bom_basin_gauges)
  fromtop <- prep_ewr_output(datain, type = 'interevents')

  expect_equal(names(fromtop), c('scenario', 'gauge', 'planning_unit_name',
                                  'state', 'SWSDLName', 'ewr_code', 'start_date',
                                  'inter_event_length', 'site', 'owner', 'geometry',
                                 'ewr_code_timing',
                                  'max_interevent', 'exceedance_days',
                                  'interevent_ratio', 'exceedance_ratio',
                                  'exceedance', 'exceedance_only',
                                  'days_in_exceeding'))
})
