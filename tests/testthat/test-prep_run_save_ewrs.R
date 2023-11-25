# Set up some base directory structures
temp_hydro_dir <- '_test_data/hydrographs'
temp_parent_dir <- '_test_data'

temp_hydro_multi <- '_test_data/hydrographs'
temp_parent_multi <- '_test_data'

set_future_multi()

test_that('returns one result, no saving', {

  # create dir so building makes sense
  make_temp_hydro()

  # Test it didn't create anything since outputType = 'none'
  start_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary'))

  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), 'summary')
  expect_equal(unique(ewr_out$summary$scenario),
               c('base_base', 'down4_down4', 'up4_up4'))
  # Test it didn't create anything since outputType = 'none'
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_equal(start_structure, realised_structure)
})

# test multiple returns- use the next one once EWR is debugged.
test_that('returns list', {
  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_true(all(c('summary','all_events') %in% names(ewr_out)))
  expect_equal(unique(ewr_out$summary$scenario),
               c('base_base', 'down4_down4', 'up4_up4'))


})

# Test complex directory scenario extraction.
test_that('complex dir structure', {
  # create dir so building makes sense
  make_temp_hydro()

  dir.create(file.path(temp_hydro_dir, 'S1'))
  dir.create(file.path(temp_hydro_dir, 'S2'))
  file.copy(file.path(temp_hydro_dir, 'base'), file.path(temp_hydro_dir, 'S1'), recursive = TRUE)
  file.copy(file.path(temp_hydro_dir, 'up4'), file.path(temp_hydro_dir, 'S2'), recursive = TRUE)

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                # scenarios = c('S1', 'S2', 'S3'),
                                outputType = list('summary', 'all'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_true(all(c('summary','all_events') %in% names(ewr_out)))
  expect_true(all(unique(ewr_out$summary$scenario) %in%
               c('base_base', 'down4_down4', 'S1_base_base', 'S2_up4_up4', 'up4_up4')))


})

test_that('manual scenario naming', {
  # create dir so building makes sense
  make_temp_hydro()

  # The user would supply this
  scenelist <- list(S1 = 'base/base.csv', S2 = 'down4/down4.csv', S3 = 'up4/up4.csv')

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                scenarios = scenelist,
                                outputType = list('summary', 'all'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_true(all(c('summary','all_events') %in% names(ewr_out)))
  expect_equal(unique(ewr_out$summary$scenario),
               c('S1', 'S2', 'S3'))


  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/S1',
                          'module_output/EWR/S1/all_events',
                          'module_output/EWR/S1/all_events/S1.csv',
                          'module_output/EWR/S1/summary',
                          'module_output/EWR/S1/summary/S1.csv',
                          'module_output/EWR/S2',
                          'module_output/EWR/S2/all_events',
                          'module_output/EWR/S2/all_events/S2.csv',
                          'module_output/EWR/S2/summary',
                          'module_output/EWR/S2/summary/S2.csv',
                          'module_output/EWR/S3',
                          'module_output/EWR/S3/all_events',
                          'module_output/EWR/S3/all_events/S3.csv',
                          'module_output/EWR/S3/summary',
                          'module_output/EWR/S3/summary/S3.csv')

  expect_true(all(realised_structure %in% expected_structure))


})

# Does the read-in and run work for singe gauges per csv?
test_that('csv per gauge works', {
  # First, generate temporary hydrograph files

  make_temp_multifile()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_multi,
                                output_parent_dir = '',
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'all'))

  expect_equal(length(ewr_out), 2)
  expect_true(all(c('summary', 'all_events') %in% names(ewr_out)))

  # These come in with directory_gauge. Split gauge off and should be left with scenarios.
  ewr_out$summary <- ewr_out$summary |>
    dplyr::mutate(scenario = purrr::map_chr(scenario, \(x) stringr::str_split_1(x, '_')[1]))
  expect_equal(unique(ewr_out$summary$scenario), c('base',
'down4',
'up4'))

  # I'm now controlling the scenario names in the toolkit, so this should work.


})

test_that('saving works for one', {

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary'),
                                datesuffix = FALSE,
                                returnType = list('summary'))
  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), 'summary')

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/base_base',
                          'module_output/EWR/base_base/summary',
                          'module_output/EWR/base_base/summary/base_base.csv',
                          'module_output/EWR/down4_down4',
                          'module_output/EWR/down4_down4/summary',
                          'module_output/EWR/down4_down4/summary/down4_down4.csv',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/up4_up4',
                          'module_output/EWR/up4_up4/summary',
                          'module_output/EWR/up4_up4/summary/up4_up4.csv')

  expect_equal(realised_structure, expected_structure)

})

test_that('saving and returning works for multiple', {

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary','all'),
                                datesuffix = FALSE,
                                returnType = list('summary','all'))
  expect_equal(length(ewr_out), 2)
  expect_equal(names(ewr_out), c('summary', 'all_events'))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/base_base',
                          'module_output/EWR/base_base/all_events',
                          'module_output/EWR/base_base/all_events/base_base.csv',
                          'module_output/EWR/base_base/summary',
                          'module_output/EWR/base_base/summary/base_base.csv',
                          'module_output/EWR/down4_down4',
                          'module_output/EWR/down4_down4/all_events',
                          'module_output/EWR/down4_down4/all_events/down4_down4.csv',
                          'module_output/EWR/down4_down4/summary',
                          'module_output/EWR/down4_down4/summary/down4_down4.csv',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/up4_up4',
                          'module_output/EWR/up4_up4/all_events',
                          'module_output/EWR/up4_up4/all_events/up4_up4.csv',
                          'module_output/EWR/up4_up4/summary',
                          'module_output/EWR/up4_up4/summary/up4_up4.csv')

  expect_equal(realised_structure, expected_structure)

})

test_that('saving and returning works for all (or nearly all) ewr outputs', {

  # create dir so building makes sense
  make_temp_hydro()

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

  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/base_base',
                          'module_output/EWR/base_base/all_events',
                          'module_output/EWR/base_base/all_events/base_base.csv',
                          'module_output/EWR/base_base/all_successful_events',
                          'module_output/EWR/base_base/all_successful_events/base_base.csv',
                          'module_output/EWR/base_base/all_successful_interEvents',
                          'module_output/EWR/base_base/all_successful_interEvents/base_base.csv',
                          'module_output/EWR/base_base/summary',
                          'module_output/EWR/base_base/summary/base_base.csv',
                          'module_output/EWR/base_base/yearly',
                          'module_output/EWR/base_base/yearly/base_base.csv',
                          'module_output/EWR/down4_down4',
                          'module_output/EWR/down4_down4/all_events',
                          'module_output/EWR/down4_down4/all_events/down4_down4.csv',
                          'module_output/EWR/down4_down4/all_successful_events',
                          'module_output/EWR/down4_down4/all_successful_events/down4_down4.csv',
                          'module_output/EWR/down4_down4/all_successful_interEvents',
                          'module_output/EWR/down4_down4/all_successful_interEvents/down4_down4.csv',
                          'module_output/EWR/down4_down4/summary',
                          'module_output/EWR/down4_down4/summary/down4_down4.csv',
                          'module_output/EWR/down4_down4/yearly',
                          'module_output/EWR/down4_down4/yearly/down4_down4.csv',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/up4_up4',
                          'module_output/EWR/up4_up4/all_events',
                          'module_output/EWR/up4_up4/all_events/up4_up4.csv',
                          'module_output/EWR/up4_up4/all_successful_events',
                          'module_output/EWR/up4_up4/all_successful_events/up4_up4.csv',
                          'module_output/EWR/up4_up4/all_successful_interEvents',
                          'module_output/EWR/up4_up4/all_successful_interEvents/up4_up4.csv',
                          'module_output/EWR/up4_up4/summary',
                          'module_output/EWR/up4_up4/summary/up4_up4.csv',
                          'module_output/EWR/up4_up4/yearly',
                          'module_output/EWR/up4_up4/yearly/up4_up4.csv')

  expect_equal(realised_structure, expected_structure)

})

test_that('NETCDF saving and returning works for all (or nearly all) ewr outputs', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir = 'nchydros',
                  orig_hydro_dir = system.file("extdata/ncdfexample/nchydros", package = 'werptoolkitr'))

  # all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

  ewroutlist <- list('summary',
                     'yearly',
                     'all_events',
                     'all_successful_events',
                     # 'all_interEvents',
                     'all_successful_interEvents')

  ewr_out <- prep_run_save_ewrs(hydro_dir = '_test_data/nchydros',
                                output_parent_dir = temp_parent_dir,
                                model_format = 'IQQM - netcdf',
                                outputType = ewroutlist,
                                datesuffix = FALSE,
                                returnType = ewroutlist)

  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))
  expect_equal(unique(ewr_out$summary$scenario), c("S1_StraightNodeGauge", "S2_StraightNodeGauge"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('module_output',
                          'module_output/EWR',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/S1_StraightNodeGauge',
                          'module_output/EWR/S1_StraightNodeGauge/all_events',
                          'module_output/EWR/S1_StraightNodeGauge/all_events/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S1_StraightNodeGauge/all_successful_events',
                          'module_output/EWR/S1_StraightNodeGauge/all_successful_events/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S1_StraightNodeGauge/all_successful_interEvents',
                          'module_output/EWR/S1_StraightNodeGauge/all_successful_interEvents/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S1_StraightNodeGauge/summary',
                          'module_output/EWR/S1_StraightNodeGauge/summary/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S1_StraightNodeGauge/yearly',
                          'module_output/EWR/S1_StraightNodeGauge/yearly/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge',
                          'module_output/EWR/S2_StraightNodeGauge/all_events',
                          'module_output/EWR/S2_StraightNodeGauge/all_events/S2_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge/all_successful_events',
                          'module_output/EWR/S2_StraightNodeGauge/all_successful_events/S2_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge/all_successful_interEvents',
                          'module_output/EWR/S2_StraightNodeGauge/all_successful_interEvents/S2_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge/summary',
                          'module_output/EWR/S2_StraightNodeGauge/summary/S2_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge/yearly',
                          'module_output/EWR/S2_StraightNodeGauge/yearly/S2_StraightNodeGauge.csv',
                          'nchydros',
                          'nchydros/S1',
                          'nchydros/S1/StraightNodeGauge.nc',
                          'nchydros/S2',
                          'nchydros/S2/StraightNodeGauge.nc')

  expect_true(all(realised_structure %in% expected_structure))

})

test_that('zipped NETCDF saving and returning works for all (or nearly all) ewr outputs', {

  # create dir so building makes sense
  make_temp_zip(temp_hydro_dir = 'hydrographs',
                  orig_hydro_zip = system.file("extdata/ncdfexample/zipcdf.zip", package = 'werptoolkitr'))

  # all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

  ewroutlist <- list('summary',
                     'yearly',
                     'all_events',
                     'all_successful_events',
                     # 'all_interEvents',
                     'all_successful_interEvents')

  ewr_out <- prep_run_save_ewrs(hydro_dir = '_test_data/hydrographs/zipcdf.zip',
                                output_parent_dir = temp_parent_dir,
                                model_format = 'IQQM - netcdf',
                                outputType = ewroutlist,
                                datesuffix = FALSE,
                                returnType = ewroutlist)


  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))
  expect_equal(unique(ewr_out$summary$scenario), c("zipcdf_S1_Straight Node (Gauge)", "zipcdf_S2_Straight Node (Gauge)"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  # ugly filepaths, but that's a consequence of the zip structure.
  # The hydrozipextract shouldn't be htere
  expected_structure <- c('hydrographs',
                          'hydrographs/zipcdf.zip',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/hydrozipextract',
                          'module_output/EWR/hydrozipextract/zipcdf',
                          'module_output/EWR/hydrozipextract/zipcdf/S1',
                          'module_output/EWR/hydrozipextract/zipcdf/S1/Straight Node (Gauge).nc',
                          'module_output/EWR/hydrozipextract/zipcdf/S2',
                          'module_output/EWR/hydrozipextract/zipcdf/S2/Straight Node (Gauge).nc',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/all_events',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/all_events/zipcdf_S1_Straight Node (Gauge).csv',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/all_successful_events',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/all_successful_interEvents',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/summary',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/summary/zipcdf_S1_Straight Node (Gauge).csv',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/yearly',
                          'module_output/EWR/zipcdf_S1_Straight Node (Gauge)/yearly/zipcdf_S1_Straight Node (Gauge).csv',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/all_events',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/all_events/zipcdf_S2_Straight Node (Gauge).csv',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/all_successful_events',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/all_successful_interEvents',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/summary',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/summary/zipcdf_S2_Straight Node (Gauge).csv',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/yearly',
                          'module_output/EWR/zipcdf_S2_Straight Node (Gauge)/yearly/zipcdf_S2_Straight Node (Gauge).csv')

  expect_true(all(realised_structure %in% expected_structure))

})

test_that('NETCDF saving and returning works in parallel', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir = 'nchydros',
                  orig_hydro_dir = system.file("extdata/ncdfexample/nchydros", package = 'werptoolkitr'))

  # all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

  ewroutlist <- list('summary')

  ewr_out <- prep_run_save_ewrs(hydro_dir = '_test_data/nchydros',
                                output_parent_dir = temp_parent_dir,
                                model_format = 'IQQM - netcdf',
                                outputType = ewroutlist,
                                datesuffix = FALSE,
                                returnType = ewroutlist,
                                rparallel = TRUE)

  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))
  expect_equal(unique(ewr_out$summary$scenario), c("S1_StraightNodeGauge", "S2_StraightNodeGauge"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('module_output',
                          'module_output/EWR',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/S1_StraightNodeGauge',
                          'module_output/EWR/S1_StraightNodeGauge/all_events',
                          'module_output/EWR/S1_StraightNodeGauge/all_events/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S1_StraightNodeGauge/all_successful_events',
                          'module_output/EWR/S1_StraightNodeGauge/all_successful_interEvents',
                          'module_output/EWR/S1_StraightNodeGauge/summary',
                          'module_output/EWR/S1_StraightNodeGauge/summary/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S1_StraightNodeGauge/yearly',
                          'module_output/EWR/S1_StraightNodeGauge/yearly/S1_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge',
                          'module_output/EWR/S2_StraightNodeGauge/all_events',
                          'module_output/EWR/S2_StraightNodeGauge/all_events/S2_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge/all_successful_events',
                          'module_output/EWR/S2_StraightNodeGauge/all_successful_interEvents',
                          'module_output/EWR/S2_StraightNodeGauge/summary',
                          'module_output/EWR/S2_StraightNodeGauge/summary/S2_StraightNodeGauge.csv',
                          'module_output/EWR/S2_StraightNodeGauge/yearly',
                          'module_output/EWR/S2_StraightNodeGauge/yearly/S2_StraightNodeGauge.csv',
                          'nchydros',
                          'nchydros/S1',
                          'nchydros/S1/StraightNodeGauge.nc',
                          'nchydros/S2',
                          'nchydros/S2/StraightNodeGauge.nc')

  expect_true(all(realised_structure %in% expected_structure))

})

test_that('specifying *Type as character instead of list', {

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = c('summary',
                                                'all'),
                                datesuffix = FALSE,
                                returnType = c('summary','all'))
  expect_equal(length(ewr_out), 2)
  expect_equal(names(ewr_out), c('summary','all_events'))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/base_base',
                          'module_output/EWR/base_base/all_events',
                          'module_output/EWR/base_base/all_events/base_base.csv',
                          'module_output/EWR/base_base/summary',
                          'module_output/EWR/base_base/summary/base_base.csv',
                          'module_output/EWR/down4_down4',
                          'module_output/EWR/down4_down4/all_events',
                          'module_output/EWR/down4_down4/all_events/down4_down4.csv',
                          'module_output/EWR/down4_down4/summary',
                          'module_output/EWR/down4_down4/summary/down4_down4.csv',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/up4_up4',
                          'module_output/EWR/up4_up4/all_events',
                          'module_output/EWR/up4_up4/all_events/up4_up4.csv',
                          'module_output/EWR/up4_up4/summary',
                          'module_output/EWR/up4_up4/summary/up4_up4.csv')

  expect_equal(realised_structure, expected_structure)

})

test_that('Single scenario among many, with access to the outer directory', {

  # create dir so building makes sense
  make_temp_hydro()

  # So far, assuming we can read and write to the outer 'hydrographs' directory, this is all standard.

  # Can we run this for a single hydro scenario?
  ewr_out <- prep_run_save_ewrs(hydro_dir = file.path(temp_hydro_dir, 'base'),
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary'),
                                datesuffix = FALSE,
                                returnType = list('summary'))



  # Expect only the single output, not for all the scenarios
  expect_equal(list.files(file.path(temp_parent_dir, 'module_output', 'EWR'), recursive = TRUE),
               c("base/summary/base.csv", "ewr_metadata.json", "ewr_metadata.yml"))

  # Tear down

})

test_that('Single scenario among many, no access to the outer directory', {

  # create dir so building makes sense
  make_temp_hydro()

  # Now, let's assume all we have is a path to the specific scenario
  scenario_path <- file.path(temp_hydro_dir, 'base')

  # So far, assuming we can read and write to the outer 'hydrographs' directory, this is all standard.

  # The test here is whether we can send the `output_parent_dir` the same value as `hydro_dir` to put the output inside the hydro scenario
  ewr_out <- prep_run_save_ewrs(hydro_dir = scenario_path,
                                output_parent_dir = scenario_path,
                                outputType = list('summary'),
                                datesuffix = FALSE,
                                returnType = list('summary'))



  # Expect only the single output, not for all the scenarios
  expected_structure <- c('base.csv',
                          'base.json',
                          'module_output/EWR/base/summary/base.csv',
                          "module_output/EWR/ewr_metadata.json",
                          "module_output/EWR/ewr_metadata.yml")
  expect_equal(list.files(scenario_path, recursive = TRUE), expected_structure)

  # Tear down

})

test_that('Single scenario among many, no access to the outer directory, different names', {

  # create dir so building makes sense
  make_temp_hydro()

  # Now, let's assume all we have is a path to the specific scenario


  # make the results dir name not match the file name of the results. so now
  # this has hydrographs/results/base.csv instead of hydrographs/base/base.csv
  file.rename(file.path(temp_hydro_dir, 'base'), file.path(temp_hydro_dir, 'results'))
  scenario_path <- file.path(temp_hydro_dir, 'results')

  # So far, assuming we can read and write to the outer 'hydrographs' directory, this is all standard.

  # The test here is whether we can send the `output_parent_dir` the same value as `hydro_dir` to put the output inside the hydro scenario
  ewr_out <- prep_run_save_ewrs(hydro_dir = scenario_path,
                                output_parent_dir = scenario_path,
                                outputType = list('summary'),
                                datesuffix = FALSE,
                                returnType = list('summary'))



  # Expect only the single output, not for all the scenarios
  expected_structure <- c('base.csv',
'base.json',
                          'module_output/EWR/base/summary/base.csv',
                          "module_output/EWR/ewr_metadata.json",
                          "module_output/EWR/ewr_metadata.yml")
  expect_equal(list.files(scenario_path, recursive = TRUE), expected_structure)

  # Tear down

})

## NOTE: the python `import`s from system.file, and so while developing won't pick up
## new versions without `install`ing the package. The `future` uses seem most
## sensitive to this- I think maybe test shims don't work right?
test_that('parallel works for two', {

  # print(future::plan())
  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary', 'yearly'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'yearly'),
                                rparallel = TRUE)


  expect_equal(length(ewr_out), 2)
  expect_equal(names(ewr_out), c('summary', 'yearly'))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/base_base',
                          'module_output/EWR/base_base/summary',
                          'module_output/EWR/base_base/summary/base_base.csv',
                          'module_output/EWR/base_base/yearly',
                          'module_output/EWR/base_base/yearly/base_base.csv',
                          'module_output/EWR/down4_down4',
                          'module_output/EWR/down4_down4/summary',
                          'module_output/EWR/down4_down4/summary/down4_down4.csv',
                          'module_output/EWR/down4_down4/yearly',
                          'module_output/EWR/down4_down4/yearly/down4_down4.csv',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/up4_up4',
                          'module_output/EWR/up4_up4/summary',
                          'module_output/EWR/up4_up4/summary/up4_up4.csv',
                          'module_output/EWR/up4_up4/yearly',
                          'module_output/EWR/up4_up4/yearly/up4_up4.csv')

  expect_equal(realised_structure, expected_structure)

})


test_that('parallel works for one', {

  # print(future::plan())

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary'),
                                datesuffix = FALSE,
                                returnType = list('summary'),
                                rparallel = TRUE)

  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), 'summary')

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/base_base',
                          'module_output/EWR/base_base/summary',
                          'module_output/EWR/base_base/summary/base_base.csv',
                          'module_output/EWR/down4_down4',
                          'module_output/EWR/down4_down4/summary',
                          'module_output/EWR/down4_down4/summary/down4_down4.csv',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/up4_up4',
                          'module_output/EWR/up4_up4/summary',
                          'module_output/EWR/up4_up4/summary/up4_up4.csv')

  expect_equal(realised_structure, expected_structure)

})

test_that('parallel works for no return', {

  # print(future::plan())

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary', 'yearly'),
                                datesuffix = FALSE,
                                returnType = list('none'),
                                rparallel = TRUE)

  expect_equal(length(ewr_out), 0)

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json',
                          'module_output',
                          'module_output/EWR',
                          'module_output/EWR/base_base',
                          'module_output/EWR/base_base/summary',
                          'module_output/EWR/base_base/summary/base_base.csv',
                          'module_output/EWR/base_base/yearly',
                          'module_output/EWR/base_base/yearly/base_base.csv',
                          'module_output/EWR/down4_down4',
                          'module_output/EWR/down4_down4/summary',
                          'module_output/EWR/down4_down4/summary/down4_down4.csv',
                          'module_output/EWR/down4_down4/yearly',
                          'module_output/EWR/down4_down4/yearly/down4_down4.csv',
                          'module_output/EWR/ewr_metadata.json',
                          'module_output/EWR/ewr_metadata.yml',
                          'module_output/EWR/up4_up4',
                          'module_output/EWR/up4_up4/summary',
                          'module_output/EWR/up4_up4/summary/up4_up4.csv',
                          'module_output/EWR/up4_up4/yearly',
                          'module_output/EWR/up4_up4/yearly/up4_up4.csv')

  expect_equal(realised_structure, expected_structure)

})

test_that('speed test', {

  skip(message = 'Speed test not really a test, just including to take advantage of setup')
  # print(future::plan())

  # create dir so building makes sense
  make_temp_hydro()

  # microbenchmark would be better, this is quick and dirty
  tp <- system.time(ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary', 'yearly'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'yearly'),
                                rparallel = TRUE))

  ts <- system.time(ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                                  output_parent_dir = temp_parent_dir,
                                                  outputType = list('summary', 'yearly'),
                                                  datesuffix = FALSE,
                                                  returnType = list('summary', 'yearly'),
                                                  rparallel = FALSE))
  tp
  ts

})




