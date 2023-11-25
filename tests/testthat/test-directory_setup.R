# Set up some base directory structures
hydro_dir <- system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')


# Tests -------------------------------------------------------------------

test_that("get scenario names and nothing else", {
  scenarios <- scenario_names_from_hydro(hydro_dir)
  expect_equal(scenarios, c('base', 'down4', 'up4'))
})

test_that("no scenario names if inside scenario", {
  scenarios <- scenario_names_from_hydro(file.path(hydro_dir, 'base'))
  expect_equal(scenarios, 'base')
})

test_that("scenario paths works for single csvs in each", {
  scenario_paths <- find_scenario_paths(hydro_dir)
  # expect_equal(2 * 2, 4)
})

test_that("scenario paths and the name fixer works for multiple csvs in each", {

  temp_hydro_multi <- '_test_data/hydrographs'
  # If needed, build the dir. This takes a while so don't tear it down, typically
  make_temp_multifile(testdir = '_test_data',
                      temp_hydro = 'hydrographs')

  scenario_paths <- find_scenario_paths(temp_hydro_multi)

  # Should be a list
  expect_snapshot(scenario_paths)

  # Now find the scenario names, fix non-unique naming, and check
  scenenames <- scenario_names_from_hydro(temp_hydro_multi)
  expect_equal(scenenames, c('base', 'down4', 'up4'))

  # This should fix
  newpaths <- fix_file_scenarios(scenario_paths, scenarios = scenenames)

  # check they changed
  scenario_paths <- find_scenario_paths(temp_hydro_multi)

  expect_snapshot(scenario_paths)

})

test_that('creating output dirs works with hydro_dir having all scenarios', {
  # In this situation, the hydrograph folder and the outputs end up at the same level, underneath a 'project_dir'
  project_dir <- '_test_data'

  make_temp_hydro()

  scenarios <- scenario_names_from_hydro(file.path(project_dir, 'hydrographs'))

  outpath <- make_output_dir(parent_dir = project_dir, scenarios = scenarios)

  realised_structure <- list.files(project_dir, recursive = TRUE, include.dirs = TRUE)
  expected_structure <- c('hydrographs', 'hydrographs/base',
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
                          'module_output/EWR/base',
                          'module_output/EWR/base/all_events',
                          'module_output/EWR/base/annual',
                          'module_output/EWR/base/summary',
                          'module_output/EWR/down4',
                          'module_output/EWR/down4/all_events',
                          'module_output/EWR/down4/annual',
                          'module_output/EWR/down4/summary',
                          'module_output/EWR/up4',
                          'module_output/EWR/up4/all_events',
                          'module_output/EWR/up4/annual',
                          'module_output/EWR/up4/summary')

  expect_equal(realised_structure, expected_structure)

})

test_that('creating output dirs works with hydro_dir as a single scenario', {

  # Here, the outputs end up within the same folder as the hydrographs

  single_hydro_scenario <- '_test_data/hydrographs/base'
  make_temp_hydro()

  scenarios <- scenario_names_from_hydro(single_hydro_scenario)

  outpath <- make_output_dir(parent_dir = single_hydro_scenario, scenarios = scenarios)

  # check the full structure of the parent tree to make sure we're not adding extra stuff elsewhere
  realised_structure <- list.files('_test_data', recursive = TRUE, include.dirs = TRUE)
  expected_structure <- c('hydrographs',
                          'hydrographs/base',
                          'hydrographs/base/base.csv',
                          'hydrographs/base/base.json',
                          'hydrographs/base/module_output',
                          'hydrographs/base/module_output/EWR',
                          'hydrographs/base/module_output/EWR/base',
                          'hydrographs/base/module_output/EWR/base/all_events',
                          'hydrographs/base/module_output/EWR/base/annual',
                          'hydrographs/base/module_output/EWR/base/summary',
                          'hydrographs/down4',
                          'hydrographs/down4/down4.csv',
                          'hydrographs/down4/down4.json',
                          'hydrographs/scenario_metadata.json',
                          'hydrographs/scenario_metadata.yml',
                          'hydrographs/up4',
                          'hydrographs/up4/up4.csv',
                          'hydrographs/up4/up4.json')

  expect_equal(realised_structure, expected_structure)

})
