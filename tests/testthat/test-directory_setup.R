# Set up some base directory structures
hydro_dir <- system.file("extdata/testsmall/hydrographs", package = 'HydroBOT')


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
  # only test the last bit, because the full path changes between `test` and `check`
  relative_part <- stringr::str_extract(scenario_paths, 'extdata.*')
  expect_snapshot(relative_part)
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
  expect_snapshot(realised_structure)

})

test_that('creating output dirs works with hydro_dir as a single scenario', {

  # Here, the outputs end up within the same folder as the hydrographs

  single_hydro_scenario <- '_test_data/hydrographs/base'
  make_temp_hydro()

  scenarios <- scenario_names_from_hydro(single_hydro_scenario)

  outpath <- make_output_dir(parent_dir = single_hydro_scenario, scenarios = scenarios)

  # check the full structure of the parent tree to make sure we're not adding extra stuff elsewhere
  realised_structure <- list.files('_test_data', recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)

})

test_that("file_search works", {

  temp_hydro_multi <- '_test_data/hydrographs'
  # If needed, build the dir. This takes a while so don't tear it down, typically
  make_temp_multifile(testdir = '_test_data',
                      temp_hydro = 'hydrographs')

  scenario_paths <- find_scenario_paths(temp_hydro_multi, type = 'csv', file_search = '412')

  # Should be a list
  expect_snapshot(scenario_paths)

  scenario_paths_all <- find_scenario_paths(temp_hydro_multi, type = 'csv')

  expect_equal(length(scenario_paths)*2, length(scenario_paths_all))

})

test_that("zip works", {

  # create dir so building makes sense
  make_temp_zip(
    temp_hydro_dir = "hydrographs",
    orig_hydro_zip = system.file("extdata/ncdfexample/zipcdf.zip", package = "HydroBOT")
  )

  temp_hydro <- '_test_data/hydrographs'
  scenario_paths <- find_scenario_paths(file.path(temp_hydro, 'zipcdf.zip'), type = 'nc', file_search = 'Straight Node')

  expect_snapshot_value(scenario_paths, style = 'deparse')
})
