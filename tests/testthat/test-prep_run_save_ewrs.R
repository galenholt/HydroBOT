# Set up some base directory structures

proj_dir <- system.file("extdata/testsmall", package = 'werptoolkitr')
hydro_dir <- system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')

temp_hydro_dir = '_test_data/temp_one/hydrographs'
temp_parent_dir = '_test_data/temp_one'

temp_hydro_multi = '_test_data/temp_multi/hydrographs'
temp_parent_multi = '_test_data/temp_multi'

# Make sure the test dirs are blank
destroy_temp_hydro(temp_parent_dir)
destroy_temp_multifile(temp_parent_multi)


test_that('returns one result, no saving', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir)

  # Test it didn't create anything since outputType = 'none'
  start_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary'))
  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), 'summary')
  expect_equal(unique(ewr_out$summary$scenario), c('base', 'down4', 'up4'))
  # Test it didn't create anything since outputType = 'none'
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_equal(start_structure, realised_structure)
  destroy_temp_hydro(temp_parent_dir)
})

# test multiple returns- use the next one once EWR is debugged.
test_that('returns list', {
  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir)

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_true(all(c('summary', 'all') %in% names(ewr_out)))
  expect_equal(unique(ewr_out$summary$scenario), c('base', 'down4', 'up4'))

  destroy_temp_hydro(temp_parent_dir)
})

# Does the read-in and run work for singe gauges per csv?
test_that('csv per gauge works', {
  # First, generate temporary hydrograph files

  make_temp_multifile(temp_hydro_multi = temp_hydro_multi)

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_multi,
                                output_parent_dir = '',
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_true(all(c('summary', 'all') %in% names(ewr_out)))

  # These come in with directory_gauge. Split gauge off and should be left with scenarios.
  ewr_out$summary <- ewr_out$summary |>
    dplyr::mutate(scenario = purrr::map_chr(scenario, \(x) stringr::str_split_1(x, '_')[1]))
  expect_equal(unique(ewr_out$summary$scenario), c('base', 'down4', 'up4'))
  # 'scenario' here gets the csv name. That happens inside the EWR tool- the
  # `_get_file_names` function does a `.split('/')` on the filepath to name the
  # scenario. I had basically tricked it previously by using `os.sep`, which
  # uses `'\\'` on windows, and so the split didnt pick it up and we got the
  # whole path, which i then later split on to get just the scenario name. That
  # feesl *really* unstable and reliant on very system-specific behaviour.

  # The catch is, I need to NOT name scenarios by just gauge, and the EWR tool
  # is auto-naming. So I'll have to figure out how to trick it. I just need to
  # do it in a more consistent way than using funny paths that are os-dependent.

  # One option would be to loop over scenarios, and replace the scenario column
  # with the loopname, but that's clunky and error-prone and gets rid of the
  # advantages of the internal loops in EWR

  # I think probably the best way to do this is to encourage the use of single
  # files per scenario, and if not, filenames that have the scenario name in
  # them with the gauge name.

  destroy_temp_multifile(temp_parent_multi)
})

test_that('saving works for one', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir)

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
                          'module_output/EWR/base',
                          'module_output/EWR/base/allevents',
                          'module_output/EWR/base/annual',
                          'module_output/EWR/base/summary',
                          'module_output/EWR/base/summary/base.csv',
                          'module_output/EWR/down4',
                          'module_output/EWR/down4/allevents',
                          'module_output/EWR/down4/annual',
                          'module_output/EWR/down4/summary',
                          'module_output/EWR/down4/summary/down4.csv',
                          "module_output/EWR/ewr_metadata.json",
                          "module_output/EWR/ewr_metadata.yml",
                          'module_output/EWR/up4',
                          'module_output/EWR/up4/allevents',
                          'module_output/EWR/up4/annual',
                          'module_output/EWR/up4/summary',
                          'module_output/EWR/up4/summary/up4.csv')
  expect_equal(realised_structure, expected_structure)
  destroy_temp_hydro(temp_parent_dir)
})

test_that('saving and returning works for multiple', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir)

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('summary', 'all'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_equal(names(ewr_out), c('summary', 'all'))

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
                          'module_output/EWR/base',
                          'module_output/EWR/base/allevents',
                          'module_output/EWR/base/allevents/base.csv',
                          'module_output/EWR/base/annual',
                          'module_output/EWR/base/summary',
                          'module_output/EWR/base/summary/base.csv',
                          'module_output/EWR/down4',
                          'module_output/EWR/down4/allevents',
                          'module_output/EWR/down4/allevents/down4.csv',
                          'module_output/EWR/down4/annual',
                          'module_output/EWR/down4/summary',
                          'module_output/EWR/down4/summary/down4.csv',
                          "module_output/EWR/ewr_metadata.json",
                          "module_output/EWR/ewr_metadata.yml",
                          'module_output/EWR/up4',
                          'module_output/EWR/up4/allevents',
                          'module_output/EWR/up4/allevents/up4.csv',
                          'module_output/EWR/up4/annual',
                          'module_output/EWR/up4/summary',
                          'module_output/EWR/up4/summary/up4.csv')

  expect_equal(realised_structure, expected_structure)
  destroy_temp_hydro(temp_parent_dir)
})

test_that('specifying *Type as character instead of list', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir)

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = c('summary', 'all'),
                                datesuffix = FALSE,
                                returnType = c('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_equal(names(ewr_out), c('summary', 'all'))

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
                          'module_output/EWR/base',
                          'module_output/EWR/base/allevents',
                          'module_output/EWR/base/allevents/base.csv',
                          'module_output/EWR/base/annual',
                          'module_output/EWR/base/summary',
                          'module_output/EWR/base/summary/base.csv',
                          'module_output/EWR/down4',
                          'module_output/EWR/down4/allevents',
                          'module_output/EWR/down4/allevents/down4.csv',
                          'module_output/EWR/down4/annual',
                          'module_output/EWR/down4/summary',
                          'module_output/EWR/down4/summary/down4.csv',
                          "module_output/EWR/ewr_metadata.json",
                          "module_output/EWR/ewr_metadata.yml",
                          'module_output/EWR/up4',
                          'module_output/EWR/up4/allevents',
                          'module_output/EWR/up4/allevents/up4.csv',
                          'module_output/EWR/up4/annual',
                          'module_output/EWR/up4/summary',
                          'module_output/EWR/up4/summary/up4.csv')

  expect_equal(realised_structure, expected_structure)
  destroy_temp_hydro(temp_parent_dir)
})

test_that('Single scenario among many, with access to the outer directory', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir)

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
  destroy_temp_hydro(temp_parent_dir)
})

test_that('Single scenario among many, no access to the outer directory', {

  # create dir so building makes sense
  make_temp_hydro(temp_hydro_dir)

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
  expected_structure <- c('base.csv', 'base.json',
                          'module_output/EWR/base/summary/base.csv',
                          "module_output/EWR/ewr_metadata.json",
                          "module_output/EWR/ewr_metadata.yml")
  expect_equal(list.files(scenario_path, recursive = TRUE), expected_structure)

  # Tear down
  destroy_temp_hydro(temp_parent_dir)
})

destroy_temp_hydro(temp_parent_dir)
destroy_temp_multifile(temp_parent_multi)
