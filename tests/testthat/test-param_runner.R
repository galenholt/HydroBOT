# my testthat::test_path() is 'tests/testthat', I have to use paths relative to that.
# And I don't want to use the `system.file` method, since i need to just pass characters.

test_that("parameter file works", {
  test <- run_toolkit_params(yamlpath = system.file('yml/package_params.yml',
                                                    package = 'werptoolkitr'))

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)
})

test_that("passing from command works", {
  # The yaml here becomes a pain if it needs to pass more than one item.
  test <- run_toolkit_params(yamlpath = system.file('yml/package_params.yml',
                                                    package = 'werptoolkitr'),
                             passed_args = "scenario_dir: '_test_data/temp_hydro'")

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)
})
