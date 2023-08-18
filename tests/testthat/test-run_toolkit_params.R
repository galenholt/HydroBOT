# my testthat::test_path() is 'tests/testthat', I have to use paths relative to that.
# And I don't want to use the `system.file` method, since i need to just pass characters.

temp_hydro_dir = '_test_data/temp_one/hydrographs'
temp_parent_dir = '_test_data/temp_one'

temp_hydro_multi = '_test_data/temp_multi/hydrographs'
temp_parent_multi = '_test_data/temp_multi'

test_that("parameter file works", {
  make_temp_hydro(temp_hydro_dir)
  test <- run_toolkit_params(yamlpath = system.file('yml/package_params.yml',
                                                    package = 'werptoolkitr'))

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)
  destroy_temp_hydro(temp_parent_dir)

})

# since the package_params.yml calls for temp_one, overwrite with temp_multi as a passed_arg.
# This will fail if the overwrite doesn't happen, since temp_one won't exist.
# This also has the benefit of testing the multifile
test_that("passing from command works", {
  # Make sure the dir package_params asks for is gone
  destroy_temp_hydro(temp_parent_dir)
  # create the new dir
  make_temp_multifile(temp_hydro_multi = temp_hydro_multi)

  # The yaml here becomes a pain if it needs to pass more than one item.
  test <- run_toolkit_params(yamlpath = system.file('yml/package_params.yml',
                                                    package = 'werptoolkitr'),
                             passed_args = "output_parent_dir: '_test_data/temp_multi'")

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)

  destroy_temp_multifile(temp_parent_multi)

})
