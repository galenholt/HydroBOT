# my testthat::test_path() is 'tests/testthat', I have to use paths relative to that.
# And I don't want to use the `system.file` method, since i need to just pass characters.

temp_hydro_dir <- "_test_data/hydrographs"
temp_parent_dir <- "_test_data"

test_that("parameter file works with params.R", {
  make_temp_hydro()
  test <- run_hydrobot_params(yamlpath = system.file("yml/package_params.yml",
    package = "HydroBOT"
  ))

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)
})

test_that("parameter file works with character aggregation", {
  make_temp_hydro()
  test <- run_hydrobot_params(yamlpath = system.file("yml/package_params_charseq.yml",
    package = "HydroBOT"
  ))

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)
})


# since the package_params.yml calls for temp, overwrite with temp_multi as a passed_arg.
# This will fail if the overwrite doesn't happen, since temp won't exist.
# This also has the benefit of testing the multifile
test_that("passing from command works", {
  # Make sure the dir package_params asks for is gone

  # create the new dir
  make_temp_multifile()

  # The yaml here becomes a pain if it needs to pass more than one item.
  test <- run_hydrobot_params(
    yamlpath = system.file("yml/package_params.yml",
      package = "HydroBOT"
    ),
    passed_args = "ewr:\n output_parent_dir: '_test_data'\naggregation:\n namehistory: TRUE"
  )

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)
})

test_that("list args work", {
  # Make sure the dir package_params asks for is gone

  # create the new dir
  make_temp_multifile()

  # The yaml here becomes a pain if it needs to pass more than one item.
  test <- run_hydrobot_params(
    yamlpath = system.file("yml/package_params.yml",
      package = "HydroBOT"
    ),
    list_args = list(output_parent_dir = "_test_data")
  )

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)


  # There's a weird thing where the params lists in quarto have a special class, and that breaks json
  qlist <- list(output_parent_dir = "_test_data")
  class(qlist) <- "knit_param_list"

  # The yaml here becomes a pain if it needs to pass more than one item.
  test2 <- run_hydrobot_params(
    yamlpath = system.file("yml/package_params.yml",
      package = "HydroBOT"
    ),
    list_args = qlist
  )

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test2)
})


test_that("re-running from self-defined params works", {
  make_temp_hydro()
  test <- run_hydrobot_params(yamlpath = system.file("yml/package_params.yml",
    package = "HydroBOT"
  ))

  # now run that off the one it just created
  test2 <- run_hydrobot_params(yamlpath = file.path(
    temp_parent_dir,
    "aggregator_output",
    "agg_metadata.yml"
  ))

  # I'll have to again think more about stronger tests here, but the main thing is it runs.
  expect_null(test)
  expect_null(test2)
})

# some unusual directory structures
test_that("Single scenario among many, no access to the outer directory", {
  # create dir so building makes sense
  make_temp_hydro()

  # Now, let's assume all we have is a path to the specific scenario
  scenario_path <- file.path(temp_hydro_dir, "base")

  test <- run_hydrobot_params(list_args = list(
    ewr = list(
    output_parent_dir = "_test_data/hydrographs/base",
    hydro_dir = "_test_data/hydrographs/base"
    ),
    aggregation = list(
    aggregation_def = "yml/params.R"
    )
  ))

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)

  # Expect only the single output, not for all the scenarios
  realised_structure <- list.files("_test_data/hydrographs/base", recursive = TRUE)
  expect_snapshot(realised_structure)

  # Tear down
})



test_that("Single scenario among many, no access to the outer directory, different names", {
  # create dir so building makes sense
  make_temp_hydro()

  # Now, let's assume all we have is a path to the specific scenario


  # make the results dir name not match the file name of the results. so now
  # this has hydrographs/results/base.csv instead of hydrographs/base/base.csv
  file.rename(file.path(temp_hydro_dir, "base"), file.path(temp_hydro_dir, "results"))
  scenario_path <- file.path(temp_hydro_dir, "results")

  # So far, assuming we can read and write to the outer 'hydrographs' directory, this is all standard.

  # The test here is whether we can send the `output_parent_dir` the same value as `hydro_dir` to put the output inside the hydro scenario

  test <- run_hydrobot_params(list_args = list(
    ewr = list(
      output_parent_dir = scenario_path,
    hydro_dir = scenario_path
    ),
    aggregation = list(
    aggregation_def = "yml/params.R"
    )
  ))

  # kind of silly- the main thing is that the above doesn't fail
  expect_null(test)

  # Expect only the single output, not for all the scenarios
  realised_structure <- list.files("_test_data/hydrographs/results", recursive = TRUE)
  expect_snapshot(realised_structure)

  # Tear down
})
