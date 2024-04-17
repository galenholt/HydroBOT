# Set up some base directory structures
temp_hydro_dir <- "_test_data/hydrographs"
temp_parent_dir <- "_test_data"

temp_hydro_multi <- "_test_data/hydrographs"
temp_parent_multi <- "_test_data"

set_future_multi()

test_that("returns one result, no saving", {
  # create dir so building makes sense
  make_temp_hydro()

  # Test it didn't create anything since outputType = 'none'
  start_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  system.time(
  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("none"),
    datesuffix = FALSE,
    returnType = list("summary")
  )
  )

  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), "summary")
  expect_equal(
    unique(ewr_out$summary$scenario),
    c("base", "down4", "up4")
  )
  # Test it didn't create anything since outputType = 'none'
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_equal(start_structure, realised_structure)
})

# test multiple returns- use the next one once EWR is debugged.
test_that("returns list", {
  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("none"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )
  expect_equal(length(ewr_out), 2)
  expect_true(all(c("summary", "all_events") %in% names(ewr_out)))
  expect_equal(
    unique(ewr_out$summary$scenario),
    c("base", "down4", "up4")
  )
})

# Test complex directory scenario extraction.
test_that("complex dir structure", {
  # create dir so building makes sense
  make_temp_hydro()

  dir.create(file.path(temp_hydro_dir, "S1"))
  dir.create(file.path(temp_hydro_dir, "S2"))
  file.copy(file.path(temp_hydro_dir, "base"), file.path(temp_hydro_dir, "S1"), recursive = TRUE)
  file.copy(file.path(temp_hydro_dir, "up4"), file.path(temp_hydro_dir, "S2"), recursive = TRUE)

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    # scenarios = c('S1', 'S2', 'S3'),
    outputType = list("summary", "all"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )

  expect_equal(length(ewr_out), 2)
  expect_true(all(c("summary", "all_events") %in% names(ewr_out)))
  expect_true(all(unique(ewr_out$summary$scenario) %in%
    c("base", "down4", "S1_base", "S2_up4", "up4")))

  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)

})

test_that("manual scenario naming", {
  # create dir so building makes sense
  make_temp_hydro()

  # The user would supply this
  scenelist <- list(S1 = "base/base.csv", S2 = "down4/down4.csv", S3 = "up4/up4.csv")

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    scenarios = scenelist,
    outputType = list("summary", "all"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )
  expect_equal(length(ewr_out), 2)
  expect_true(all(c("summary", "all_events") %in% names(ewr_out)))
  expect_equal(
    unique(ewr_out$summary$scenario),
    c("S1", "S2", "S3")
  )


  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)
})


# Does the read-in and run work for singe gauges per csv?
test_that("do different length gauge records break EWR", {
  # First, generate temporary hydrograph files

  make_temp_multifile()

  # For each of those, delete some section(s)
  # super manual and annoying
  alldates <- tibble::tibble(Date = lubridate::ymd('20150205'), .rows = 0)
  base <- alldates
  up4 <- alldates
  down4 <- alldates
  allfiles <- list.files(temp_hydro_multi, recursive = TRUE)
  for (i in 1:length(allfiles)) {
    tc <- readr::read_csv(file.path(temp_hydro_multi, allfiles[i]))
    if ("412002" %in% names(tc)) {
      tc[lubridate::year(tc$Date) %in% c('2017'), 2] <- NA
    }
    if ("412005" %in% names(tc)) {
      tc[lubridate::year(tc$Date) %in% c('2015'), 2] <- NA
    }
    if ("412038" %in% names(tc)) {
      tc[lubridate::year(tc$Date) %in% c('2019'), 2] <- NA
    }
    if ("421001" %in% names(tc)) {
      tc[lubridate::year(tc$Date) %in% c('2015', '2018', '2019'), 2] <- NA

    }
    if ("421004" %in% names(tc)) {
      tc[lubridate::year(tc$Date) %in% c('2015', '2016'), 2] <- NA

    }
    if ("421011" %in% names(tc)) {
      tc[lubridate::year(tc$Date) %in% c('2016', '2018'), 2] <- NA

    }

    # Glue together
    alldates <- tibble::tibble(Date = unique(c(alldates$Date, tc$Date)))

    if (grepl('base', allfiles[i])) {
      base <- alldates |>
        dplyr::left_join(base) |>
        dplyr::left_join(tc)
    }
    if (grepl('down4', allfiles[i])) {
      down4 <- alldates |>
        dplyr::left_join(down4) |>
        dplyr::left_join(tc)
    }
    if (grepl('up4', allfiles[i])) {
      up4 <- alldates |>
        dplyr::left_join(up4) |>
        dplyr::left_join(tc)
    }

    # Save singles
    write.csv(tc, file.path(temp_hydro_multi, allfiles[i]))
  }

  # save the combos
  dir.create(file.path(temp_parent_dir, 'hydrosmoosh'))
  dir.create(file.path(temp_parent_dir, 'hydrosmoosh', 'base'))
  dir.create(file.path(temp_parent_dir, 'hydrosmoosh', 'down4'))
  dir.create(file.path(temp_parent_dir, 'hydrosmoosh', 'up4'))

  write.csv(base, file.path(temp_parent_dir, 'hydrosmoosh', 'base', 'base.csv'))
  write.csv(down4, file.path(temp_parent_dir, 'hydrosmoosh', 'down4', 'down4.csv'))
  write.csv(up4, file.path(temp_parent_dir, 'hydrosmoosh', 'up4', 'up4.csv'))


  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_multi,
    output_parent_dir = temp_parent_dir,
    outputType = list("none"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )

  smoosh_out <- prep_run_save_ewrs(
    hydro_dir = file.path(temp_parent_dir, 'hydrosmoosh'),
    output_parent_dir = temp_parent_dir,
    outputType = list("none"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )


  expect_equal(ewr_out$summary, smoosh_out$summary)

  # all_events gets sorted, but if we sort, should match
  ewa <- ewr_out$all_events |>
    dplyr::arrange(scenario, gauge, pu, ewr)
  swa <- smoosh_out$all_events |>
    dplyr::arrange(scenario, gauge, pu, ewr)

  expect_equal(ewa, swa)

})

# Does the read-in and run work for singe gauges per csv?
test_that("csv per gauge works", {
  # First, generate temporary hydrograph files

  make_temp_multifile()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_multi,
    output_parent_dir = temp_parent_dir,
    outputType = list("summary", "yearly"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )

  expect_equal(length(ewr_out), 2)
  expect_true(all(c("summary", "all_events") %in% names(ewr_out)))

  # These no longer come in with directory_gauge. Split gauge off and should be left with scenarios.
  # ewr_out$summary <- ewr_out$summary |>
  #   dplyr::mutate(scenario = purrr::map_chr(scenario, \(x) stringr::str_split_1(x, "_")[1]))
  expect_equal(unique(ewr_out$summary$scenario), c(
    "base",
    "down4",
    "up4"
  ))
  expect_equal(unique(ewr_out$all_events$scenario), c(
    "base",
    "down4",
    "up4"
  ))

  # I'm now controlling the scenario names in the toolkit, so this should work.

  # Test it created the expected structure.
  # These still need to create a single output per input, or they overwrite
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)
})


test_that("csv per gauge works for filenames", {
  # First, generate temporary hydrograph files

  make_temp_multifile()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_multi,
    scenarios_from = 'file',
    output_parent_dir = temp_parent_dir,
    outputType = list("summary", "yearly"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )

  expect_equal(length(ewr_out), 2)
  expect_true(all(c("summary", "all_events") %in% names(ewr_out)))

  # These no longer come in with directory_gauge. Split gauge off and should be left with scenarios.
  # ewr_out$summary <- ewr_out$summary |>
  #   dplyr::mutate(scenario = purrr::map_chr(scenario, \(x) stringr::str_split_1(x, "_")[1]))
  expect_snapshot(unique(ewr_out$summary$scenario))

  # I'm now controlling the scenario names in the toolkit, so this should work.

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)
})

test_that("saving works for one", {
  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("summary"),
    datesuffix = FALSE,
    returnType = list("summary")
  )

  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), "summary")

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)
})

test_that("saving works with subdir", {
  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    output_subdir = 'testsub',
    outputType = list("summary"),
    datesuffix = FALSE,
    returnType = list("summary")
  )
  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), "summary")

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)
})

test_that("saving and returning works for all (or nearly all) ewr outputs", {
  # create dir so building makes sense
  make_temp_hydro()

  # all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

  ewroutlist <- list(
    "summary",
    "yearly",
    "all_events",
    "all_successful_events",
    'all_interEvents',
    "all_successful_interEvents"
  )

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = ewroutlist,
    datesuffix = FALSE,
    returnType = ewroutlist
  )

  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)
})

test_that("NETCDF saving and returning works for all (or nearly all) ewr outputs", {
  # create dir so building makes sense
  make_temp_hydro(
    temp_hydro_dir = "nchydros",
    orig_hydro_dir = system.file("extdata/ncdfexample/nchydros", package = "werptoolkitr")
  )

  # all working as of 2.1.0

  ewroutlist <- list(
    "summary",
    "yearly",
    "all_events",
    "all_successful_events",
    'all_interEvents',
    "all_successful_interEvents"
  )

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = "_test_data/nchydros",
    output_parent_dir = temp_parent_dir,
    model_format = "IQQM - netcdf",
    outputType = ewroutlist,
    datesuffix = FALSE,
    returnType = ewroutlist
  )

  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))
  expect_equal(unique(ewr_out$summary$scenario), c("S1", "S2"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)
  expect_snapshot(realised_structure)

})

test_that("zipped NETCDF saving and returning works for all (or nearly all) ewr outputs", {
  # create dir so building makes sense
  make_temp_zip(
    temp_hydro_dir = "hydrographs",
    orig_hydro_zip = system.file("extdata/ncdfexample/zipcdf.zip", package = "werptoolkitr")
  )

  # all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

  ewroutlist <- list(
    "summary",
    "yearly",
    "all_events",
    "all_successful_events",
    'all_interEvents',
    "all_successful_interEvents"
  )

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = "_test_data/hydrographs/zipcdf.zip",
    output_parent_dir = temp_parent_dir,
    model_format = "IQQM - netcdf",
    scenarios_from = 'file',
    outputType = ewroutlist,
    datesuffix = FALSE,
    returnType = ewroutlist
  )


  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))
  expect_equal(unique(ewr_out$summary$scenario), c("zipcdf_S1", "zipcdf_S2"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  # ugly filepaths, but that's a consequence of the zip structure.
  # The hydrozipextract shouldn't be htere
  expect_snapshot(realised_structure)

})

test_that("NETCDF saving and returning works in parallel", {
  # create dir so building makes sense
  make_temp_hydro(
    temp_hydro_dir = "nchydros",
    orig_hydro_dir = system.file("extdata/ncdfexample/nchydros", package = "werptoolkitr")
  )

  # all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

  ewroutlist <- list("summary")

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = "_test_data/nchydros",
    output_parent_dir = temp_parent_dir,
    model_format = "IQQM - netcdf",
    outputType = ewroutlist,
    datesuffix = FALSE,
    returnType = ewroutlist,
    rparallel = TRUE
  )

  expect_equal(length(ewr_out), length(ewroutlist))
  expect_equal(names(ewr_out), unlist(ewroutlist))
  expect_equal(unique(ewr_out$summary$scenario), c("S1", "S2"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_snapshot(realised_structure)

})

test_that("specifying *Type as character instead of list", {
  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = c(
      "summary",
      "all"
    ),
    datesuffix = FALSE,
    returnType = c("summary", "all")
  )
  expect_equal(length(ewr_out), 2)
  expect_equal(names(ewr_out), c("summary", "all_events"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_snapshot(realised_structure)
})

test_that("Single scenario among many, with access to the outer directory", {
  # create dir so building makes sense
  make_temp_hydro()

  # So far, assuming we can read and write to the outer 'hydrographs' directory, this is all standard.

  # Can we run this for a single hydro scenario?
  ewr_out <- prep_run_save_ewrs(
    hydro_dir = file.path(temp_hydro_dir, "base"),
    output_parent_dir = temp_parent_dir,
    outputType = list("summary"),
    datesuffix = FALSE,
    returnType = list("summary")
  )



  # Expect only the single output, not for all the scenarios
  expect_equal(
    list.files(file.path(temp_parent_dir, "module_output", "EWR"), recursive = TRUE),
    c("base/summary.csv", "ewr_metadata.json", "ewr_metadata.yml")
  )

  # Tear down
})

test_that("Single scenario among many, no access to the outer directory", {
  # create dir so building makes sense
  make_temp_hydro()

  # Now, let's assume all we have is a path to the specific scenario
  scenario_path <- file.path(temp_hydro_dir, "base")

  # So far, assuming we can read and write to the outer 'hydrographs' directory, this is all standard.

  # The test here is whether we can send the `output_parent_dir` the same value as `hydro_dir` to put the output inside the hydro scenario
  ewr_out <- prep_run_save_ewrs(
    hydro_dir = scenario_path,
    output_parent_dir = scenario_path,
    outputType = list("summary"),
    datesuffix = FALSE,
    returnType = list("summary")
  )



  # Expect only the single output, not for all the scenarios
  realised_structure <- list.files(scenario_path, recursive = TRUE)
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
  ewr_out <- prep_run_save_ewrs(
    hydro_dir = scenario_path,
    output_parent_dir = scenario_path,
    outputType = list("summary"),
    datesuffix = FALSE,
    returnType = list("summary")
  )



  # Expect only the single output, not for all the scenarios
  realised_structure <- list.files(scenario_path, recursive = TRUE)
  expect_snapshot(realised_structure)

  # Tear down
})

## NOTE: the python `import`s from system.file, and so while developing won't pick up
## new versions without `install`ing the package. The `future` uses seem most
## sensitive to this- I think maybe test shims don't work right?
test_that("parallel works for two", {
  # print(future::plan())
  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("summary", "yearly"),
    datesuffix = FALSE,
    returnType = list("summary", "yearly"),
    rparallel = TRUE
  )


  expect_equal(length(ewr_out), 2)
  expect_equal(names(ewr_out), c("summary", "yearly"))

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_snapshot(realised_structure)
})


test_that("parallel works for one", {
  # print(future::plan())

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("summary"),
    datesuffix = FALSE,
    returnType = list("summary"),
    rparallel = TRUE
  )

  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), "summary")

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_snapshot(realised_structure)
})

test_that("parallel works for no return", {
  # print(future::plan())

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("summary", "yearly"),
    datesuffix = FALSE,
    returnType = list("none"),
    rparallel = TRUE
  )

  expect_equal(length(ewr_out), 0)

  # Test it created the expected structure
  realised_structure <- list.files(temp_parent_dir, recursive = TRUE, include.dirs = TRUE)

  expect_snapshot(realised_structure)
})

test_that("speed test", {
  skip(message = "Speed test not really a test, just including to take advantage of setup")
  # print(future::plan())

  # create dir so building makes sense
  make_temp_hydro()

  # microbenchmark would be better, this is quick and dirty
  tp <- system.time(ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("summary", "yearly"),
    datesuffix = FALSE,
    returnType = list("summary", "yearly"),
    rparallel = TRUE
  ))

  ts <- system.time(ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("summary", "yearly"),
    datesuffix = FALSE,
    returnType = list("summary", "yearly"),
    rparallel = FALSE
  ))
  tp
  ts
})

# test multiple returns- use the next one once EWR is debugged.
test_that("safety works", {
  # create dir so building makes sense
  make_temp_hydro()

  # Create a file that should fail
  dir.create(file.path(temp_hydro_dir, "FAILURE"))
  write.csv(datasets::iris, file.path(temp_hydro_dir, "FAILURE", "iris.csv"))

  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("none"),
    datesuffix = FALSE,
    returnType = list("summary", "all")
  )

  expect_equal(length(ewr_out), 2)
  expect_true(all(c("summary", "all_events") %in% names(ewr_out)))
  expect_equal(
    unique(ewr_out$summary$scenario),
    c("base", "down4", "up4")
  )
})
