
# helper functions to make temp copies of the hydrographs directory to test with

# single csvs, each with several gauges -----------------------------------



make_temp_hydro <- function(testdir = '_test_data',
                            temp_hydro_dir = 'hydrographs',
                            orig_hydro_dir = system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')) {


  # This will throw a warning if the dir exists, which we want, since the test
  # isn't testing right if there's already something here.
  full_hydro_path <- file.path(testdir, temp_hydro_dir)
  dir.create(full_hydro_path, recursive = TRUE)
  file.copy(list.files(orig_hydro_dir, full.names = TRUE), full_hydro_path, recursive = TRUE)

  # This destroys it once used
  withr::defer_parent(unlink(testdir, recursive = TRUE))


}

make_temp_zip <- function(testdir = '_test_data',
                            temp_hydro_dir = 'hydrographs',
                            orig_hydro_zip = system.file("extdata/ncdfexample/zipcdf.zip", package = 'werptoolkitr')) {


  # This will throw a warning if the dir exists, which we want, since the test
  # isn't testing right if there's already something here.
  full_hydro_path <- file.path(testdir, temp_hydro_dir)
  dir.create(full_hydro_path, recursive = TRUE)
  file.copy(orig_hydro_zip, full_hydro_path)

  # This destroys it once used
  withr::defer_parent(unlink(testdir, recursive = TRUE))


}

# destroy_temp_hydro <- function(temp_parent_dir = '_test_data_one') {
#   unlink(temp_parent_dir, recursive = TRUE)
# }

# temporarily change the future plan
set_future_multi <- function() {
  future::plan(future::multisession)

  withr::defer_parent(future::plan('default'))
}


# multiple csvs, each with one gauge ---------------------------------------


make_temp_multifile <- function(testdir = '_test_data',
                                temp_hydro_dir = 'hydrographs',
                                orig_hydro_dir = system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')) {

  full_hydro_path <- file.path(testdir, temp_hydro_dir)
  # Make the directories
  scenepaths <- find_scenario_paths(orig_hydro_dir)

  scenenames <- names(scenepaths) |>
    stringr::str_remove('_.*')

  # scenenames <- scenario_names_from_hydro(orig_hydro_dir)

    purrr::map(scenenames, \(x) dir.create(file.path(full_hydro_path, x), recursive = TRUE))



    for (s in 1:length(scenenames)) {

      multigauges <- readr::read_csv(scenepaths[[s]])
      for (i in 2:ncol(multigauges)) {
        readr::write_csv(multigauges[, c(1, i)],
                         file = file.path(full_hydro_path, scenenames[s],
                                          paste0(names(multigauges)[i], '.csv')))
      }
    }

    # destroy after use
    withr::defer_parent(unlink(testdir, recursive = TRUE))

}

# destroy_temp_multifile <- function(temp_parent_dir = '_test_data_multi') {
#   unlink(temp_parent_dir, recursive = TRUE)
# }


# Copy over inst/yml ------------------------------------------------------

# This avoids some goofy issues with working directory
if (grepl("testthat", getwd())) {
  if (!dir.exists('yml')) {dir.create('yml')}
  file.copy(list.files(system.file("yml", package = 'werptoolkitr'), full.names = TRUE),
            to = 'yml', overwrite = TRUE)
}

