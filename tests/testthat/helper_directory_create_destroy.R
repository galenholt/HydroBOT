
# helper functions to make temp copies of the hydrographs directory to test with

# single csvs, each with several gauges -----------------------------------



make_temp_hydro <- function(temp_hydro_dir = '_test_data/temp_one/hydrographs',
                            orig_hydro_dir = system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')) {


  # This will throw a warning if the dir exists, which we want, since the test
  # isn't testing right if there's already something here.
  dir.create(temp_hydro_dir, recursive = TRUE)
  file.copy(list.files(orig_hydro_dir, full.names = TRUE), temp_hydro_dir, recursive = TRUE)
}

destroy_temp_hydro <- function(temp_parent_dir = '_test_data/temp_one') {
  unlink(temp_parent_dir, recursive = TRUE)
}



# multiple csvs, each with one gauge ---------------------------------------


make_temp_multifile <- function(temp_hydro_multi = '_test_data/temp_multi/hydrographs',
                                orig_hydro_dir = system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')) {

  # Make the directories
  scenenames <- scenario_names_from_hydro(orig_hydro_dir)

    purrr::map(scenenames, \(x) dir.create(file.path(temp_hydro_multi, x), recursive = TRUE))

    scenepaths <- find_scenario_paths(orig_hydro_dir)

    for (s in 1:length(scenenames)) {

      multigauges <- readr::read_csv(scenepaths[s])
      for (i in 2:ncol(multigauges)) {
        readr::write_csv(multigauges[, c(1, i)],
                         file = file.path(temp_hydro_multi, scenenames[s],
                                          paste0(names(multigauges)[i], '.csv')))
      }
    }
}

destroy_temp_multifile <- function(temp_parent_dir = '_test_data/temp_multi') {
  unlink(temp_parent_dir, recursive = TRUE)
}


# Copy over inst/yml ------------------------------------------------------

# This avoids some goofy issues with working directory
if (grepl("testthat", getwd())) {
  if (!dir.exists('yml')) {dir.create('yml')}
  file.copy(list.files(system.file("yml", package = 'werptoolkitr'), full.names = TRUE),
            to = 'yml', overwrite = TRUE)
}

