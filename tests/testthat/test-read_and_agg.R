
temp_hydro_dir = '_test_data/temp/hydrographs'
temp_parent_dir = '_test_data/temp'

# build a test set
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

ewr_results <- file.path(temp_parent_dir, 'module_output', 'EWR')

# Saving tends to get tested in run_toolkit_params, which is kind of silly, but doing it both places seems silly too

test_that("multi-step theme and spatial works", {

  # check the OS
  os_name <- tolower(Sys.info()["sysname"])

  # skipping on ubuntu systems
  if (os_name == "ubuntu") {
    message("Skipping test on Ubuntu systems")
    skip()
  }

  aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                 env_obj =  c('ewr_code', "env_obj"),
                 sdl_units = sdl_units,
                 Specific_goal = c('env_obj', "Specific_goal"),
                 catchment = cewo_valleys,
                 Objective = c('Specific_goal', 'Objective'),
                 mdb = basin,
                 target_5_year_2024 = c('Objective', 'target_5_year_2024'))

  funseq <- list('ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean')

  spatagg <- read_and_agg(datpath = ewr_results,
                          type = 'achievement',
                          geopath = bom_basin_gauges,
                          causalpath = causal_ewr,
                          groupers = 'scenario',
                          aggCols = 'ewr_achieved',
                          aggsequence = aggseq,
                          funsequence = funseq,
                          keepAllPolys = FALSE)

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'target_5_year_2024',
                  'target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved',
                  'OBJECTID', 'DDIV_NAME', 'AREA_HA', 'SHAPE_AREA', 'SHAPE_LEN',
                  'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 228)

  # Plots are useful for checking spatial outcomes.
  # There are a million targets. Pick one
  target_5 <- spatagg |>
    dplyr::filter(target_5_year_2024 == "Annual detection of species and life stages representative of the whole fish community through key fish passages in specified planning units")
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = target_5,
                     ggplot2::aes(fill = target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved)) +
    # ggplot2::geom_sf(data = sumspat) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("spatial-theme multi withreadin", g2sdl_plot)
})

test_that("parsing geo and char work for aggsequence", {

  aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                 env_obj =  c('ewr_code', "env_obj"),
                 sdl_units = "sdl_units",
                 Specific_goal = c('env_obj', "Specific_goal"),
                 Objective = c('Specific_goal', 'Objective'),
                 mdb = basin,
                 target_5_year_2024 = c('Objective', 'target_5_year_2024'))

  funseq <- list('ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 'ArithmeticMean')

  spatagg <- read_and_agg(datpath = ewr_results,
                          type = 'achievement',
                          geopath = bom_basin_gauges,
                          causalpath = causal_ewr,
                          groupers = 'scenario',
                          aggCols = 'ewr_achieved',
                          aggsequence = aggseq,
                          funsequence = funseq,
                          keepAllPolys = FALSE)

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'target_5_year_2024',
                  'target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved',
                  'OBJECTID', 'DDIV_NAME', 'AREA_HA', 'SHAPE_AREA', 'SHAPE_LEN',
                  'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 228)
})

test_that("parsing bare and char and rlang::quo for funsequence", {

  aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                 env_obj =  c('ewr_code', "env_obj"),
                 sdl_units = "sdl_units",
                 Specific_goal = c('env_obj', "Specific_goal"),
                 Objective = c('Specific_goal', 'Objective'),
                 mdb = basin,
                 target_5_year_2024 = c('Objective', 'target_5_year_2024'))

  funseq <- list('ArithmeticMean',
                 'ArithmeticMean',
                 list(mean = ~mean(., na.rm = TRUE)),
                 'list(mean = ~mean(., na.rm = TRUE))',
                 'ArithmeticMean',
                 rlang::quo(list(wm = ~weighted.mean(., w = area, na.rm = TRUE))),
                 c('ArithmeticMean', 'LimitingFactor'))

  spatagg <- read_and_agg(datpath = ewr_results,
                          type = 'achievement',
                          geopath = bom_basin_gauges,
                          causalpath = causal_ewr,
                          groupers = 'scenario',
                          aggCols = 'ewr_achieved',
                          aggsequence = aggseq,
                          funsequence = funseq,
                          keepAllPolys = FALSE)

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'target_5_year_2024',
                  'target_5_year_2024_ArithmeticMean_mdb_wm_Objective_ArithmeticMean_Specific_goal_mean_sdl_units_mean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved',
                  'target_5_year_2024_LimitingFactor_mdb_wm_Objective_ArithmeticMean_Specific_goal_mean_sdl_units_mean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved',
                  'OBJECTID', 'DDIV_NAME', 'AREA_HA', 'SHAPE_AREA', 'SHAPE_LEN',
                  'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 228)
})
