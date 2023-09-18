# Just use one of the multi_aggregate tests with the read-in bit too
project_dir <- system.file('extdata/testsmall', package = 'werptoolkitr')
hydro_dir <- file.path(project_dir, 'hydrographs')
ewr_results <- file.path(project_dir, 'module_output', 'EWR')

# Saving tends to get tested in run_toolkit_params, which is kind of silly, but doing it both places seems silly too

test_that("multi-step theme and spatial works", {

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
                          type = 'summary',
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
                          type = 'summary',
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
                          type = 'summary',
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
