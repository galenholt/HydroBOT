# Get EWR data, prep it, and aggregate it so we don't have to include it in the package and so we're always testing on up-to-date workflows

# EWR
make_test_ewr_output <- function() {
  # Set up some base directory structures
  temp_hydro_dir <- '_test_data/hydrographs'
  temp_parent_dir <- '_test_data'

  temp_hydro_multi <- '_test_data/hydrographs'
  temp_parent_multi <- '_test_data'

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'yearly'))
  return(ewr_out)
}

# prep the ewr so they look like what we'd use
make_test_ewr_prepped <- function() {
  ewr <- make_test_ewr_output()
  ewr_prepped <- prep_ewr_agg(ewr, geopath = bom_basin_gauges)

  # check that works correctly
  ewrmaps <- ewr$summary |>
    dplyr::select(gauge, planningUnit) |>
    dplyr::distinct() |>
    dplyr::mutate(gaugexpu = paste0(gauge, '_', planningUnit))

  prepmaps <- ewr_prepped |>
    dplyr::select(gauge, planning_unit_name) |>
    dplyr::distinct() |>
    dplyr::mutate(gaugexpu = paste0(gauge, '_', planning_unit_name))

  expect_true(all(ewrmaps$gaugexpu %in% prepmaps$gaugexpu) &
                all(prepmaps$gaugexpu %in% ewrmaps$gaugexpu))

  return(ewr_prepped)
}

# make a test aggregation
make_test_agg <- function(namehistory = TRUE) {
  sumspat <- make_test_ewr_prepped()

  aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                 env_obj =  c('ewr_code', "env_obj"),
                 sdl_units = sdl_units,
                 Specific_goal = c('env_obj', "Specific_goal"),
                 catchment = cewo_valleys,
                 Objective = c('Specific_goal', 'Objective'),
                 mdb = basin,
                 target_5_year_2024 = c('Objective', 'target_5_year_2024'))

  funseq <- list('CompensatingFactor',
                 'ArithmeticMean',
                 'ArithmeticMean',
                 c('ArithmeticMean', 'LimitingFactor'),
                 list(wm = ~weighted.mean(., w = area,
                                          na.rm = TRUE)),
                 'ArithmeticMean',
                 list(wm = ~weighted.mean(., w = area,
                                          na.rm = TRUE)),
                 'ArithmeticMean')

  # Expect_warning because sf throws a warning about spatially constant attributes. and it gets thrown multiple times
  agg_theme_space <- multi_aggregate(sumspat,
                                     aggsequence = aggseq,
                                     groupers = 'scenario',
                                     aggCols = 'ewr_achieved',
                                     funsequence = funseq,
                                     causal_edges = causal_ewr,
                                     namehistory = namehistory,
                                     saveintermediate = TRUE)

  return(agg_theme_space)
}


