# Get EWR data, prep it, and aggregate it so we don't have to include it in the package and so we're always testing on up-to-date workflows

# EWR
make_test_ewr_output <- function(build_dirs = TRUE) {
  # Set up some base directory structures
  temp_hydro_dir <- "_test_data/hydrographs"
  temp_parent_dir <- "_test_data"

  temp_hydro_multi <- "_test_data/hydrographs"
  temp_parent_multi <- "_test_data"


  if (build_dirs) {
    # create dir so building makes sense
    make_temp_hydro()
  }


  ewr_out <- prep_run_save_ewrs(
    hydro_dir = temp_hydro_dir,
    output_parent_dir = temp_parent_dir,
    outputType = list("none"),
    datesuffix = FALSE,
    returnType = list("summary", "yearly")
  )
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
    dplyr::mutate(gaugexpu = paste0(gauge, "_", planningUnit))

  prepmaps <- ewr_prepped |>
    dplyr::select(gauge, planning_unit_name) |>
    dplyr::distinct() |>
    dplyr::mutate(gaugexpu = paste0(gauge, "_", planning_unit_name))

  expect_true(all(ewrmaps$gaugexpu %in% prepmaps$gaugexpu) &
    all(prepmaps$gaugexpu %in% ewrmaps$gaugexpu))

  return(ewr_prepped)
}

# make a test aggregation
make_test_agg <- function(namehistory = TRUE, style = 'PU') {
  sumspat <- make_test_ewr_prepped()

  # Respects planning units, as in an analysis, but often worse for simple tests.
  if (style == 'PU') {
    # Group_until and other sorts of agg tests are done separately in the relevant file tests
    aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                   planning_units = planning_units,
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
                   'SpatialWeightedMean',
                   "ArithmeticMean",
                   list(wm = ~weighted.mean(., w = area,
                                            na.rm = TRUE)),
                   'ArithmeticMean',
                   list(wm = ~weighted.mean(., w = area,
                                            na.rm = TRUE)),
                   'ArithmeticMean')
  }

  # This ignores planning units, which yields much better data for testing plots, even if it's not what we would do in an analysis.
  if (style == 'noPU') {
    # Group_until and other sorts of agg tests are done separately in the relevant file tests
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
                   "ArithmeticMean",
                   list(wm = ~weighted.mean(., w = area,
                                            na.rm = TRUE)),
                   'ArithmeticMean',
                   list(wm = ~weighted.mean(., w = area,
                                            na.rm = TRUE)),
                   'ArithmeticMean')
  }


  agg_theme_space <- multi_aggregate(sumspat,
                                     aggsequence = aggseq,
                                     groupers = c('scenario', 'planning_unit_name', 'gauge'),
                                     group_until = list(planning_unit_name = is_notpoint, gauge = is_notpoint),
                                     aggCols = 'ewr_achieved',
                                     funsequence = funseq,
                                     causal_edges = causal_ewr,
                                     namehistory = namehistory,
                                     pseudo_spatial = 'planning_units',
                                     saveintermediate = TRUE)

  return(agg_theme_space)
}
