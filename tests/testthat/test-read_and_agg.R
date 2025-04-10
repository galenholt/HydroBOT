temp_hydro_dir <- "_test_data/hydrographs"
temp_parent_dir <- "_test_data"

# build a test set
# create dir so building makes sense
make_temp_hydro()

# all_interEvents is breaking in 1.0.6 EWR tool, so skip for now. We don't tend
# to use it anyway

ewroutlist <- list(
  "summary",
  "yearly",
  "all_events",
  "all_successful_events",
  # 'all_interEvents',
  "all_successful_interEvents"
)

ewr_out <- prep_run_save_ewrs(
  hydro_dir = temp_hydro_dir,
  output_parent_dir = temp_parent_dir,
  outputType = ewroutlist,
  returnType = ewroutlist
)

ewr_results <- file.path(temp_parent_dir, "module_output", "EWR")

# Saving tends to get tested in run_hydrobot_params, which is kind of silly, but doing it both places seems silly too

test_that("multi-step theme and spatial works", {

  skip_on_os('linux')

  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units,
    Specific_goal = c("env_obj", "Specific_goal"),
    catchment = cewo_valleys,
    Objective = c("Specific_goal", "Objective"),
    mdb = basin,
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
  )

  spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
    pseudo_spatial = 'sdl_units'
  )

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", 'date', "polyID", "target_5_year_2024",
    "target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 1425)

  # Plots are useful for checking spatial outcomes.
  # There are a million targets. Pick one
  target_5 <- spatagg |>
    dplyr::filter(target_5_year_2024 == "Annual detection of species and life stages representative of the whole fish community through key fish passages in specified planning units")
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = target_5,
      ggplot2::aes(fill = target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved)
    ) +
    # ggplot2::geom_sf(data = sumspat) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "none")

  vdiffr::expect_doppelganger("spatial-theme multi withreadin", g2sdl_plot)
})


test_that("multi-step theme-spatial-time", {

  # These mirror multi_aggregate, but make sure we're not introducing more issues here.
  aggseq_s_th_t <- list(sdl_units = 'sdl_units',
                 ewr_code = c('ewr_code_timing', 'ewr_code'),
                 yrs = '2 years')

  aggseq_s_t_th <- list(sdl_units = 'sdl_units',
                        yrs = '2 years',
                        ewr_code = c('ewr_code_timing', 'ewr_code'))

  aggseq_th_s_t <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                        sdl_units = 'sdl_units',
                        yrs = '2 years')

  aggseq_th_t_s <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                        yrs = '2 years',
                        sdl_units = 'sdl_units')

  aggseq_t_s_th <- list(yrs = '2 years',
                        sdl_units = 'sdl_units',
                        ewr_code = c('ewr_code_timing', 'ewr_code'))

  aggseq_t_th_s <- list(yrs = '2 years',
                        ewr_code = c('ewr_code_timing', 'ewr_code'),
                        sdl_units = 'sdl_units')



  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
    )

  # Warnings because we are spatailly moving to sdl units to test spatial agg
  expect_warning(spatagg_s_th_t <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_th_t,
    funsequence = funseq,
    group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
    keepAllPolys = FALSE,
    auto_ewr_PU = FALSE,
    saveintermediate = TRUE
  ))

  # the sdl sheet should have all the dates and code_timings
  expect_snapshot_value(spatagg_s_th_t$sdl_units$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_th_t$sdl_units$ewr_code_timing |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_th_t$sdl_units$SWSDLID |> unique(), style = 'deparse')

  # The ewr_code sheet should have ewr_codes, sdls, and all dates
  expect_snapshot_value(spatagg_s_th_t$ewr_code$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_th_t$ewr_code$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_th_t$ewr_code$SWSDLID |> unique(), style = 'deparse')

  # The yrs sheet should have ewr_codes, sdls, and two-year intervals
  expect_snapshot_value(spatagg_s_th_t$yrs$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_th_t$yrs$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_th_t$yrs$SWSDLID |> unique(), style = 'deparse')

  expect_warning(spatagg_s_t_th <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_t_th,
    funsequence = funseq,
    keepAllPolys = FALSE,
    group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
    auto_ewr_PU = FALSE,
    saveintermediate = TRUE
  ))

  # the sdl sheet should have all the dates and code_timings
  expect_snapshot_value(spatagg_s_t_th$sdl_units$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_t_th$sdl_units$ewr_code_timing |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_t_th$sdl_units$SWSDLID |> unique(), style = 'deparse')

  # The yrs sheet should have code_timing, sdl, and two-year intervals
  expect_snapshot_value(spatagg_s_t_th$yrs$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_t_th$yrs$ewr_code_timing |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_t_th$yrs$SWSDLID|> unique(), style = 'deparse')

  # The ewr_code sheet should have ewr_codes, sdls, and 2-year
  expect_snapshot_value(spatagg_s_t_th$ewr_code$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_t_th$ewr_code$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_s_t_th$ewr_code$SWSDLID |> unique(), style = 'deparse')

  expect_warning(spatagg_th_s_t <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_th_s_t,
    funsequence = funseq,
    keepAllPolys = FALSE,
    group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
    auto_ewr_PU = FALSE,
    saveintermediate = TRUE
  ))

  # The ewr_code sheet should have ewr_codes, planning units (not sdls), and all dates
  expect_snapshot_value(spatagg_th_s_t$ewr_code$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_s_t$ewr_code$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_s_t$ewr_code$planning_unit_name |> unique(), style = 'deparse')

  # the sdl sheet should have all times and ewr_codes
  expect_snapshot_value(spatagg_th_s_t$sdl_units$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_s_t$sdl_units$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_s_t$sdl_units$SWSDLID |> unique(), style = 'deparse')

  # The yrs sheet should have ewr_codes, sdls, and two-year intervals
  expect_snapshot_value(spatagg_th_s_t$yrs$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_s_t$yrs$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_s_t$yrs$SWSDLID |> unique(), style = 'deparse')

  expect_warning(spatagg_th_t_s <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_th_t_s,
    funsequence = funseq,
    keepAllPolys = FALSE,
    group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
    auto_ewr_PU = FALSE,
    saveintermediate = TRUE
  ))

  # The ewr_code sheet should have ewr_codes, planning units (not sdls), and all dates
  expect_snapshot_value(spatagg_th_t_s$ewr_code$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_t_s$ewr_code$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_t_s$ewr_code$planning_unit_name |> unique(), style = 'deparse')

  # The yrs sheet should have ewr_codes, planning units, and two-year intervals
  expect_snapshot_value(spatagg_th_t_s$yrs$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_t_s$yrs$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_t_s$yrs$planning_unit_name |> unique(), style = 'deparse')

  # the sdl sheet should have 2-year intervals and ewr_codes
  expect_snapshot_value(spatagg_th_t_s$sdl_units$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_t_s$sdl_units$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_th_t_s$sdl_units$SWSDLID |> unique(), style = 'deparse')


  expect_warning(spatagg_t_s_th <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_t_s_th,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = FALSE,
    group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
    saveintermediate = TRUE
  ))

  # The yrs sheet should have code_timing, planning units, and two-year intervals
  expect_snapshot_value(spatagg_t_s_th$yrs$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_s_th$yrs$ewr_code_timing |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_s_th$yrs$planning_unit_name |> unique(), style = 'deparse')

  # the sdl sheet should have 2-year intervals and code_timing
  expect_snapshot_value(spatagg_t_s_th$sdl_units$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_s_th$sdl_units$ewr_code_timing |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_s_th$sdl_units$SWSDLID |> unique(), style = 'deparse')

  # The ewr_code sheet should have ewr_codes, sdls, and 2-year
  expect_snapshot_value(spatagg_t_s_th$ewr_code$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_s_th$ewr_code$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_s_th$ewr_code$SWSDLID |> unique(), style = 'deparse')


  expect_warning(spatagg_t_th_s <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_t_th_s,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = FALSE,
    group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
    saveintermediate = TRUE
  ))

  # The yrs sheet should have code_timing, planning units, and two-year intervals
  expect_snapshot_value(spatagg_t_th_s$yrs$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_th_s$yrs$ewr_code_timing |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_th_s$yrs$planning_unit_name |> unique(), style = 'deparse')

  # The ewr_code sheet should have ewr_codes, planning_untois, and 2-year
  expect_snapshot_value(spatagg_t_th_s$ewr_code$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_th_s$ewr_code$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_th_s$ewr_code$planning_unit_name |> unique(), style = 'deparse')

  # the sdl sheet should have 2-year intervals and ewr_code
  expect_snapshot_value(spatagg_t_th_s$sdl_units$date |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_th_s$sdl_units$ewr_code |> unique(), style = 'deparse')
  expect_snapshot_value(spatagg_t_th_s$sdl_units$SWSDLID |> unique(), style = 'deparse')

})


test_that("nonspatial joins of spatial data (as in multi_agg)", {
  # this is copied over from multi_aggregate testing, just making sure things pass correctly.
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    planning_units = planning_units,
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units,
    Specific_goal = c("env_obj", "Specific_goal"),
    catchment = cewo_valleys,
    Objective = c("Specific_goal", "Objective"),
    mdb = basin,
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "SpatialWeightedMean",
    "ArithmeticMean",
    "SpatialWeightedMean",
    "ArithmeticMean",
    "SpatialWeightedMean",
    "ArithmeticMean"
  )

  spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    saveintermediate = TRUE,
    namehistory = FALSE,
    auto_ewr_PU = TRUE,
    pseudo_spatial = "planning_units"
  )

  # stringr::str_flatten(names(spatagg), "', '")
  expect_equal(names(spatagg), c('agg_input', names(aggseq)))
  expect_s3_class(spatagg$planning_units, "sf")

  # Check the values are actually right
  funs_in_df <- spatagg$target_5_year_2024 |>
    sf::st_drop_geometry() |>
    dplyr::slice(1) |>
    dplyr::select(tidyselect::starts_with('aggfun_')) |>
    unlist()

  aggs_in_df <- spatagg$target_5_year_2024 |>
    sf::st_drop_geometry() |>
    dplyr::slice(1) |>
    dplyr::select(tidyselect::starts_with('aggLevel_')) |>
    unlist()


  expect_equal(unname(funs_in_df), unlist(funseq))
  expect_equal(unname(aggs_in_df), names(aggseq))


  ## This test doesn't cover some corner cases because the test data doesn't have any PUs with multiple gauges. But it does have multiple PUs per a couple gauges. So, it should have the same number of rows as the non-PU version, just differently indexed. And the ones from the same gauge should have the same value.
  # for testing, in multi_aggregate after fromto_pair:
  # fromto_pair |> dplyr::group_by(gauge) |> dplyr::reframe(gauge = unique(planning_unit_name))
  # fromto_pair |> dplyr::group_by(gauge) |> dplyr::reframe(pun = unique(planning_unit_name))
  expect_equal(nrow(spatagg$ewr_code), nrow(spatagg$planning_units))
  # This is *very* specific to test data, so if that ever changes, this might too
  gauge412005 <- spatagg$ewr_code |>
    sf::st_drop_geometry() |>
    dplyr::filter(scenario == 'base' & gauge == 412005) |>
    dplyr::select(scenario, planning_unit_name, ewr_code, ewr_achieved) |>
    dplyr::arrange(planning_unit_name, ewr_code, ewr_achieved)


  for (i in unique(gauge412005$planning_unit_name)) {
    pudf <- spatagg$planning_units |>
      sf::st_drop_geometry() |>
      dplyr::filter(scenario == 'base' & planning_unit_name == i) |>
      dplyr::select(scenario, planning_unit_name, ewr_code, ewr_achieved) |>
      dplyr::arrange(planning_unit_name, ewr_code, ewr_achieved)
    expect_equal(gauge412005 |> dplyr::filter(planning_unit_name == i), pudf)
  }
})

test_that("parsing geo and char work for aggsequence", {
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = "sdl_units",
    Specific_goal = c("env_obj", "Specific_goal"),
    Objective = c("Specific_goal", "Objective"),
    mdb = basin,
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
  )

  spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "date", "polyID", "target_5_year_2024",
    "target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 1406)
})

test_that("parsing bare and char and rlang::quo for funsequence", {
  aggseq <- list(
    all_time = 'all_time',
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = "sdl_units",
    Specific_goal = c("env_obj", "Specific_goal"),
    Objective = c("Specific_goal", "Objective"),
    mdb = basin,
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    list(mean = ~ mean(., na.rm = TRUE)),
    "list(mean = ~mean(., na.rm = TRUE))",
    "ArithmeticMean",
    rlang::quo(list(wm = ~ weighted.mean(., w = area, na.rm = TRUE))),
    c("ArithmeticMean", "LimitingFactor")
  )

  spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "target_5_year_2024",
    "target_5_year_2024_ArithmeticMean_mdb_wm_Objective_ArithmeticMean_Specific_goal_mean_sdl_units_mean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_all_time_ArithmeticMean_ewr_achieved",
    "target_5_year_2024_LimitingFactor_mdb_wm_Objective_ArithmeticMean_Specific_goal_mean_sdl_units_mean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_all_time_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 296)
})

test_that("Various group_until formats work", {
  aggseq <- list(
    all_time = 'all_time',
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units,
    Specific_goal = c("env_obj", "Specific_goal"),
    catchment = cewo_valleys,
    Objective = c("Specific_goal", "Objective"),
    mdb = basin,
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
  )

  # Does it save with auto_ewr_PU
  spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    savepath = file.path(temp_parent_dir, "aggregated")
  )

  # Can we parse group_until into yaml if it's a named list?
  # expect_warnings because of the desired non-spatial sdl
  expect_warning(expect_warning(expect_warning(spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = c("scenario", "planning_unit_name"),
    group_until = list(planning_unit_name = "sdl_units"),
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    savepath = file.path(temp_parent_dir, "aggregated")
  ))))

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$aggregation$group_until, list(planning_unit_name = 4))
  expect_equal(yamout$aggregation$auto_ewr_PU, FALSE)

  # Vector- this relies on multi_aggregate to make it a list
  # expect_warnings because of the desired non-spatial sdl
  expect_warning(expect_warning(expect_warning(spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = c("scenario", "planning_unit_name"),
    group_until = c(NA, "sdl_units"),
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    savepath = file.path(temp_parent_dir, "aggregated")
  ))))

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$aggregation$group_until, list(planning_unit_name = 4))
  expect_equal(yamout$aggregation$auto_ewr_PU, FALSE)

  # function. As with funsequence and agg sequence, these don't actually have
  # to be the same as the inputs, they just have to evaluate the same.- ie
  # characters instead of sf names
  # expect_warnings because of the desired non-spatial sdl
  expect_warning(expect_warning(expect_warning(spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = c("scenario", "planning_unit_name"),
    group_until = c(NA, is_notpoint),
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    savepath = file.path(temp_parent_dir, "aggregated")
  ))))

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$aggregation$group_until, list(planning_unit_name = 4))
  expect_equal(yamout$aggregation$auto_ewr_PU, FALSE)

  # List, and numeric. This is what would get read from params. (though that could have character too.)
  # also drop it from groupers to check that that works
  # expect_warnings because of the desired non-spatial sdl
  expect_warning(expect_warning(expect_warning(spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = c("scenario"),
    group_until = list(planning_unit_name = 4),
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    savepath = file.path(temp_parent_dir, "aggregated")
  ))))

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$aggregation$group_until, list(planning_unit_name = 4))
  expect_equal(yamout$aggregation$auto_ewr_PU, FALSE)
})


test_that("parallel works", {

  set_future_multi()

  # These mirror multi_aggregate, but make sure we're not introducing more issues here.
  aggseq_s_th_t <- list(
    sdl_units = 'sdl_units',
    ewr_code = c('ewr_code_timing', 'ewr_code'),
    yrs = '2 years'
  )

  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
  )

  dir.create(file.path(temp_parent_dir, 'aggout/parallel'), recursive = TRUE)
  dir.create(file.path(temp_parent_dir, 'aggout/parallel_par'), recursive = TRUE)
  dir.create(file.path(temp_parent_dir, 'aggout/parallel_combine'), recursive = TRUE)
  dir.create(file.path(temp_parent_dir, 'aggout/parallel_par_combine'), recursive = TRUE)
  dir.create(file.path(temp_parent_dir, 'aggout/sequential'), recursive = TRUE)

  spatagg_pt <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_th_t,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE,
    savepath = file.path(temp_parent_dir, 'aggout/parallel'),
    rparallel = TRUE,
    par_recursive = TRUE,
    savepar = 'each'
  )

  spatagg_pf <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_th_t,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE,
    savepath = file.path(temp_parent_dir, 'aggout/parallel_par'),
    rparallel = TRUE,
    par_recursive = FALSE,
    savepar = 'each'
  )

  spatagg_np <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_th_t,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE,
    savepath = file.path(temp_parent_dir, 'aggout/sequential'),
    rparallel = FALSE,
    par_recursive = FALSE
  )

  spatagg_pt_c <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_th_t,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE,
    savepath = file.path(temp_parent_dir, 'aggout/parallel_combine'),
    rparallel = TRUE,
    par_recursive = TRUE,
    savepar = 'combine'
  )

  spatagg_pf_c <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_th_t,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE,
    savepath = file.path(temp_parent_dir, 'aggout/parallel_par_combine'),
    rparallel = TRUE,
    par_recursive = FALSE,
    savepar = 'combine'
  )

  # The scenarios get jumbled, but should be identical
  expect_equal(spatagg_np$agg_input |> dplyr::arrange(scenario),
               spatagg_pf$agg_input |> dplyr::arrange(scenario))
  expect_equal(spatagg_np$agg_input |> dplyr::arrange(scenario),
               spatagg_pt$agg_input |> dplyr::arrange(scenario))

  # The `savepar = 'each'` saves one file per scenario, read them in and glue together to test.
  ptfiles <- list.files(file.path(temp_parent_dir, 'aggout/parallel'),
                        pattern = '.rds',
                        full.names = TRUE, recursive = TRUE)
  read_pt <- purrr::map(ptfiles, readRDS) |>
    purrr::list_transpose() |>
    purrr::map(dplyr::bind_rows)

  pffiles <- list.files(file.path(temp_parent_dir, 'aggout/parallel'),
                        pattern = '.rds',
                        full.names = TRUE, recursive = TRUE)
  read_pf <- purrr::map(pffiles, readRDS) |>
    purrr::list_transpose() |>
    purrr::map(dplyr::bind_rows)

  # just read in the sequential and the combineds
  read_np <- readRDS(file.path(temp_parent_dir, 'aggout/sequential', 'achievement_aggregated.rds'))
  read_ptc <- readRDS(file.path(temp_parent_dir, 'aggout/parallel_combine', 'achievement_aggregated.rds'))
  read_pfc <- readRDS(file.path(temp_parent_dir, 'aggout/parallel_par_combine', 'achievement_aggregated.rds'))


  expect_equal(spatagg_np, read_np)
  expect_equal(spatagg_pf, read_pf)
  expect_equal(spatagg_pt, read_pt)
  expect_equal(spatagg_pf, read_pfc)
  expect_equal(spatagg_pt, read_ptc)

  })

# Non-module data ------------------------------------------------------------

test_that('non-module works', {

  # Similar to multi_aggregate, but have saved out the dummy data there

  # setup
  austates <- readRDS(test_path("test_data", "austates.rds"))
  all_aus <- readRDS(test_path("test_data", "all_aus.rds"))

  # make a simple 'causal' network
  state_theme <- tibble::tibble(theme1 = c("E", "F", "G", "H", "I", "J"),
                                theme2 = c("vowel", "consonant", "consonant",
                                           "consonant", "vowel", "consonant")) |>
    list()

  # This will aggregate into weeks, then to type, and then to the country.
  ausseq <- list(
    week = 'week',
    theme2 = c('theme1', 'theme2'),
    all_aus = all_aus
  )

  # just use mean, since there are no NA in the data.
  ausfuns <- list(
    week = 'mean',
    type = 'mean',
    all_aus = 'mean'
  )

  # Do the aggregation
expect_warning(ausagg <- read_and_agg(
  datpath = test_path("test_data", "module_output", "fake_module"),
  type = "everything",
  geopath = austates,
  causalpath = state_theme,
  groupers = "scenario",
  aggCols = "value",
    aggsequence = ausseq,
  funsequence = ausfuns,
  saveintermediate = TRUE,
  namehistory = FALSE,
  keepAllPolys = FALSE,
  returnList = TRUE,
  add_max = FALSE
))

  weekcheck <- ausagg$week |>
    dplyr::filter(theme1 == 'E') |>
    plot_outcomes(
      outcome_col = "value",
      plot_type = "map",
      colorgroups = NULL,
      colorset = "value",
      pal_list = list("scico::berlin"),
      pal_direction = -1,
      facet_col = "scenario",
      facet_row = "date"
    )

  themecheck <- ausagg$theme2 |>
    dplyr::filter(date == lubridate::ymd("2000-01-03")) |>
    plot_outcomes(
      outcome_col = "value",
      plot_type = "map",
      colorgroups = NULL,
      colorset = "value",
      pal_list = list("scico::berlin"),
      pal_direction = -1,
      facet_col = "scenario",
      facet_row = "theme2"
    )

  spacecheck <- ausagg$all_aus |>
    dplyr::filter(date == lubridate::ymd("2000-01-03")) |>
    plot_outcomes(
      outcome_col = "value",
      plot_type = "map",
      colorgroups = NULL,
      colorset = "value",
      pal_list = list("scico::berlin"),
      pal_direction = -1,
      facet_col = "scenario",
      facet_row = "theme2"
    )
  vdiffr::expect_doppelganger("aus_weekcheck", weekcheck)
  vdiffr::expect_doppelganger("aus_themecheck", themecheck)
  vdiffr::expect_doppelganger("aus_spacecheck", spacecheck)
})

