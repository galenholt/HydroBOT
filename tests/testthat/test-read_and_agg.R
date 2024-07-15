temp_hydro_dir <- "_test_data/hydrographs"
temp_parent_dir <- "_test_data"

# build a test set
# create dir so building makes sense
make_temp_hydro()

# all_interEvents is breaking in 1.0.6 EWR tool, so skip for now.

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
  datesuffix = FALSE,
  returnType = ewroutlist
)

ewr_results <- file.path(temp_parent_dir, "module_output", "EWR")

# Saving tends to get tested in run_toolkit_params, which is kind of silly, but doing it both places seems silly too

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
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "target_5_year_2024",
    "target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 300)

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

  spatagg_s_th_t <- read_and_agg(
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
    saveintermediate = TRUE
  )

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

  spatagg_s_t_th <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_s_t_th,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE
  )

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

  spatagg_th_s_t <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_th_s_t,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE
  )

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

  spatagg_th_t_s <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_th_t_s,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE
  )

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


  spatagg_t_s_th <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_t_s_th,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE
  )

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


  spatagg_t_th_s <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    aggsequence = aggseq_t_th_s,
    funsequence = funseq,
    keepAllPolys = FALSE,
    auto_ewr_PU = TRUE,
    saveintermediate = TRUE
  )

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
  expect_equal(names(spatagg), c('ewr_code_timing', names(aggseq)))
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
    "scenario", "polyID", "target_5_year_2024",
    "target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 300)
})

test_that("parsing bare and char and rlang::quo for funsequence", {
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
    "target_5_year_2024_ArithmeticMean_mdb_wm_Objective_ArithmeticMean_Specific_goal_mean_sdl_units_mean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved",
    "target_5_year_2024_LimitingFactor_mdb_wm_Objective_ArithmeticMean_Specific_goal_mean_sdl_units_mean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 300)
})

test_that("Various group_until formats work", {
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
  spatagg <- read_and_agg(
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
  )

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$agg_group_until, list(planning_unit_name = 3))
  expect_equal(yamout$auto_ewr_PU, FALSE)

  # Vector- this relies on multi_aggregate to make it a list
  spatagg <- read_and_agg(
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
  )

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$agg_group_until, list(planning_unit_name = 3))
  expect_equal(yamout$auto_ewr_PU, FALSE)

  # function. As with funsequence and agg sequence, these don't actually have
  # to be the same as the inputs, they just have to evaluate the same.- ie
  # characters instead of sf names
  spatagg <- read_and_agg(
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
  )

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$agg_group_until, list(planning_unit_name = 3))
  expect_equal(yamout$auto_ewr_PU, FALSE)

  # List, and numeric. This is what would get read from params. (though that could have character too.)
  # also drop it from groupers to check that that works
  spatagg <- read_and_agg(
    datpath = ewr_results,
    type = "achievement",
    geopath = bom_basin_gauges,
    causalpath = causal_ewr,
    groupers = c("scenario"),
    group_until = list(planning_unit_name = 3),
    aggCols = "ewr_achieved",
    aggsequence = aggseq,
    funsequence = funseq,
    keepAllPolys = FALSE,
    savepath = file.path(temp_parent_dir, "aggregated")
  )

  yamout <- yaml::read_yaml(file.path(temp_parent_dir, "aggregated", "agg_metadata.yml"))
  expect_equal(yamout$agg_group_until, list(planning_unit_name = 3))
  expect_equal(yamout$auto_ewr_PU, FALSE)
})
