### First, test that it works as a wrapper of theme_aggregate and
### multi_aggregate- it should pass all their tests with minor format edits:
# edits- change function name, turn aggregation and functions into lists
ewr_to_agg <- make_test_ewr_prepped()

non_spatial_ewrout <- ewr_to_agg |> sf::st_drop_geometry()

# Tests from theme_aggregate ----------------------------------------------

test_that("ewr-obj works, nongeom", {
  # no need to load the demo/test data since it's in /data
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    causal_edges = make_edges(causal_ewr, list(c("ewr_code_timing", "ewr_code"))),
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("auto-generating causal_edges works", {
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("spatial input data works", {
  agged <- multi_aggregate(ewr_to_agg,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c(
    "scenario", "gauge", "polyID", "planning_unit_name",
    "ewr_code", "ewr_code_mean_ewr_achieved",
    "geometry"
  ))
  expect_s3_class(agged, "data.frame")
  expect_s3_class(agged, "sf")

  # geonames can't be user-set because multi_agg tracks all of them
})

test_that("bare functions", {
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list(mean),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("list functions", {
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list(list(mean = ~ mean(., na.rm = TRUE))),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("multiple functions", {
  # Character
  agged_c <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list(c("mean", "sd")),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_c), c(
    "scenario", "gauge", "planning_unit_name", "ewr_code",
    "ewr_code_mean_ewr_achieved", "ewr_code_sd_ewr_achieved"
  ))
  expect_s3_class(agged_c, "data.frame")

  # bare
  agged_b <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list(c(mean, sd)),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_b), c(
    "scenario", "gauge", "planning_unit_name", "ewr_code",
    "ewr_code_mean_ewr_achieved", "ewr_code_sd_ewr_achieved"
  ))
  expect_s3_class(agged_b, "data.frame")

  # List
  agged_l <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = list(list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    )),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_l), c(
    "scenario", "gauge", "planning_unit_name", "ewr_code",
    "ewr_code_mean_ewr_achieved", "ewr_code_sd_ewr_achieved"
  ))
  expect_s3_class(agged_l, "data.frame")
})



# Tests from spatial_aggregate --------------------------------------------

test_that("gauge to poly works", {

  skip_on_os('linux')

  # should error if not list, to keep from iterating over sdl_units itself
  expect_error(multi_aggregate(ewr_to_agg,
    aggsequence = sdl_units,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean")
  ))

  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean")
  )
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "sdl_units_mean_ewr_achieved",
    "SWSDLID", "SWSDLName", "StateID", "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 6)

  # Keeping the whole set of polys
  spataggkeep <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    keepAllPolys = TRUE
  )
  # stringr::str_flatten(names(spatagg), "', '")
  # namestring <- c('scenario', 'polyID', 'sdl_units_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spataggkeep), namestring)
  expect_s3_class(spataggkeep, "sf")
  expect_equal(nrow(spataggkeep), nrow(sdl_units) * length(unique(ewr_to_agg$scenario)))

  # Plots are useful for checking spatial outcomes
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spatagg,
      ggplot2::aes(fill = sdl_units_mean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  g2sdl_all_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spataggkeep,
      ggplot2::aes(fill = sdl_units_mean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("gauge to sdl multi", g2sdl_plot)
  vdiffr::expect_doppelganger("gauge to sdl all multi", g2sdl_all_plot)
})

test_that("poly to poly works", {

  skip_on_os('linux')

  g2pagg <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean")
  )

  p2pagg <- multi_aggregate(g2pagg,
    aggsequence = list(cewo_valleys = cewo_valleys),
    groupers = "scenario",
    aggCols = "sdl_units_mean_ewr_achieved",
    funsequence = list("mean")
  )

  # stringr::str_flatten(names(p2pagg), "', '")
  namestring <- c(
    "scenario", "polyID", "cewo_valleys_mean_sdl_units_mean_ewr_achieved",
    "ValleyName", "ValleyID", "ValleyCode", "geometry"
  )
  expect_equal(names(p2pagg), namestring)
  expect_s3_class(p2pagg, "sf")
  expect_equal(nrow(p2pagg), 21)

  # Plots are useful for checking spatial outcomes
  g2sdl2cewo_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = p2pagg,
      ggplot2::aes(fill = cewo_valleys_mean_sdl_units_mean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("g2sdl2cewo multi", g2sdl2cewo_plot)
})


# argument formats --------------------------------------------------------


test_that("bare functions", {
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(mean)
  )
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c("scenario", "polyID", "sdl_units_mean_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry")
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 6)
})

test_that("list functions", {
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(list(mean = ~ mean(., na.rm = TRUE)))
  )
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c("scenario", "polyID", "sdl_units_mean_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry")
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 6)
})

test_that("multiple functions", {
  # character
  spatagg_c <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(c("mean", "sd"))
  )
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "sdl_units_mean_ewr_achieved",
    "sdl_units_sd_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry"
  )
  expect_equal(names(spatagg_c), namestring)
  expect_s3_class(spatagg_c, "sf")
  expect_equal(nrow(spatagg_c), 6)

  # bare- naming fails
  spatagg_b <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(c(mean, sd))
  )
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "sdl_units_mean_ewr_achieved",
    "sdl_units_sd_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry"
  )
  expect_equal(names(spatagg_b), namestring)
  expect_s3_class(spatagg_b, "sf")
  expect_equal(nrow(spatagg_b), 6)

  # list
  spatagg_l <- multi_aggregate(ewr_to_agg,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    ))
  )

  expect_equal(names(spatagg_l), namestring)
  expect_s3_class(spatagg_l, "sf")
  expect_equal(nrow(spatagg_l), 6)
})


# Multi-aggregation -------------------------------------------------------

# Theme-only
test_that("multi-step theme agg works, nongeom", {
  # no need to load the demo/test data since it's in /data
  aggseq <- list(
    c("ewr_code_timing", "ewr_code"),
    c("ewr_code", "env_obj"),
    c("env_obj", "Specific_goal"),
    c("Specific_goal", "Objective"),
    c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    c("CompensatingFactor"),
    c("ArithmeticMean"),
    c("ArithmeticMean"),
    c("ArithmeticMean"),
    c("ArithmeticMean")
  )

  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = aggseq,
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = make_edges(causal_ewr, aggseq),
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(agged), "', '")
  expect_equal(
    names(agged),
    c(
      "scenario", "gauge", "planning_unit_name", "target_5_year_2024",
      "target_5_year_2024_ArithmeticMean_Objective_ArithmeticMean_Specific_goal_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_CompensatingFactor_ewr_achieved"
    )
  )
  expect_s3_class(agged, "data.frame")
})

test_that("multi-step theme agg works, auto-edges", {
  # no need to load the demo/test data since it's in /data
  aggseq <- list(
    c("ewr_code_timing", "ewr_code"),
    c("ewr_code", "env_obj"),
    c("env_obj", "Specific_goal"),
    c("Specific_goal", "Objective"),
    c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    c("CompensatingFactor"),
    c("ArithmeticMean"),
    c("ArithmeticMean"),
    c("ArithmeticMean"),
    c("ArithmeticMean")
  )

  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = aggseq,
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(agged), "', '")
  expect_equal(
    names(agged),
    c(
      "scenario", "gauge", "planning_unit_name", "target_5_year_2024",
      "target_5_year_2024_ArithmeticMean_Objective_ArithmeticMean_Specific_goal_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_CompensatingFactor_ewr_achieved"
    )
  )
  expect_s3_class(agged, "data.frame")
})


# Spatial only
test_that("multi-step spatial works", {

  skip_on_os('linux')

  aggseq <- list(
    sdl_units = sdl_units,
    cewo_valleys = cewo_valleys,
    basin = basin
  )
  funseq <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
  )

  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID",
    "basin_ArithmeticMean_cewo_valleys_ArithmeticMean_sdl_units_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 3)

  # Keeping the whole set of polys doesn't really matter here
  spataggkeep <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    keepAllPolys = TRUE,
    auto_ewr_PU = TRUE
  )
  # stringr::str_flatten(names(spatagg), "', '")
  # namestring <- c('scenario', 'polyID', 'sdl_units_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spataggkeep), namestring)
  expect_s3_class(spataggkeep, "sf")
  expect_equal(nrow(spataggkeep), nrow(basin) * length(unique(ewr_to_agg$scenario)))

  # Plots are useful for checking spatial outcomes
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spatagg,
      ggplot2::aes(fill = basin_ArithmeticMean_cewo_valleys_ArithmeticMean_sdl_units_ArithmeticMean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  g2sdl_all_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spataggkeep,
      ggplot2::aes(fill = basin_ArithmeticMean_cewo_valleys_ArithmeticMean_sdl_units_ArithmeticMean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("multi-spatial", g2sdl_plot)
  vdiffr::expect_doppelganger("mulit-spatial", g2sdl_all_plot)
})

# Theme and spatial together
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

  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
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
  expect_equal(nrow(spatagg), 228)

  # Plots are useful for checking spatial outcomes.
  # There are a million targets. Pick one
  target_5 <- spatagg |>
    dplyr::filter(target_5_year_2024 == "Annual detection of species and life stages representative of the whole fish community through key fish passages in specified planning units")
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = target_5,
      ggplot2::aes(fill = target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("spatial-theme multi", g2sdl_plot)
})

# Theme and spatial together
test_that("multi-step theme and spatial works with !namehistory", {

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


  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    namehistory = FALSE,
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "target_5_year_2024", "OBJECTID",
    "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN", "geometry",
    "ewr_achieved", "aggfun_1", "aggLevel_1", "aggfun_2",
    "aggLevel_2", "aggfun_3", "aggLevel_3", "aggfun_4",
    "aggLevel_4", "aggfun_5", "aggLevel_5", "aggfun_6",
    "aggLevel_6", "aggfun_7", "aggLevel_7", "aggfun_8",
    "aggLevel_8"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 228)

  # Plots are useful for checking spatial outcomes.
  # There are a million targets. Pick one
  target_5 <- spatagg |>
    dplyr::filter(target_5_year_2024 == "Annual detection of species and life stages representative of the whole fish community through key fish passages in specified planning units")
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = target_5,
      ggplot2::aes(fill = ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("spatial-theme_multi_namehistory", g2sdl_plot)
})

test_that("passing name of sf objects works", {
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = "sdl_units",
    Specific_goal = c("env_obj", "Specific_goal"),
    catchment = cewo_valleys,
    Objective = c("Specific_goal", "Objective"),
    mdb = "basin",
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


  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    namehistory = FALSE,
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "target_5_year_2024", "OBJECTID",
    "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN", "geometry",
    "ewr_achieved", "aggfun_1", "aggLevel_1", "aggfun_2",
    "aggLevel_2", "aggfun_3", "aggLevel_3", "aggfun_4",
    "aggLevel_4", "aggfun_5", "aggLevel_5", "aggfun_6",
    "aggLevel_6", "aggfun_7", "aggLevel_7", "aggfun_8",
    "aggLevel_8"
  )
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 228)

  # Don't bother with the plots
})


# Theme and spatial together
test_that("backstepping along theme axis throws informative error", {
  # How does the code behave if we try to back back up a previous  causal level,
  # e.g. not move in a nested way through the network?



  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units,
    Specific_goal = c("env_obj", "Specific_goal"),
    Objective = c("Specific_goal", "Objective"),
    Target = c("Objective", "Target"),
    target_5_year_2024 = c("Objective", "target_5_year_2024"),
    test1 = c("Objective", "testy"),
    test2 = c("env_obj", "more_testy")
  )

  funseq <- list(
    "CompensatingFactor",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
  )


  expect_error(
    spatagg <- multi_aggregate(ewr_to_agg,
      aggsequence = aggseq,
      groupers = "scenario",
      aggCols = "ewr_achieved",
      funsequence = funseq,
      causal_edges = causal_ewr,
      namehistory = FALSE,
      auto_ewr_PU = TRUE
    ),
    regexp = "Aggregating multiple times"
  )
})


test_that("saving the list of steps and appropriate persistence of PU grouping", {

  skip_on_os('linux')

  # also allows checking that the planning unit grouping persists until it shouldn't and then gets dropped

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

  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$target_5_year_2024), 228)
  expect_equal(sum(is.na(spatagg$target_5_year_2024)), 3)

  # Plots are useful for checking spatial outcomes.
  # There are a million targets. Pick one
  target_5 <- spatagg$target_5_year_2024 |>
    dplyr::filter(target_5_year_2024 == "Annual detection of species and life stages representative of the whole fish community through key fish passages in specified planning units")
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = target_5,
      ggplot2::aes(fill = target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("spatial-theme multi_endoflist", g2sdl_plot)
})


# Types of functions ------------------------------------------------------

test_that("single functions at each step, called in different ways", {
  # Use a smaller set of aggs
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units
  )
  # character
  funseq_c <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean"
  )

  # Expect_warning because sf throws a warning about spatially constant attributes. and it gets thrown multiple times
  spatagg_c <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_c,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_c), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg_c, "list")
  expect_s3_class(spatagg_c[[length(spatagg_c)]], "sf")
  expect_equal(nrow(spatagg_c$sdl_units), 195)
  expect_equal(sum(is.na(spatagg_c$sdl_units)), 6)

  # bare
  funseq_b <- list(
    ArithmeticMean,
    ArithmeticMean,
    ArithmeticMean
  )

  # Expect_warning because sf throws a warning about spatially constant attributes. and it gets thrown multiple times
  expect_error(spatagg_b <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_b,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  spatagg_b <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(
      ArithmeticMean,
      ArithmeticMean,
      ArithmeticMean
    ),
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_b), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg_b, "list")
  expect_s3_class(spatagg_b[[length(spatagg_b)]], "sf")
  expect_equal(nrow(spatagg_b$sdl_units), 195)
  expect_equal(sum(is.na(spatagg_b$sdl_units)), 6)

  # list
  funseq_l <- list(
    list(~ ArithmeticMean(.)),
    list(~ ArithmeticMean(.)),
    list(~ ArithmeticMean(.))
  )

  spatagg_l <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_l,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_l), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg_l, "list")
  expect_s3_class(spatagg_l[[length(spatagg_l)]], "sf")
  expect_equal(nrow(spatagg_l$sdl_units), 195)
  expect_equal(sum(is.na(spatagg_l$sdl_units)), 6)
})

test_that("multiple functions at each step", {
  # Use a smaller set of aggs
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units
  )
  # character
  funseq_c <- list(
    c("ArithmeticMean", "LimitingFactor"),
    c("ArithmeticMean", "GeometricMean"),
    c("ArithmeticMean", "CompensatingFactor")
  )

  spatagg_c <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_c,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_c), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg_c, "list")
  expect_s3_class(spatagg_c[[length(spatagg_c)]], "sf")
  expect_equal(nrow(spatagg_c$sdl_units), 195)
  expect_equal(ncol(spatagg_c$sdl_units), 15)
  expect_equal(sum(is.na(spatagg_c$sdl_units)), 6)

  # bare
  # funseq_b <- list(c(ArithmeticMean, LimitingFactor),
  #                  c(ArithmeticMean, GeometricMean),
  #                  c(ArithmeticMean, CompensatingFactor))

  spatagg_b <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(
      c(ArithmeticMean, LimitingFactor),
      c(ArithmeticMean, GeometricMean),
      c(ArithmeticMean, CompensatingFactor)
    ),
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_b), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg_b, "list")
  expect_s3_class(spatagg_b[[length(spatagg_b)]], "sf")
  expect_equal(nrow(spatagg_b$sdl_units), 195)
  expect_equal(ncol(spatagg_c$sdl_units), 15)
  expect_equal(sum(is.na(spatagg_b$sdl_units)), 6)

  # list
  funseq_l <- list(
    list(ArithmeticMean = ~ ArithmeticMean(.), LimitingFactor = ~ LimitingFactor(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), GeometricMean = ~ GeometricMean(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  spatagg_l <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_l,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_l), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg_l, "list")
  expect_s3_class(spatagg_l[[length(spatagg_l)]], "sf")
  expect_equal(nrow(spatagg_l$sdl_units), 195)
  expect_equal(ncol(spatagg_c$sdl_units), 15)
  expect_equal(sum(is.na(spatagg_l$sdl_units)), 6)
})

test_that("mixed functions between steps", {
  # Use a smaller set of aggs
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units
  )

  # all three- I don't expect the multiple bare to work
  funseq <- list(
    c("ArithmeticMean", "LimitingFactor"),
    c(ArithmeticMean, GeometricMean),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Directly declaring list
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(
      c("ArithmeticMean", "LimitingFactor"),
      c(ArithmeticMean, GeometricMean),
      list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
    ),
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 195)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 6)

  # a single bare to avoid that issue still fails
  funseq_1b <- list(
    c("ArithmeticMean", "LimitingFactor"),
    ArithmeticMean,
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_1b,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Mixed single functions
  # a single of everything still fails
  funseq_1 <- list(
    "ArithmeticMean",
    ArithmeticMean,
    list(ArithmeticMean = ~ ArithmeticMean(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_1,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Mixed character and list
  funseq_cl <- list(
    c("ArithmeticMean", "LimitingFactor"),
    c("ArithmeticMean", "GeometricMean"),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_cl,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 195)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 6)


  # Mixed character and list within a single level- this is no longer supported
  # as of dplyr 1.1. It *does* work with dplyr 1.0, but I think we need to move
  # forward from that.
  skip_if_not_installed("dplyr", minimum_version = 1.1)
  funseq_clc <- list(
    c("ArithmeticMean", "LimitingFactor"),
    list("ArithmeticMean", GeometricMean = ~ GeometricMean(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_clc,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # for dplyr 1.0 these work
  # expect_equal(names(spatagg), c('ewr_code_timing', names(aggseq)))
  # expect_type(spatagg, 'list')
  # expect_s3_class(spatagg[[length(spatagg)]], 'sf')
  # expect_equal(nrow(spatagg$sdl_units), 195)
  # expect_equal(ncol(spatagg$sdl_units), 15)
  # expect_equal(sum(is.na(spatagg$sdl_units)), 6)
})


test_that("mixed functions between steps", {
  # Use a smaller set of aggs
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units
  )

  # all three- I don't expect the multiple bare to work
  funseq <- list(
    c("ArithmeticMean", "LimitingFactor"),
    c(ArithmeticMean, GeometricMean),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Directly declaring list
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(
      c("ArithmeticMean", "LimitingFactor"),
      c(ArithmeticMean, GeometricMean),
      list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
    ),
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 195)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 6)

  # a single bare to avoid that issue still fails
  funseq_1b <- list(
    c("ArithmeticMean", "LimitingFactor"),
    ArithmeticMean,
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_1b,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Mixed single functions
  # a single of everything still fails
  funseq_1 <- list(
    "ArithmeticMean",
    ArithmeticMean,
    list(ArithmeticMean = ~ ArithmeticMean(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_1,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Mixed character and list
  funseq_cl <- list(
    c("ArithmeticMean", "LimitingFactor"),
    c("ArithmeticMean", "GeometricMean"),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_cl,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 195)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 6)


  # Mixed character and list within a single level- this is no longer supported
  # as of dplyr 1.1. It *does* work with dplyr 1.0, but I think we need to move
  # forward from that.
  skip_if_not_installed("dplyr", minimum_version = 1.1)
  funseq_clc <- list(
    c("ArithmeticMean", "LimitingFactor"),
    list("ArithmeticMean", GeometricMean = ~ GeometricMean(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_clc,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # for dplyr 1.0 these work
  # expect_equal(names(spatagg), c('ewr_code_timing', names(aggseq)))
  # expect_type(spatagg, 'list')
  # expect_s3_class(spatagg[[length(spatagg)]], 'sf')
  # expect_equal(nrow(spatagg$sdl_units), 195)
  # expect_equal(ncol(spatagg$sdl_units), 15)
  # expect_equal(sum(is.na(spatagg$sdl_units)), 6)
})

test_that("mixed functions including quosures, singles, multiples, and characters", {
  # The goal here is mostly to provide tests of the situation where we get
  # character vectors from a params file, and to give me things to look at in
  # the debugger to figure out how it handles the different options
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = "sdl_units",
    Specific_goal = c("env_obj", "Specific_goal"),
    cewo_valleys = "cewo_valleys",
    Objective = c("Specific_goal", "Objective"),
    mdb = "basin",
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )

  funseq <- list(
    "ArithmeticMean",
    list(mean = ~ mean(., na.rm = TRUE)),
    "list(mean = ~mean(., na.rm = TRUE))", # as it might come in from yml
    "ArithmeticMean",
    rlang::quo(list(wm = ~ weighted.mean(., w = area, na.rm = TRUE))),
    "ArithmeticMean",
    "rlang::quo(list(wm = ~weighted.mean(., w = area, na.rm = TRUE)))",
    c("ArithmeticMean", "LimitingFactor")
  )

  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 195)
  expect_equal(ncol(spatagg$sdl_units), 8)
  expect_equal(sum(is.na(spatagg$sdl_units)), 6)
  expect_equal(nrow(spatagg$target_5_year_2024), 228)
  expect_equal(ncol(spatagg$target_5_year_2024), 11)
  expect_equal(sum(is.na(spatagg$target_5_year_2024)), 3)
})

test_that("group_until works", {
  # basic usage
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

  # first, expect it to fail without auto_ewr_PU or group_until

  expect_error(spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = c("scenario"),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  ))

  # By name, vctor
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name"),
    group_until = c(NA, "sdl_units"),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # By name, a list, as it should be
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name"),
    group_until = list(scenario = NA, planning_unit_name = "sdl_units"),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )


  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  # By index
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name"),
    group_until = c(NA, 3),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")

  # by type/test
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name"),
    group_until = c(NA, is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  expect_equal(names(spatagg), c("ewr_code_timing", names(aggseq)))
  expect_type(spatagg, "list")

  # a named list, only one entry
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name"),
    group_until = list(planning_unit_name = "sdl_units"),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))

  # a named list, only one entry, function
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name"),
    group_until = list(planning_unit_name = is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))

  # a named list, group_until not in groupers and tidyselected grouper
  # Because
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = tidyselect::starts_with("sce"),
    group_until = list(planning_unit_name = is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))

  # another tidyselect
  spatagg <- multi_aggregate(ewr_to_agg,
    aggsequence = aggseq,
    groupers = tidyselect::matches("(sce)|(plan)"),
    group_until = list(planning_unit_name = is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$ewr_code_timing) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))
})
