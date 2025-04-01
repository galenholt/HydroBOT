### First, test that it works as a wrapper of theme_aggregate and
### multi_aggregate- it should pass all their tests with minor format edits:
# edits- change function name, turn aggregation and functions into lists
ewr_to_agg <- make_test_ewr_prepped()

# Sets up the earlier approach with time-means that most tests were built for
ewr_to_agg_timemean <- temporal_aggregate(ewr_to_agg,
                                           breaks = 'all_time',
                                           groupers = c('scenario',
                                                        'gauge', 'planning_unit_name', 'state', 'SWSDLName',
                                                        'ewr_code', 'ewr_code_timing', 'site'),
                                           aggCols = 'ewr_achieved',
                                           funlist = 'ArithmeticMean',
                                           prefix = '') |>
  dplyr::rename(ewr_achieved = ArithmeticMean_ewr_achieved)

non_spatial_ewrout <- ewr_to_agg_timemean |> sf::st_drop_geometry()

# Tests from theme_aggregate ----------------------------------------------

test_that("ewr-obj works, nongeom", {
  # no need to load the demo/test data since it's in /data
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    causal_edges = make_edges(causal_ewr, list(c("ewr_code_timing", "ewr_code"))),
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", 'SWSDLName',
                               "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("auto-generating causal_edges works", {
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", 'SWSDLName',
                               "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("spatial input data works", {
  agged <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_snapshot(names(agged))
  expect_s3_class(agged, "data.frame")
  expect_s3_class(agged, "sf")

  # geonames can't be user-set because multi_agg tracks all of them
})

test_that("bare functions", {
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(mean),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", 'SWSDLName',
                               "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("list functions", {
  agged <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(list(mean = ~ mean(., na.rm = TRUE))),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c("scenario", "gauge", "planning_unit_name", 'SWSDLName',
                               "ewr_code", "ewr_code_mean_ewr_achieved"))
  expect_s3_class(agged, "data.frame")
})

test_that("multiple functions", {
  # Character
  agged_c <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(c("mean", "sd")),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_c), c(
    "scenario", "gauge", "planning_unit_name", 'SWSDLName', "ewr_code",
    "ewr_code_mean_ewr_achieved", "ewr_code_sd_ewr_achieved"
  ))
  expect_s3_class(agged_c, "data.frame")

  # bare
  agged_b <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(c(mean, sd)),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_b), c(
    "scenario", "gauge", "planning_unit_name", 'SWSDLName', "ewr_code",
    "ewr_code_mean_ewr_achieved", "ewr_code_sd_ewr_achieved"
  ))
  expect_s3_class(agged_b, "data.frame")

  # List
  agged_l <- multi_aggregate(non_spatial_ewrout,
    aggsequence = list(c("ewr_code_timing", "ewr_code")),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list(list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    )),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_l), c(
    "scenario", "gauge", "planning_unit_name", 'SWSDLName', "ewr_code",
    "ewr_code_mean_ewr_achieved", "ewr_code_sd_ewr_achieved"
  ))
  expect_s3_class(agged_l, "data.frame")
})



# Tests from spatial_aggregate --------------------------------------------

test_that("gauge to poly works", {

  skip_on_os('linux')

  # should error if not list, to keep from iterating over sdl_units itself
  expect_error(multi_aggregate(ewr_to_agg_timemean,
    aggsequence = sdl_units,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean")
  ))

  # Two warnings because of theme and sptial where should retain tings.
  expect_warning(expect_warning(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean")
  )))

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "sdl_units_mean_ewr_achieved",
    "SWSDLID", "SWSDLName", "StateID", "geometry"
  )
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 8)

  # Keeping the whole set of polys
  expect_warning(expect_warning(spataggkeep <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean"),
    keepAllPolys = TRUE
  )))

  # stringr::str_flatten(names(spatagg), "', '")
  # namestring <- c('scenario', 'polyID', 'sdl_units_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(sort(names(spataggkeep)), sort(namestring))
  expect_s3_class(spataggkeep, "sf")
  expect_equal(nrow(spataggkeep), nrow(sdl_units) * length(unique(ewr_to_agg_timemean$scenario)))

  # Plots are useful for checking spatial outcomes
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spatagg,
      ggplot2::aes(fill = sdl_units_mean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  g2sdl_all_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spataggkeep,
      ggplot2::aes(fill = sdl_units_mean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("gauge to sdl multi", g2sdl_plot)
  vdiffr::expect_doppelganger("gauge to sdl all multi", g2sdl_all_plot)

})

test_that("poly to poly works", {

  skip_on_os('linux')

  expect_warning(expect_warning(g2pagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = list("mean")
  )))

  expect_warning(p2pagg <- multi_aggregate(g2pagg,
    aggsequence = list(cewo_valleys = cewo_valleys),
    groupers = "scenario",
    aggCols = "sdl_units_mean_ewr_achieved",
    funsequence = list("mean")
  ))

  # stringr::str_flatten(names(p2pagg), "', '")
  namestring <- c(
    "scenario", "polyID", "cewo_valleys_mean_sdl_units_mean_ewr_achieved",
    "ValleyName", "ValleyID", "ValleyCode", "geometry"
  )
  expect_equal(names(p2pagg), namestring)
  expect_s3_class(p2pagg, "sf")
  expect_equal(nrow(p2pagg), 28)

  # Plots are useful for checking spatial outcomes
  g2sdl2cewo_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = p2pagg,
      ggplot2::aes(fill = cewo_valleys_mean_sdl_units_mean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("g2sdl2cewo multi", g2sdl2cewo_plot)
})


# argument formats --------------------------------------------------------

# These are largely wrapped in expect_warning because they're purely spatial and ignore the theme dim, whihc throws a warning.

test_that("bare functions", {
  expect_warning(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    auto_ewr_PU = TRUE,
    funsequence = list(mean)
  ))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c("scenario", "polyID", "sdl_units_mean_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry")
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 8)
})

test_that("list functions", {
  expect_warning(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    auto_ewr_PU = TRUE,
    funsequence = list(list(mean = ~ mean(., na.rm = TRUE)))
  ))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c("scenario", "polyID", "sdl_units_mean_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry")
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 8)
})

test_that("multiple functions", {
  # character
  expect_warning(spatagg_c <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    auto_ewr_PU = TRUE,
    funsequence = list(c("mean", "sd"))
  ))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "sdl_units_mean_ewr_achieved",
    "sdl_units_sd_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry"
  )
  expect_equal(sort(names(spatagg_c)), sort(namestring))
  expect_s3_class(spatagg_c, "sf")
  expect_equal(nrow(spatagg_c), 8)

  # bare- naming fails
  expect_warning(spatagg_b <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = "scenario",
    aggCols = "ewr_achieved",
    auto_ewr_PU = TRUE,
    funsequence = list(c(mean, sd))
  ))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID", "sdl_units_mean_ewr_achieved",
    "sdl_units_sd_ewr_achieved", "SWSDLID", "SWSDLName", "StateID", "geometry"
  )
  expect_equal(sort(names(spatagg_b)), sort(namestring))
  expect_s3_class(spatagg_b, "sf")
  expect_equal(nrow(spatagg_b), 8)

  # list
  expect_warning(spatagg_l <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = list(sdl_units = sdl_units),
    groupers = c("scenario"),
    aggCols = "ewr_achieved",
    auto_ewr_PU = TRUE,
    funsequence = list(list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    )))
  )

  expect_equal(sort(names(spatagg_l)), sort(namestring))
  expect_s3_class(spatagg_l, "sf")
  expect_equal(nrow(spatagg_l), 8)
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
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = make_edges(causal_ewr, aggseq),
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(agged), "', '")
  expect_snapshot(names(agged))
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
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )

  # stringr::str_flatten(names(agged), "', '")
  expect_snapshot(names(agged))
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

  expect_warning(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    auto_ewr_PU = TRUE
  ))

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c(
    "scenario", "polyID",
    "basin_ArithmeticMean_cewo_valleys_ArithmeticMean_sdl_units_ArithmeticMean_ewr_achieved",
    "OBJECTID", "DDIV_NAME", "AREA_HA", "SHAPE_AREA", "SHAPE_LEN",
    "geometry"
  )
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 4)

  # Keeping the whole set of polys doesn't really matter here
  expect_warning(spataggkeep <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    keepAllPolys = TRUE,
    auto_ewr_PU = TRUE
  ))
  # stringr::str_flatten(names(spatagg), "', '")
  # namestring <- c('scenario', 'polyID', 'sdl_units_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spataggkeep), namestring)
  expect_s3_class(spataggkeep, "sf")
  expect_equal(nrow(spataggkeep), nrow(basin) * length(unique(ewr_to_agg_timemean$scenario)))

  # Plots are useful for checking spatial outcomes
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spatagg,
      ggplot2::aes(fill = basin_ArithmeticMean_cewo_valleys_ArithmeticMean_sdl_units_ArithmeticMean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  g2sdl_all_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = spataggkeep,
      ggplot2::aes(fill = basin_ArithmeticMean_cewo_valleys_ArithmeticMean_sdl_units_ArithmeticMean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
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

  spatagg <- multi_aggregate(ewr_to_agg_timemean,
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
  expect_equal(sort(names(spatagg)), sort(namestring))
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
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("spatial-theme multi", g2sdl_plot)
})

# Theme and spatial together
test_that("multi-step theme and spatial works with !namehistory", {

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
    "SpatialWeightedMean",
    "ArithmeticMean",
    "SpatialWeightedMean",
    "ArithmeticMean"
  )


  spatagg <- multi_aggregate(ewr_to_agg_timemean,
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
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 300)

  # Check the values are actually right
  funs_in_df <- spatagg |>
    sf::st_drop_geometry() |>
    dplyr::slice(1) |>
    dplyr::select(tidyselect::starts_with('aggfun_')) |>
    unlist()

  aggs_in_df <- spatagg |>
    sf::st_drop_geometry() |>
    dplyr::slice(1) |>
    dplyr::select(tidyselect::starts_with('aggLevel_')) |>
    unlist()


  expect_equal(unname(funs_in_df), unlist(funseq))
  expect_equal(unname(aggs_in_df), names(aggseq))

  # does the parsing work for 'list' funseqs? lambda functions? other anonymous?

  # lambda
  funseq_wm <- list(
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    "ArithmeticMean",
    list(wm = ~weighted.mean(., w = area,
                             na.rm = TRUE)),
    "ArithmeticMean",
    list(wm = ~weighted.mean(., w = area,
                             na.rm = TRUE)),
    "ArithmeticMean"
  )

  # funseq_wm <- list(
  #   "ArithmeticMean",
  #   "ArithmeticMean",
  #   "ArithmeticMean",
  #   "ArithmeticMean",
  #   wm = \(x) weighted.mean(x, w = area,
  #                            na.rm = TRUE),
  #   "ArithmeticMean",
  #   wm = \(x) weighted.mean(x, w = area,
  #                            na.rm = TRUE),
  #   "ArithmeticMean"
  # )


  spatagg_wm <- multi_aggregate(ewr_to_agg_timemean,
                               aggsequence = aggseq,
                               groupers = "scenario",
                               aggCols = "ewr_achieved",
                               funsequence = funseq_wm,
                               causal_edges = causal_ewr,
                               namehistory = FALSE,
                               auto_ewr_PU = TRUE
  )

  # Check the values are actually right
  funs_in_df_wm <- spatagg_wm |>
    sf::st_drop_geometry() |>
    dplyr::slice(1) |>
    dplyr::select(tidyselect::starts_with('aggfun_')) |>
    unlist()

  aggs_in_df_wm <- spatagg_wm |>
    sf::st_drop_geometry() |>
    dplyr::slice(1) |>
    dplyr::select(tidyselect::starts_with('aggLevel_')) |>
    unlist()


  # Ge the name of the lambda
  fs <- purrr::map_lgl(funseq_wm, is.character)
  fse <- funseq_wm
  fse[!fs] <- names(unlist(funseq_wm[!fs]))

  expect_equal(unname(funs_in_df_wm), unlist(fse))
  expect_equal(unname(aggs_in_df_wm), names(aggseq))

  ## THIS HAS NOT CHECKED A SITUATION WITH MULTIPLE AGGREGATION FUNCTIONS. presumably if one parses ok, they all will, but that should be checked.

  # Plots are useful for checking spatial outcomes.
  # There are a million targets. Pick one
  target_5 <- spatagg |>
    dplyr::filter(target_5_year_2024 == "Annual detection of species and life stages representative of the whole fish community through key fish passages in specified planning units")
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = target_5,
      ggplot2::aes(fill = ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")


  skip_on_os('linux')

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


  spatagg <- multi_aggregate(ewr_to_agg_timemean,
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
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, "sf")
  expect_equal(nrow(spatagg), 300)

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
    spatagg <- multi_aggregate(ewr_to_agg_timemean,
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

  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  # should have planning units and gauge up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  expect_true("gauge" %in% names(spatagg$agg_input) &
                "gauge" %in% names(spatagg$ewr_code) &
                "gauge" %in% names(spatagg$env_obj))

  expect_true(!"gauge" %in% names(spatagg$sdl_units) &
                !"gauge" %in% names(spatagg$Specific_goal) &
                !"gauge" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$target_5_year_2024), 300)
  expect_equal(sum(is.na(spatagg$target_5_year_2024)), 0)

  # Plots are useful for checking spatial outcomes.
  # There are a million targets. Pick one
  target_5 <- spatagg$target_5_year_2024 |>
    dplyr::filter(target_5_year_2024 == "Annual detection of species and life stages representative of the whole fish community through key fish passages in specified planning units")
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = target_5,
      ggplot2::aes(fill = target_5_year_2024_ArithmeticMean_mdb_ArithmeticMean_Objective_ArithmeticMean_catchment_ArithmeticMean_Specific_goal_ArithmeticMean_sdl_units_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_ArithmeticMean_ewr_achieved)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = "bottom")

  vdiffr::expect_doppelganger("spatial-theme multi_endoflist", g2sdl_plot)
})

# Nonspatial joining of spatial data
test_that("nonspatial joins of spatial data", {

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

  spatagg <- multi_aggregate(ewr_to_agg_timemean,
                             aggsequence = aggseq,
                             groupers = "scenario",
                             aggCols = "ewr_achieved",
                             funsequence = funseq,
                             causal_edges = causal_ewr,
                             saveintermediate = TRUE,
                             namehistory = FALSE,
                             auto_ewr_PU = TRUE,
                             pseudo_spatial = "planning_units"
  )

  # Find some NA- delete
  naexample <- spatagg$ewr_code |> dplyr::filter(scenario == 'base' & planning_unit_name == 'Muggabah Creek')
  naobj <- spatagg$env_obj |> dplyr::filter(scenario == 'base' & planning_unit_name == 'Muggabah Creek')

  relevantewrs <- causal_ewr$ewr2obj |>
    dplyr::filter(ewr_code %in% naexample$ewr_code & planning_unit_name %in% naexample$planning_unit_name)

  naexample$ewr_code[!naexample$ewr_code %in% relevantewrs$ewr_code]



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

  # testing the splitting up with higher levels

  # get the PU in the PU level
  puinpu <- spatagg$planning_units |>
    dplyr::summarise(ewr_achieved = mean(ewr_achieved, na.rm = T),
                     .by = c(planning_unit_name, geometry))
  # each planning unit should appear once (e.g. they are 1:1 with geometry)
  expect_true(!any(duplicated(puinpu$planning_unit_name)))

  # This helps see if there is any splitting- do any of the PU actually cross
  # sdl boundaries? Yes, barely? Lower darling shows up in the sdls?
  g2puplot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = dplyr::filter(sdl_units, SWSDLName %in% unique(spatagg$sdl_units$SWSDLName))) +
    ggplot2::geom_sf(
      data = puinpu,
      ggplot2::aes(fill = planning_unit_name)
    ) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean |> dplyr::select(geometry) |> dplyr::distinct()) +
    ggplot2::theme(legend.position = "bottom")

  # to actually test the intersection (and correct areas), we need to test
  # spatial_joiner, since those intersections won't be preserved
  # post-aggregation. that's done in the spatial_joiner testing.

  ## and what happens to overflow if we pre-cut the sdls?
  # spatagg above gains an sdl (lower darling)

  expect_equal(unique(spatagg$sdl_units$SWSDLName), c('Lachlan', 'Macquarie-Castlereagh'))

  # but if we pre-cut to the appropriate sdls, that should limit it.
  # *THIS SHOULD BE DONE BY USER*, since it is appropriate behaviour to do the intersection properly. but we want to make sure it works for the user.
  aggseqc <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    planning_units = planning_units,
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = dplyr::filter(sdl_units, SWSDLName %in% c('Lachlan', 'Murrumbidgee', 'Macquarie-Castlereagh')),
    Specific_goal = c("env_obj", "Specific_goal"),
    catchment = cewo_valleys,
    Objective = c("Specific_goal", "Objective"),
    mdb = basin,
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )

  spataggc <- multi_aggregate(ewr_to_agg_timemean,
                             aggsequence = aggseqc,
                             groupers = "scenario",
                             aggCols = "ewr_achieved",
                             funsequence = funseq,
                             causal_edges = causal_ewr,
                             saveintermediate = TRUE,
                             namehistory = FALSE,
                             auto_ewr_PU = TRUE,
                             pseudo_spatial = "planning_units"
  )

  expect_equal(unique(spataggc$sdl_units$SWSDLName), c('Lachlan', 'Macquarie-Castlereagh'))

  # check it works for group_until explicitly
  spatagg_g <- multi_aggregate(ewr_to_agg_timemean,
                              aggsequence = aggseqc,
                              groupers = "scenario",
                              aggCols = "ewr_achieved",
                              group_until = list(planning_unit_name = 'sdl_units', gauge = is_notpoint, SWSDLName = 'sdl_units'),
                              funsequence = funseq,
                              causal_edges = causal_ewr,
                              saveintermediate = TRUE,
                              namehistory = FALSE,
                              pseudo_spatial = "planning_units"
  )

  # those should match
  sortc <- spataggc$mdb |>
    sf::st_drop_geometry() |>
    dplyr::arrange(scenario, Objective, polyID, ewr_achieved)

  sortg <- spatagg_g$mdb |>
    sf::st_drop_geometry() |>
    dplyr::arrange(scenario, Objective, polyID, ewr_achieved)

  expect_equal(sortc$ewr_achieved, sortg$ewr_achieved)



  # Plots are useful for checking spatial outcomes
  skip_on_os('linux')

  vdiffr::expect_doppelganger("planning_unit_agg", g2puplot)
})


test_that("nonspatial joins of spatial data at sdl unit", {

  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    sdl_units = sdl_units,
    env_obj = c("ewr_code", "env_obj"),
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
    "SpatialWeightedMean",
    "ArithmeticMean",
    "SpatialWeightedMean",
    "ArithmeticMean"
  )

  # THis is now working. I just need to make sure I can test that it is in fact
  # working non-spatially
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
                             aggsequence = aggseq,
                             groupers = "scenario",
                             aggCols = "ewr_achieved",
                             funsequence = funseq,
                             causal_edges = causal_ewr,
                             saveintermediate = TRUE,
                             namehistory = FALSE,
                             auto_ewr_PU = TRUE,
                             pseudo_spatial = "sdl_units"
  )

  # are any of these gauges outside the SWSDL? This data might not work to test this.
  # sf::st_join(ewr_to_agg_timemean |> dplyr::distinct(gauge, geometry), sdl_units)
  # dplyr::distinct(ewr_to_agg_timemean, gauge, geometry, SWSDLName)

  # I can alter the data though, and see if they end up in the name or the location.

  fakeewrs <- ewr_to_agg_timemean |>
    dplyr::mutate(SWSDLName = dplyr::case_when(SWSDLName == 'Lachlan' ~ 'Broken',
                                               SWSDLName == 'Macquarie-Castlereagh' ~ 'Goulburn'))
  # Need to fake the causal too, or the links don't work
  fakecausal <- causal_ewr
  fakecausal[1:2] <- fakecausal[1:2] |>
    purrr::map(\(x) x |>
                 dplyr::mutate(SWSDLName = dplyr::case_when(SWSDLName == 'Lachlan' ~ 'Broken',
                                                            SWSDLName == 'Macquarie-Castlereagh' ~ 'Goulburn')))

  spatagg_wrongsdl <- fakeewrs |>
  multi_aggregate(aggsequence = aggseq,
                                      groupers = "scenario",
                                      aggCols = "ewr_achieved",
                                      funsequence = funseq,
                                      causal_edges = fakecausal ,
                                      saveintermediate = TRUE,
                                      namehistory = FALSE,
                                      auto_ewr_PU = TRUE,
                                      pseudo_spatial = "sdl_units"
  )


  # Should throw a warning not to do this without auto_ewr_pu or pseudo-spatial.
  # Put a group_until on since that will fix the theme and time aggs
  expect_warning(spatagg_wrongsdl_spatial <- fakeewrs |>
    multi_aggregate(aggsequence = aggseq,
                    groupers = "scenario",
                    aggCols = "ewr_achieved",
                    funsequence = funseq,
                    causal_edges = causal_ewr,
                    saveintermediate = TRUE,
                    namehistory = FALSE,
                    auto_ewr_PU = FALSE,
                    group_until = list(SWSDLName = 'sdl_units',
                                       planning_unit_name = 'sdl_units',
                                       gauge = is_notpoint),
    ))

  # so, we should have real names in the first one (nonspatial),
  expect_equal(unique(spatagg$sdl_units$SWSDLName),
               c('Lachlan', 'Macquarie-Castlereagh'))
  # fake names in the nonspatial with fake names
  expect_equal(unique(spatagg_wrongsdl$sdl_units$SWSDLName),
               c('Goulburn', 'Broken'))
  # real names with spatial starting with fake names
  expect_equal(unique(spatagg_wrongsdl_spatial$sdl_units$SWSDLName),
               c('Lachlan', 'Macquarie-Castlereagh'))

  # And, make sure we're retaining planning units
  expect_true('planning_unit_name' %in% names(spatagg$ewr_code))
  expect_true('planning_unit_name' %in% names(spatagg_wrongsdl$ewr_code))
  expect_true('planning_unit_name' %in% names(spatagg_wrongsdl_spatial$ewr_code))

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
  spatagg_c <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_c,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_c), c("agg_input", names(aggseq)))
  expect_type(spatagg_c, "list")
  expect_s3_class(spatagg_c[[length(spatagg_c)]], "sf")
  expect_equal(nrow(spatagg_c$sdl_units), 248)
  expect_equal(sum(is.na(spatagg_c$sdl_units)), 0)

  # bare
  funseq_b <- list(
    ArithmeticMean,
    ArithmeticMean,
    ArithmeticMean
  )

  # Expect_warning because sf throws a warning about spatially constant attributes. and it gets thrown multiple times
  expect_error(spatagg_b <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_b,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  spatagg_b <- multi_aggregate(ewr_to_agg_timemean,
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

  expect_equal(names(spatagg_b), c("agg_input", names(aggseq)))
  expect_type(spatagg_b, "list")
  expect_s3_class(spatagg_b[[length(spatagg_b)]], "sf")
  expect_equal(nrow(spatagg_b$sdl_units), 248)
  expect_equal(sum(is.na(spatagg_b$sdl_units)), 0)

  # list
  funseq_l <- list(
    list(~ ArithmeticMean(.)),
    list(~ ArithmeticMean(.)),
    list(~ ArithmeticMean(.))
  )

  spatagg_l <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_l,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_l), c("agg_input", names(aggseq)))
  expect_type(spatagg_l, "list")
  expect_s3_class(spatagg_l[[length(spatagg_l)]], "sf")
  expect_equal(nrow(spatagg_l$sdl_units), 248)
  expect_equal(sum(is.na(spatagg_l$sdl_units)), 0)
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

  spatagg_c <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_c,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_c), c("agg_input", names(aggseq)))
  expect_type(spatagg_c, "list")
  expect_s3_class(spatagg_c[[length(spatagg_c)]], "sf")
  expect_equal(nrow(spatagg_c$sdl_units), 248)
  expect_equal(ncol(spatagg_c$sdl_units), 15)
  expect_equal(sum(is.na(spatagg_c$sdl_units)), 0)

  # bare
  # funseq_b <- list(c(ArithmeticMean, LimitingFactor),
  #                  c(ArithmeticMean, GeometricMean),
  #                  c(ArithmeticMean, CompensatingFactor))

  spatagg_b <- multi_aggregate(ewr_to_agg_timemean,
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

  expect_equal(names(spatagg_b), c("agg_input", names(aggseq)))
  expect_type(spatagg_b, "list")
  expect_s3_class(spatagg_b[[length(spatagg_b)]], "sf")
  expect_equal(nrow(spatagg_b$sdl_units), 248)
  expect_equal(ncol(spatagg_c$sdl_units), 15)
  expect_equal(sum(is.na(spatagg_b$sdl_units)), 0)

  # list
  funseq_l <- list(
    list(ArithmeticMean = ~ ArithmeticMean(.), LimitingFactor = ~ LimitingFactor(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), GeometricMean = ~ GeometricMean(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  spatagg_l <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_l,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg_l), c("agg_input", names(aggseq)))
  expect_type(spatagg_l, "list")
  expect_s3_class(spatagg_l[[length(spatagg_l)]], "sf")
  expect_equal(nrow(spatagg_l$sdl_units), 248)
  expect_equal(ncol(spatagg_c$sdl_units), 15)
  expect_equal(sum(is.na(spatagg_l$sdl_units)), 0)
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

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Directly declaring list
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
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

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 248)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 0)

  # a single bare to avoid that issue still fails
  funseq_1b <- list(
    c("ArithmeticMean", "LimitingFactor"),
    ArithmeticMean,
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
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

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
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

  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_cl,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 248)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 0)


  # Mixed character and list within a single level- this is no longer supported
  # as of dplyr 1.1. It *does* work with dplyr 1.0, but I think we need to move
  # forward from that.
  skip_if_not_installed("dplyr", minimum_version = '1.1')
  funseq_clc <- list(
    c("ArithmeticMean", "LimitingFactor"),
    list("ArithmeticMean", GeometricMean = ~ GeometricMean(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_clc,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # for dplyr 1.0 these work
  # expect_equal(names(spatagg), c('agg_input', names(aggseq)))
  # expect_type(spatagg, 'list')
  # expect_s3_class(spatagg[[length(spatagg)]], 'sf')
  # expect_equal(nrow(spatagg$sdl_units), 248)
  # expect_equal(ncol(spatagg$sdl_units), 15)
  # expect_equal(sum(is.na(spatagg$sdl_units)), 8)
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

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # Directly declaring list
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
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

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 248)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 0)

  # a single bare to avoid that issue still fails
  funseq_1b <- list(
    c("ArithmeticMean", "LimitingFactor"),
    ArithmeticMean,
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
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

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
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

  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_cl,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 248)
  expect_equal(ncol(spatagg$sdl_units), 15)
  expect_equal(sum(is.na(spatagg$sdl_units)), 0)


  # Mixed character and list within a single level- this is no longer supported
  # as of dplyr 1.1. It *does* work with dplyr 1.0, but I think we need to move
  # forward from that.
  skip_if_not_installed("dplyr", minimum_version = '1.1')
  funseq_clc <- list(
    c("ArithmeticMean", "LimitingFactor"),
    list("ArithmeticMean", GeometricMean = ~ GeometricMean(.)),
    list(ArithmeticMean = ~ ArithmeticMean(.), CompensatingFactor = ~ CompensatingFactor(.))
  )

  expect_error(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq_clc,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  ))

  # for dplyr 1.0 these work
  # expect_equal(names(spatagg), c('agg_input', names(aggseq)))
  # expect_type(spatagg, 'list')
  # expect_s3_class(spatagg[[length(spatagg)]], 'sf')
  # expect_equal(nrow(spatagg$sdl_units), 248)
  # expect_equal(ncol(spatagg$sdl_units), 15)
  # expect_equal(sum(is.na(spatagg$sdl_units)), 8)
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

  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = "scenario",
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")
  expect_s3_class(spatagg[[length(spatagg)]], "sf")
  expect_equal(nrow(spatagg$sdl_units), 248)
  expect_equal(ncol(spatagg$sdl_units), 8)
  expect_equal(sum(is.na(spatagg$sdl_units)), 0)
  expect_equal(nrow(spatagg$target_5_year_2024), 300)
  expect_equal(ncol(spatagg$target_5_year_2024), 11)
  expect_equal(sum(is.na(spatagg$target_5_year_2024)), 0)
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

  # Need to wrap twice because there are two steps that throw warnings.
  expect_warning(expect_warning(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = c("scenario"),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )))

  # By name, vctor
  # not listing gauge, but geometry makes the data persist
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name", 'SWSDLName'),
    group_until = c(NA, "sdl_units", 'sdl_units'),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # We've lost the 'gauge' name, but the geometry holds the rows together
  expect_true('gauge' %in% names(spatagg$agg_input))
  expect_true(!'gauge' %in% names(spatagg$ewr_code))
  expect_equal(nrow(spatagg$ewr_code), 356)

  # with gauge explicitly
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
                             aggsequence = aggseq,
                             groupers = c("scenario", "planning_unit_name", 'SWSDLName', "gauge"),
                             group_until = c(NA, "sdl_units", "sdl_units", 'sdl_units'),
                             aggCols = "ewr_achieved",
                             funsequence = funseq,
                             causal_edges = causal_ewr,
                             pseudo_spatial = 'sdl_units',
                             saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
                "planning_unit_name" %in% names(spatagg$ewr_code) &
                "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
                !"planning_unit_name" %in% names(spatagg$Specific_goal) &
                !"planning_unit_name" %in% names(spatagg$catchment))

  # now we should have gauge in the same places as plannign units
  expect_true("gauge" %in% names(spatagg$agg_input) &
                "gauge" %in% names(spatagg$ewr_code) &
                "gauge" %in% names(spatagg$env_obj))

  expect_true(!"gauge" %in% names(spatagg$sdl_units) &
                !"gauge" %in% names(spatagg$Specific_goal) &
                !"gauge" %in% names(spatagg$catchment))
  # and the rows should be the same as above, because this just recapitulates geometry
  expect_equal(nrow(spatagg$ewr_code), 356)


  # By name, a list, as it should be
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name", 'SWSDLName'),
    group_until = list(scenario = NA, planning_unit_name = "sdl_units", SWSDLName = 'sdl_units'),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )


  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  # By index
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name", 'SWSDLName'),
    group_until = c(NA, 3, 3),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")

  # by type/test
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name", 'SWSDLName'),
    group_until = c(NA, is_notpoint, is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))
  # There are more, but that should cover it

  expect_equal(names(spatagg), c("agg_input", names(aggseq)))
  expect_type(spatagg, "list")

  # a named list, only one entry
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name"),
    group_until = list(planning_unit_name = "sdl_units", SWSDLName = 'sdl_units'),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))

  # a named list, only one entry, function
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = c("scenario", "planning_unit_name", 'SWSDLName'),
    group_until = list(planning_unit_name = is_notpoint, SWSDLName = is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))

  # a named list, group_until not in groupers and tidyselected grouper
  # Putting 'gauge' in here too, since it usually should be, and checks it works with two
  # warning because sdls should be done pseudo-spatially, but here we want to test spatial
  expect_warning(spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = tidyselect::starts_with("sce"),
    group_until = list(planning_unit_name = is_notpoint, gauge = is_notpoint, SWSDLName = is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  ))

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # now we should have gauge in the same places as plannign units
  expect_true("gauge" %in% names(spatagg$agg_input) &
                "gauge" %in% names(spatagg$ewr_code) &
                "gauge" %in% names(spatagg$env_obj))

  expect_true(!"gauge" %in% names(spatagg$sdl_units) &
                !"gauge" %in% names(spatagg$Specific_goal) &
                !"gauge" %in% names(spatagg$catchment))
  # and the rows should be the same as above, because this just recapitulates geometry
  expect_equal(nrow(spatagg$ewr_code), 356)


  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))

  # another tidyselect
  # warning because sdls should be done pseudo-spatially, but here we want to test spatial
  spatagg <- multi_aggregate(ewr_to_agg_timemean,
    aggsequence = aggseq,
    groupers = tidyselect::matches("(sce)|(plan)"),
    group_until = list(planning_unit_name = is_notpoint, SWSDLName = is_notpoint),
    aggCols = "ewr_achieved",
    funsequence = funseq,
    causal_edges = causal_ewr,
    saveintermediate = TRUE
  )

  # should have planning units up until we aggregate to sdl
  expect_true("planning_unit_name" %in% names(spatagg$agg_input) &
    "planning_unit_name" %in% names(spatagg$ewr_code) &
    "planning_unit_name" %in% names(spatagg$env_obj))

  expect_true(!"planning_unit_name" %in% names(spatagg$sdl_units) &
    !"planning_unit_name" %in% names(spatagg$Specific_goal) &
    !"planning_unit_name" %in% names(spatagg$catchment))

  # make sure the other groupers persisted
  expect_true("scenario" %in% names(spatagg$target_5_year_2024))
})


# Temporal and sequencing -------------------------------------------------

test_that("Sequencing edge cases", {

  # if we don't pass theme steps, it should drop the theme levels because there's no way to infer them
  # But SHOULD keep time, because there is
  # This test is done above, so commenting out here, but keeping for reference
  # spatagg <- multi_aggregate(ewr_to_agg,
  #                                  aggsequence = list(sdl_units = sdl_units),
  #                                  groupers = "scenario",
  #                                  aggCols = "ewr_achieved",
  #                                  funsequence = list("mean")
  # )

  # If we do pass theme steps, but they are in a later stage, they need to be kept around

    # warning because sdls should be done pseudo-spatially, but here we want to test spatial
  expect_warning(
    spatagg_s_th_t <- multi_aggregate(ewr_to_agg,
                                     causal_edges = causal_ewr,
                                     aggsequence = list(sdl_units = sdl_units,
                                                        ewr_code = c('ewr_code_timing', 'ewr_code'),
                                                        yrs = '2 years'),
                                     groupers = "scenario",
                                     aggCols = "ewr_achieved",
                                     saveintermediate = TRUE,
                                     funsequence = list("mean", 'ArithmeticMean', 'mean')
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


    spatagg_s_t_th <- multi_aggregate(ewr_to_agg,
                                      causal_edges = causal_ewr,
                                      aggsequence = list(sdl_units = sdl_units,
                                                         yrs = '2 years',
                                                         ewr_code = c('ewr_code_timing', 'ewr_code')),
                                      groupers = "scenario",
                                      aggCols = "ewr_achieved",
                                      saveintermediate = TRUE,
                                      funsequence = list("mean", 'ArithmeticMean', 'mean'),
                                      auto_ewr_PU = TRUE
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


    spatagg_th_s_t <- multi_aggregate(ewr_to_agg,
                                      causal_edges = causal_ewr,
                                      aggsequence = list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                                                         sdl_units = sdl_units,
                                                         yrs = '2 years'),
                                      groupers = "scenario",
                                      aggCols = "ewr_achieved",
                                      saveintermediate = TRUE,
                                      funsequence = list("mean", 'ArithmeticMean', 'mean'),
                                      auto_ewr_PU = TRUE
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

    spatagg_th_t_s <- multi_aggregate(ewr_to_agg,
                                      causal_edges = causal_ewr,
                                      aggsequence = list(ewr_code = c('ewr_code_timing', 'ewr_code'),
                                                         yrs = '2 years',
                                                         sdl_units = sdl_units),
                                      groupers = "scenario",
                                      aggCols = "ewr_achieved",
                                      saveintermediate = TRUE,
                                      funsequence = list("mean", 'ArithmeticMean', 'mean'),
                                      auto_ewr_PU = TRUE
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

    spatagg_t_s_th <- multi_aggregate(ewr_to_agg,
                                      causal_edges = causal_ewr,
                                      aggsequence = list(yrs = '2 years',
                                                         sdl_units = sdl_units,
                                                         ewr_code = c('ewr_code_timing', 'ewr_code')),
                                      groupers = "scenario",
                                      aggCols = "ewr_achieved",
                                      saveintermediate = TRUE,
                                      funsequence = list("mean", 'ArithmeticMean', 'mean'),
                                      auto_ewr_PU = TRUE
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

    spatagg_t_th_s <- multi_aggregate(ewr_to_agg,
                                      causal_edges = causal_ewr,
                                      aggsequence = list(yrs = '2 years',
                                                         ewr_code = c('ewr_code_timing', 'ewr_code'),
                                                         sdl_units = sdl_units),
                                      groupers = "scenario",
                                      aggCols = "ewr_achieved",
                                      saveintermediate = TRUE,
                                      funsequence = list("mean", 'ArithmeticMean', 'mean'),
                                      auto_ewr_PU = TRUE
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

test_that("Temporal", {

  skip_on_os('linux')

  # This should thow warnings about theme if I don't have them in groupers, but not if I do

  # This should not have a time column
  spatagg_temp <- multi_aggregate(ewr_to_agg,
                                   causal_edges = causal_ewr,
                                   aggsequence = list(all_time = 'all_time'),
                                   groupers = c("scenario", 'ewr_code', 'ewr_code_timing'),
                                   aggCols = "ewr_achieved",
                                   saveintermediate = TRUE,
                                   funsequence = list("mean"),
                                  auto_ewr_PU = TRUE
  )

  expect_true(!any(purrr::map_lgl(spatagg_temp$all_time, is_time)))

  # This should
  spatagg_years <- multi_aggregate(ewr_to_agg,
                                  causal_edges = causal_ewr,
                                  aggsequence = list(yrs = '2 years'),
                                  groupers = c("scenario", 'ewr_code', 'ewr_code_timing'),
                                  aggCols = "ewr_achieved",
                                  saveintermediate = TRUE,
                                  funsequence = list("mean"),
                                  auto_ewr_PU = TRUE
  )

  expect_true(any(purrr::map_lgl(spatagg_years$yrs, is_time)))

  # and check we can feed arbitrary posix
  spatagg_psx <- multi_aggregate(ewr_to_agg,
                                   causal_edges = causal_ewr,
                                   aggsequence = list(datebits = c(lubridate::ymd('20150101', '20180404', '20200202'))),
                                   groupers = c("scenario", 'ewr_code', 'ewr_code_timing'),
                                   aggCols = "ewr_achieved",
                                   saveintermediate = TRUE,
                                   funsequence = list("mean"),
                                   auto_ewr_PU = TRUE
  )

  expect_snapshot_value(spatagg_psx$datebits$date |> unique(), style = 'deparse')


})


# Non-module data ------------------------------------------------------------

test_that('non-module works' {

  # setup
  austates <- readRDS(test_path("test_data", "austates.rds"))
  all_aus <- readRDS(test_path("test_data", "all_aus.rds"))

  # add a date column
  state_inputs <- austates |>
    dplyr::mutate(date = lubridate::ymd('20000101'))

  # add some values
  withr::with_seed(17,
                   state_inputs <- state_inputs |>
                     dplyr::mutate(value = runif(nrow(state_inputs)))
  )

  withr::with_seed(17,
                   # add some more days, each with different values
                   state_inputs <- purrr::map(0:10,
                                              \(x) dplyr::mutate(state_inputs,
                                                                 date = date + x,
                                                                 value = value * rnorm(nrow(state_inputs),
                                                                                       mean = x, sd = x/2))) |>
                     dplyr::bind_rows()
                   )

  # add a scenario column, each with different values
  state_inputs <- purrr::imap(letters[1:4],
                              \(x,y) dplyr::mutate(state_inputs,
                                            scenario = x,
                                            value = value + y)) |>
    dplyr::bind_rows()

  # add a theme-relevant column, each with different values
  state_inputs <- purrr::imap(c("E", "F", "G", "H", "I", "J"),
                              \(x,y) dplyr::mutate(state_inputs,
                                                   theme1 = x,
                                                   value = value + y)) |>
    dplyr::bind_rows()

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
  expect_warning(ausagg <-  multi_aggregate(
    dat = state_inputs,
    causal_edges = state_theme,
    groupers = "scenario",
    aggCols = "value",
    aggsequence = ausseq,
    funsequence = ausfuns,
    namehistory = FALSE,
    saveintermediate = TRUE
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
