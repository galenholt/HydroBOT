ewr_to_agg <- make_test_ewr_prepped()

# Sets up the earlier approach with time-means that most tests were built for
ewr_to_agg_timemean <- temporal_aggregate(ewr_to_agg,
  breaks = "all_time",
  groupers = c(
    "scenario", "gauge",
    "planning_unit_name",
    "SWSDLName",
    "ewr_code",
    "ewr_code_timing"
  ),
  aggCols = "ewr_achieved",
  funlist = "ArithmeticMean",
  prefix = ""
) |>
  dplyr::rename(ewr_achieved = ArithmeticMean_ewr_achieved)

test_that("ewr-obj works, nongeom", {
  # no need to load the demo/test data since it's in /data
  agged <- theme_aggregate(ewr_to_agg_timemean |> sf::st_drop_geometry(),
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = "mean",
    causal_edges = make_edges(
      causal_ewr,
      list(c(
        "ewr_code_timing",
        "ewr_code"
      ))
    ),
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c(
    "scenario", "gauge", "planning_unit_name",
    "SWSDLName", "ewr_code",
    "ewr_code_mean_ewr_achieved"
  ))
  expect_s3_class(agged, "data.frame")
})

test_that("auto-generating causal_edges works", {
  agged <- theme_aggregate(ewr_to_agg_timemean |> sf::st_drop_geometry(),
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = "mean",
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c(
    "scenario", "gauge", "planning_unit_name",
    "SWSDLName", "ewr_code", "ewr_code_mean_ewr_achieved"
  ))
  expect_s3_class(agged, "data.frame")
})

test_that("spatial input data works", {
  agged <- theme_aggregate(ewr_to_agg_timemean,
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = "mean",
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )

  expect_equal(names(agged), c(
    "scenario", "gauge", "polyID", "planning_unit_name", "SWSDLName",
    "ewr_code", "ewr_code_mean_ewr_achieved",
    "geometry"
  ))
  expect_s3_class(agged, "data.frame")
  expect_s3_class(agged, "sf")

  # Not usually how geonames will be used (it persists extra cols), but it does work as a test
  agged <- theme_aggregate(ewr_to_agg_timemean |>
                             dplyr::mutate(site = paste0(gauge, '_site')),
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = "mean",
    causal_edges = causal_ewr,
    geonames = "site",
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c(
    "scenario", "gauge", "polyID", "planning_unit_name", "SWSDLName",
    "ewr_code", "ewr_code_mean_ewr_achieved",
    "site",
    "geometry"
  ))
  expect_s3_class(agged, "data.frame")
  expect_s3_class(agged, "sf")
})

test_that("bare functions", {
  agged <- theme_aggregate(ewr_to_agg_timemean,
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = mean,
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c(
    "scenario", "gauge", "polyID", "planning_unit_name", "SWSDLName",
    "ewr_code", "ewr_code_mean_ewr_achieved",
    "geometry"
  ))
  expect_s3_class(agged, "data.frame")
})

test_that("list functions", {
  agged <- theme_aggregate(ewr_to_agg_timemean,
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = list(mean = ~ mean(., na.rm = TRUE)),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged), c(
    "scenario", "gauge", "polyID", "planning_unit_name", "SWSDLName",
    "ewr_code", "ewr_code_mean_ewr_achieved",
    "geometry"
  ))
  expect_s3_class(agged, "data.frame")
})

test_that("multiple functions", {
  # Character
  agged_c <- theme_aggregate(ewr_to_agg_timemean,
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = c("mean", "sd"),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_c), c(
    "scenario", "gauge", "polyID", "planning_unit_name", "SWSDLName",
    "ewr_code",
    "ewr_code_mean_ewr_achieved",
    "ewr_code_sd_ewr_achieved",
    "geometry"
  ))
  expect_s3_class(agged_c, "data.frame")

  # bare
  agged_b <- theme_aggregate(ewr_to_agg_timemean,
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = c(mean, sd),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_b), c(
    "scenario", "gauge", "polyID", "planning_unit_name", "SWSDLName",
    "ewr_code",
    "ewr_code_mean_ewr_achieved",
    "ewr_code_sd_ewr_achieved",
    "geometry"
  ))
  expect_s3_class(agged_b, "data.frame")

  # List
  agged_l <- theme_aggregate(ewr_to_agg_timemean,
    from_theme = "ewr_code_timing",
    to_theme = "ewr_code",
    groupers = c("scenario", "gauge"),
    aggCols = "ewr_achieved",
    funlist = list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    ),
    causal_edges = causal_ewr,
    auto_ewr_PU = TRUE
  )
  expect_equal(names(agged_l), c(
    "scenario", "gauge", "polyID", "planning_unit_name", "SWSDLName",
    "ewr_code",
    "ewr_code_mean_ewr_achieved",
    "ewr_code_sd_ewr_achieved",
    "geometry"
  ))
  expect_s3_class(agged_l, "data.frame")
})
