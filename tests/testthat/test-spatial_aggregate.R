rlang::local_options(lifecycle_verbosity = "error")

ewr_to_agg <- make_test_ewr_prepped()

# Sets up the earlier approach with time-means that most tests were built for
ewr_to_agg_timemean <- temporal_aggregate(ewr_to_agg,
                                          breaks = 'all_time',
                                          groupers = c('scenario', 'gauge',
                                                       'planning_unit_name',
                                                       'SWSDLName', 'ewr_code',
                                                       'ewr_code_timing'),
                                          aggCols = 'ewr_achieved',
                                          funlist = 'ArithmeticMean',
                                          prefix = '') |>
  dplyr::rename(ewr_achieved = ArithmeticMean_ewr_achieved)

test_that("gauge to poly works", {


  # The warning is because this shouldn't really be a spatial aggregation
  expect_warning(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = sdl_units,
                               groupers = 'scenario',
                               aggCols = 'ewr_achieved',
                               funlist = 'mean'))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved',
                  'SWSDLID', 'StateID', 'SWSDLName', 'geometry')
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 8)

  # Keeping the whole set of polys
  # Warning because this shouldn't be a spatial aggregation in typical use
  expect_warning(spataggkeep <- spatial_aggregate(ewr_to_agg_timemean,
                                   to_geo = sdl_units,
                                   groupers = 'scenario',
                                   aggCols = 'ewr_achieved',
                                   funlist = 'mean',
                                   keepAllPolys = TRUE))

  # stringr::str_flatten(names(spatagg), "', '")
  # namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spataggkeep), namestring)
  expect_s3_class(spataggkeep, 'sf')
  expect_equal(nrow(spataggkeep), nrow(sdl_units)*length(unique(ewr_to_agg_timemean$scenario)))

  # Plots are useful for checking spatial outcomes
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data =spatagg,
                     ggplot2::aes(fill = spatial_mean_ewr_achieved)) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'bottom')

  g2sdl_all_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data =spataggkeep,
                     ggplot2::aes(fill = spatial_mean_ewr_achieved)) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'bottom')

  skip_on_os('linux')
  vdiffr::expect_doppelganger("gauge to sdl", g2sdl_plot)
  vdiffr::expect_doppelganger("gauge to sdl all", g2sdl_all_plot)
})

test_that("poly to poly works", {

  skip_on_os('linux')

  # Warning because this shouldn't be a spatial aggregation in typical use
  expect_warning(g2pagg <- spatial_aggregate(ewr_to_agg_timemean,
                              to_geo = sdl_units,
                              groupers = 'scenario',
                              aggCols = 'ewr_achieved',
                              funlist = 'mean'))
  # Expect_warning because sf throws a warning about spatially constant attributes
  p2pagg <- spatial_aggregate(g2pagg,
                              to_geo = cewo_valleys,
                              groupers = 'scenario',
                              aggCols = 'spatial_mean_ewr_achieved',
                              funlist = 'mean')

  # stringr::str_flatten(names(p2pagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_spatial_mean_ewr_achieved',
                  'ValleyName', 'ValleyID', 'ValleyCode', 'geometry')
  expect_equal(names(p2pagg), namestring)
  expect_s3_class(p2pagg, 'sf')
  expect_equal(nrow(p2pagg), 28)

  # Plots are useful for checking spatial outcomes
  g2sdl2cewo_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data =p2pagg,
                     ggplot2::aes(fill = spatial_mean_spatial_mean_ewr_achieved)) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'bottom')

  skip_on_os('linux')
  vdiffr::expect_doppelganger("g2sdl2cewo", g2sdl2cewo_plot)

})

test_that("psuedo-spatial works", {

  spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = planning_units,
                               groupers = 'scenario',
                               aggCols = 'ewr_achieved',
                               funlist = 'mean',
                               joinby = 'nonspatial')
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'PlanningUnitName', 'LTWPShortName', 'PU_Region', 'PU_Code', 'STATE', 'geometry', 'planning_unit_name')
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 36)


  # Fail if nothing in common
  expect_error(spatagg_no_common <- spatial_aggregate(ewr_to_agg_timemean,
                                   to_geo = resource_plan_areas,
                                   groupers = 'scenario',
                                   aggCols = 'ewr_achieved',
                                   funlist = 'mean',
                                   keepAllPolys = TRUE,
                                   joinby = 'nonspatial'))

  # Plots are useful for checking spatial outcomes
  g2pu_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data =spatagg,
                     ggplot2::aes(fill = spatial_mean_ewr_achieved)) +
    ggplot2::geom_sf(data = ewr_to_agg_timemean) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'bottom')

  skip_on_os('linux')
  vdiffr::expect_doppelganger("gauge to PU", g2pu_plot)
})


# argument formats: functions --------------------------------------------------------


test_that("bare functions", {

  # Warning because this shouldn't be a spatial aggregation in typical use
  expect_warning(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = sdl_units,
                               groupers = 'scenario',
                               aggCols = 'ewr_achieved',
                               funlist = mean))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'SWSDLID', 'StateID', 'SWSDLName', 'geometry')
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 8)
})

test_that("list functions", {

  # Warning because this shouldn't be a spatial aggregation in typical use
  expect_warning(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = sdl_units,
                               groupers = 'scenario',
                               aggCols = 'ewr_achieved',
                               funlist = list(mean = ~mean(., na.rm = TRUE))))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- sort(c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry'))
  expect_equal(sort(names(spatagg)), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 8)
})

test_that("multiple functions", {

  # character
  # Warning because this shouldn't be a spatial aggregation in typical use
  expect_warning(spatagg_c <- spatial_aggregate(ewr_to_agg_timemean,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = c('mean', 'sd')))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_sd_ewr_achieved', 'SWSDLID', 'StateID', 'SWSDLName', 'geometry')
  expect_equal(sort(names(spatagg_c)), sort(namestring))
  expect_s3_class(spatagg_c, 'sf')
  expect_equal(nrow(spatagg_c), 8)

  # multiple bare names works if done in the call
  # Warning because this shouldn't be a spatial aggregation in typical use
  expect_warning(spatagg_b <- spatial_aggregate(ewr_to_agg_timemean,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = c(mean, sd)))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_sd_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(sort(names(spatagg_b)), sort(namestring))
  expect_s3_class(spatagg_b, 'sf')
  expect_equal(nrow(spatagg_b), 8)

  # multiple bare names does not work if declared externally
  bare2 <- c(mean, sd)
  expect_warning(expect_error(spatagg_b2 <- spatial_aggregate(ewr_to_agg_timemean,
                                               to_geo = sdl_units,
                                               groupers = 'scenario',
                                               aggCols = 'ewr_achieved',
                                               funlist = bare2)))

  # list- typically specified outside the call
  fnlist <- list(mean = ~mean(., na.rm = TRUE),
                 sd = ~sd(., na.rm = TRUE))
  expect_warning(spatagg_l <- spatial_aggregate(ewr_to_agg_timemean,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = fnlist))

  expect_equal(sort(names(spatagg_l)), sort(namestring))
  expect_s3_class(spatagg_l, 'sf')
  expect_equal(nrow(spatagg_l), 8)

  # # Should also work with other anonymous function, but only actually has once.
  # # list- typically specified outside the call
  # For some reason this version gets passed in as a character "fnlistx", not a
  # list. We can see the list if we type `fnlistx`, but `funlist` is a
  # character. Need to do some more sorting out to get it to work.
  # fnlistx <- list(mean = \(x) mean(x, na.rm = TRUE),
  #                sd = \(x) sd(x, na.rm = TRUE))
  # spatagg_lx <- spatial_aggregate(ewr_to_agg_timemean,
  #                                to_geo = sdl_units,
  #                                groupers = 'scenario',
  #                                aggCols = 'ewr_achieved',
  #                                funlist = fnlistx)
  #
  # expect_equal(names(spatagg_lx), namestring)
  # expect_s3_class(spatagg_lx, 'sf')
  # expect_equal(nrow(spatagg_lx), 8)


  # vector arguments (e.g. weighting)
  wtgauge <- ewr_to_agg_timemean |>
    dplyr::group_by(scenario, gauge) |>
    dplyr::mutate(wt = dplyr::n()) |>
    dplyr::ungroup()

  fnlistw <- rlang::quo(list(mean = ~mean(., na.rm = TRUE),
                             wm = ~weighted.mean(., w = wt, na.rm = TRUE)))

  expect_warning(spatagg_lw <- spatial_aggregate(wtgauge,
                                  to_geo = sdl_units,
                                  groupers = 'scenario',
                                  aggCols = 'ewr_achieved',
                                  funlist = fnlistw))

  namestringw <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved',
                   'spatial_wm_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(sort(names(spatagg_lw)), sort(namestringw))
  expect_s3_class(spatagg_lw, 'sf')
  expect_equal(nrow(spatagg_lw), 8)


  # Same, but keeping polys?
  expect_warning(spataggsumdat_lw <- spatial_aggregate(dat = wtgauge,
                                        to_geo = sdl_units,
                                        groupers = c('scenario', 'env_obj'),
                                        aggCols = 'ewr_achieved',
                                        funlist = fnlistw,
                                        keepAllPolys = TRUE,
                                        failmissing = FALSE))

})


# argument formats: groupers and aggcol --------------------------------------------------------

# Single characters already tested

# character vectors for groups and aggCols
test_that("bare functions", {

  # have to choose the only other numeric to agg
  expect_warning(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = sdl_units,
                               groupers = c('scenario', 'ewr_code'),
                               aggCols = c('ewr_achieved'),
                               funlist = mean))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 116)
})

test_that("bare names", {

  expect_warning(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = sdl_units,
                               groupers = c(scenario, ewr_code),
                               aggCols = c(ewr_achieved),
                               funlist = mean))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 116)
})

test_that("tidyselect and mixed", {

  # this gets extra fancy with the -contains, since ewr_to_agg_timemean has a
  # 'scenario_path' column, making it a good edge case test
  expect_warning(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = sdl_units,
                               groupers = c(starts_with('sce'), ewr_code, -contains('path')),
                               aggCols = c(ends_with("achieved")),
                               funlist = mean))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 116)
})

# character vectors for groups and aggCols
test_that("failmissing", {

  # without failmissing, should error if we request extras
  expect_warning(expect_error(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                                            to_geo = sdl_units,
                                            groupers = c('scenario', 'ewr_code', 'fake_grp'),
                                            aggCols = c('ewr_achieved', 'fake_vals'),
                                            funlist = mean)))

  # Should work and ignore the extras with failmissing = FALSE
  # Warning because this shouldn't be a spatial aggregation in typical use
  expect_warning(spatagg <- spatial_aggregate(ewr_to_agg_timemean,
                               to_geo = sdl_units,
                               groupers = c('scenario', 'ewr_code', 'fake_grp'),
                               aggCols = c('ewr_achieved', 'fake_vals'),
                               funlist = mean,
                               failmissing = FALSE))

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(sort(names(spatagg)), sort(namestring))
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 116)
})

# todo --------------------------------------------------------------------
# ...
