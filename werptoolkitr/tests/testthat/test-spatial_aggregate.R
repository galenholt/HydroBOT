test_that("gauge to poly works", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  spatagg <- spatial_aggregate(sumspat,
                               to_geo = sdl_units,
                               groupers = 'scenario',
                               aggCols = 'ewr_achieved',
                               funlist = 'mean')
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 6)

  # Keeping the whole set of polys
  spataggkeep <- spatial_aggregate(sumspat,
                                   to_geo = sdl_units,
                                   groupers = 'scenario',
                                   aggCols = 'ewr_achieved',
                                   funlist = 'mean',
                                   keepAllPolys = TRUE)
  # stringr::str_flatten(names(spatagg), "', '")
  # namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spataggkeep), namestring)
  expect_s3_class(spataggkeep, 'sf')
  expect_equal(nrow(spataggkeep), nrow(sdl_units)*length(unique(sumspat$scenario)))

  # Plots are useful for checking spatial outcomes
  g2sdl_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data =spatagg,
                     ggplot2::aes(fill = spatial_mean_ewr_achieved)) +
    ggplot2::geom_sf(data = sumspat) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'bottom')

  g2sdl_all_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data =spataggkeep,
                     ggplot2::aes(fill = spatial_mean_ewr_achieved)) +
    ggplot2::geom_sf(data = sumspat) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauge to sdl", g2sdl_plot)
  vdiffr::expect_doppelganger("gauge to sdl all", g2sdl_all_plot)
})

test_that("poly to poly works", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  g2pagg <- spatial_aggregate(sumspat,
                              to_geo = sdl_units,
                              groupers = 'scenario',
                              aggCols = 'ewr_achieved',
                              funlist = 'mean')
  # Expect_warning because sf throws a warning about spatially constant attributes
  expect_warning(p2pagg <- spatial_aggregate(g2pagg,
                                             to_geo = cewo_valleys,
                                             groupers = 'scenario',
                                             aggCols = 'spatial_mean_ewr_achieved',
                                             funlist = 'mean'))

  # stringr::str_flatten(names(p2pagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_spatial_mean_ewr_achieved',
                  'ValleyName', 'ValleyID', 'ValleyCode', 'geometry')
  expect_equal(names(p2pagg), namestring)
  expect_s3_class(p2pagg, 'sf')
  expect_equal(nrow(p2pagg), 21)

  # Plots are useful for checking spatial outcomes
  g2sdl2cewo_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data =p2pagg,
                     ggplot2::aes(fill = spatial_mean_spatial_mean_ewr_achieved)) +
    ggplot2::geom_sf(data = sumspat) +
    ggplot2::facet_wrap(~scenario) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("g2sdl2cewo", g2sdl2cewo_plot)

})


# argument formats: functions --------------------------------------------------------


test_that("bare functions", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  spatagg <- spatial_aggregate(sumspat,
                               to_geo = sdl_units,
                               groupers = 'scenario',
                               aggCols = 'ewr_achieved',
                               funlist = mean)
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 6)
})

test_that("list functions", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  spatagg <- spatial_aggregate(sumspat,
                               to_geo = sdl_units,
                               groupers = 'scenario',
                               aggCols = 'ewr_achieved',
                               funlist = list(mean = ~mean(., na.rm = TRUE)))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 6)
})

test_that("multiple functions", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  # character
  spatagg_c <- spatial_aggregate(sumspat,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = c('mean', 'sd'))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_sd_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg_c), namestring)
  expect_s3_class(spatagg_c, 'sf')
  expect_equal(nrow(spatagg_c), 6)

  # multiple bare names works if done in the call
  spatagg_b <- spatial_aggregate(sumspat,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = c(mean, sd))
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_sd_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg_b), namestring)
  expect_s3_class(spatagg_b, 'sf')
  expect_equal(nrow(spatagg_b), 6)

  # multiple bare names does not work if declared externally
  bare2 <- c(mean, sd)
  expect_error(spatagg_b2 <- spatial_aggregate(sumspat,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = bare2))

  # list- typically specified outside the call
  fnlist <- list(mean = ~mean(., na.rm = TRUE),
                 sd = ~sd(., na.rm = TRUE))
  spatagg_l <- spatial_aggregate(sumspat,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = fnlist)

  expect_equal(names(spatagg_l), namestring)
  expect_s3_class(spatagg_l, 'sf')
  expect_equal(nrow(spatagg_l), 6)

  # # Should also work with other anonymous function, but only actually has once.
  # # list- typically specified outside the call
  # For some reason this version gets passed in as a character "fnlistx", not a
  # list. We can see the list if we type `fnlistx`, but `funlist` is a
  # character. Need to do some more sorting out to get it to work.
  # fnlistx <- list(mean = \(x) mean(x, na.rm = TRUE),
  #                sd = \(x) sd(x, na.rm = TRUE))
  # spatagg_lx <- spatial_aggregate(sumspat,
  #                                to_geo = sdl_units,
  #                                groupers = 'scenario',
  #                                aggCols = 'ewr_achieved',
  #                                funlist = fnlistx)
  #
  # expect_equal(names(spatagg_lx), namestring)
  # expect_s3_class(spatagg_lx, 'sf')
  # expect_equal(nrow(spatagg_lx), 6)


  # vector arguments (e.g. weighting)
  wtgauge <- sumspat %>%
    dplyr::group_by(scenario, gauge) %>%
    dplyr::mutate(wt = dplyr::n()) %>%
    dplyr::ungroup()

  fnlistw <- rlang::quo(list(mean = ~mean(., na.rm = TRUE),
                 wm = ~weighted.mean(., w = wt, na.rm = TRUE)))

  spatagg_lw <- spatial_aggregate(wtgauge,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = fnlistw)

  namestringw <- c('scenario', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_wm_ewr_achieved', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg_lw), namestringw)
  expect_s3_class(spatagg_l, 'sf')
  expect_equal(nrow(spatagg_l), 6)

  # DPLYR 1.1 WILL BREAK THIS- need to sort that out sometime
  fnlistw <- rlang::quo(list(mean = ~mean(., na.rm = TRUE),
                  wm = ~weighted.mean(., wt, na.rm = TRUE)))

  sumdat <- prep_ewr_agg(summary_ewr_output, type = 'summary',
                         geopath = bom_basin_gauges)

  wtgauge2 <- sumdat %>%
    dplyr::group_by(scenario, gauge) %>%
    dplyr::mutate(wt = dplyr::n()) %>%
    dplyr::ungroup()

  spataggsumdat_lw <- spatial_aggregate(dat = wtgauge2,
                                 to_geo = sdl_units,
                                 groupers = c('scenario', 'env_obj'),
                                 aggCols = 'ewr_achieved',
                                 funlist = fnlistw,
                                 keepAllPolys = TRUE,
                                 failmissing = FALSE)

})


# argument formats: groupers and aggcol --------------------------------------------------------

# Single characters already tested

# character vectors for groups and aggCols
test_that("bare functions", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  spatagg <- spatial_aggregate(sumspat,
                               to_geo = sdl_units,
                               groupers = c('scenario', 'ewr_code'),
                               aggCols = c('ewr_achieved', 'event_count'),
                               funlist = mean)
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_mean_event_count', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 78)
})

test_that("bare names", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  spatagg <- spatial_aggregate(sumspat,
                               to_geo = sdl_units,
                               groupers = c(scenario, ewr_code),
                               aggCols = c(ewr_achieved, event_count),
                               funlist = mean)
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_mean_event_count', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 78)
})

test_that("tidyselect and mixed", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  # this gets extra fancy with the -contains, since sumspat has a
  # 'scenario_path' column, making it a good edge case test
  spatagg <- spatial_aggregate(sumspat,
                               to_geo = sdl_units,
                               groupers = c(starts_with('sce'), ewr_code, -contains('path')),
                               aggCols = c(ends_with("achieved"), all_of('event_count')),
                               funlist = mean)
  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_mean_event_count', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 78)
})

# character vectors for groups and aggCols
test_that("failmissing", {
  sumspat <- gauge2geo(summary_ewr_output,
                       gaugelocs = bom_basin_gauges)

  # without failmissing, should error if we request extras
  expect_error(spatagg <- spatial_aggregate(sumspat,
                                            to_geo = sdl_units,
                                            groupers = c('scenario', 'ewr_code', 'fake_grp'),
                                            aggCols = c('ewr_achieved', 'event_count', 'fake_vals'),
                                            funlist = mean))

  # Should work and ignore the extras with failmissing = FALSE
  spatagg <- spatial_aggregate(sumspat,
                               to_geo = sdl_units,
                               groupers = c('scenario', 'ewr_code', 'fake_grp'),
                               aggCols = c('ewr_achieved', 'event_count', 'fake_vals'),
                               funlist = mean,
                               failmissing = FALSE)

  # stringr::str_flatten(names(spatagg), "', '")
  namestring <- c('scenario', 'ewr_code', 'polyID', 'spatial_mean_ewr_achieved',
                  'spatial_mean_event_count', 'SWSDLID', 'SWSDLName', 'StateID', 'geometry')
  expect_equal(names(spatagg), namestring)
  expect_s3_class(spatagg, 'sf')
  expect_equal(nrow(spatagg), 78)
})

# todo --------------------------------------------------------------------
# ...
