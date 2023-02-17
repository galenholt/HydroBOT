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


# argument formats --------------------------------------------------------


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

  # bare- naming fails
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

  # list
  spatagg_l <- spatial_aggregate(sumspat,
                                 to_geo = sdl_units,
                                 groupers = 'scenario',
                                 aggCols = 'ewr_achieved',
                                 funlist = list(mean = ~mean(., na.rm = TRUE),
                                                sd = ~sd(., na.rm = TRUE)))

  expect_equal(names(spatagg_l), namestring)
  expect_s3_class(spatagg_l, 'sf')
  expect_equal(nrow(spatagg_l), 6)


})

# todo --------------------------------------------------------------------

# bare names groupers and aggcols
# multiple aggcols
# ...
