
agg_theme_space <- make_test_agg()

test_that("basin works with single color palette", {
  basin_to_plot <- agg_theme_space$mdb |>
    dplyr::rename(allArith = 4, oneLimiting = 5) |> # for readability
    dplyr::filter(!is.na(Objective))

  basin_plot <- plot_outcomes(basin_to_plot,
                              y_col = 'allArith',
                              colorset = 'Objective',
                              pal_list = list("scico::oslo"),
                              sceneorder = c('down4', 'base', 'up4')) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("stacked bar simple", basin_plot)
})

test_that("multi-palette and facetting", {
  obj_sdl_to_plot <- agg_theme_space$sdl_units |>
    dplyr::rename(allArith = 4) # for readability

  # Create a grouping variable
  obj_sdl_to_plot <- obj_sdl_to_plot |>
    dplyr::mutate(env_group = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(env_group)) |>
    dplyr::arrange(env_group, env_obj)

  # Create a palette list
  grouplist = list(EB = 'grDevices::Grays',
                   EF = 'grDevices::Purp',
                   NF = 'grDevices::Mint',
                   NV = 'grDevices::Burg',
                   OS = 'grDevices::Blues',
                   WB = 'grDevices::Peach')

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_plot <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = grouplist,
                  facet_wrapper = 'SWSDLName',
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_group_sdl", sdl_plot)

  # This is really just a test that passing '.' to a facet_row or facet_col
  # behaves as expected
  sdl_plot_facrow <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = grouplist,
                  facet_col = 'SWSDLName',
                  facet_row = '.',
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_group_sdlrow", sdl_plot_facrow)

  sdl_plot_factgroup <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = grouplist,
                  facet_row = 'env_group',
                  facet_col = 'SWSDLName',
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_groupfacet_sdl", sdl_plot_factgroup)

})

test_that("flipped", {
  # What I want to do is just swap the x and fill arguments and pass in the
  # scenario palette. Can I do that easily?

  obj_sdl_to_plot <- agg_theme_space$sdl_units |>
    dplyr::rename(allArith = 4) # for readability

  # Create a grouping variable
  obj_sdl_to_plot <- obj_sdl_to_plot |>
    dplyr::mutate(env_group = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(env_group)) |>
    dplyr::arrange(env_group, env_obj)

  scene_pal <- make_pal(levels = unique(obj_sdl_to_plot$scenario),
                        palette = 'calecopal::superbloom3')

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_plot <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'env_obj',
                  colorgroups = NULL,
                  colorset = 'env_obj',
                  pal_list = list('scico::berlin'),
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("scenario stack", sdl_plot)


  # outcome groups- If I want to do this, I need pass a named palette, I think
  # It doesn't actually plot any differently at present, but I'm leaving it here
  # because if we can figure out how to plot those colors in a not-ugly way,
  # we'll be there.
  obj_pal <- make_pal(levels = unique(obj_sdl_to_plot$env_group),
                      palette = 'scico::berlin')

  sdl_plot_g <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'env_obj',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("scenario stack group", sdl_plot_g)

  # interesting plot, good for testing labels
  sdl_plot_groupblock <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'scenario',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'))
  vdiffr::expect_doppelganger("stacked_objgroups", sdl_plot_groupblock)

  # change label name
  # interesting plot, good for testing labels
  sdl_plot_groupblock_l <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'scenario',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  color_lab = 'Environmental\ngroup',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'))
  vdiffr::expect_doppelganger("legend_name", sdl_plot_groupblock_l)

  # Drop labels
  sdl_plot_groupblock_n <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'scenario',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  color_lab = NULL,
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("no legend title", sdl_plot_groupblock_n)

  # do I want to allow dropping the legend?

})

test_that("quant x", {
  # What I want to do is just use a quantitative x and have it automatically use
  # a line. Can I do that easily?

  obj_sdl_to_plot <- agg_theme_space$sdl_units |>
    dplyr::rename(allArith = 4) # for readability

  # create a quant description of scenarios
  scenarios <- tibble::tibble(scenario = c('base', 'down4', 'up4'), delta = c(1, 0.25, 4))

  # Create a grouping variable
  obj_sdl_to_plot <- obj_sdl_to_plot |>
    dplyr::mutate(env_group = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(env_group)) |>
    dplyr::arrange(env_group, env_obj) |>
    # and join the quant descriptions
    dplyr::left_join(scenarios, by = 'scenario')

  scene_pal <- make_pal(levels = unique(obj_sdl_to_plot$scenario),
                        palette = 'calecopal::superbloom3')

  obj_pal <- make_pal(levels = unique(obj_sdl_to_plot$env_group),
                      palette = 'scico::berlin')

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_line <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'delta',
                  colorgroups = NULL,
                  colorset = 'env_obj',
                  pal_list = list('scico::berlin'),
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("line defaults", sdl_line)

  # change a bunch of options
  sdl_line_options <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'delta',
                  y_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Environmental\ngroup',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  base_lev = 'base',
                  comp_fun = 'difference',
                  group_cols = c('env_obj', 'polyID'))

  vdiffr::expect_doppelganger("line with scaling and labels", sdl_line_options)

  # check that it works with other things as colors
  # and that the point_group argument works.
  sdl_line_catchment <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'delta',
                  y_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Catchment',
                  colorgroups = NULL,
                  colorset = 'SWSDLName',
                  point_group = 'env_obj',
                  pal_list = list('RColorBrewer::Dark2'),
                  facet_row = 'env_group',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  base_lev = 'base',
                  comp_fun = 'difference',
                  group_cols = c('env_obj', 'polyID'),
                  smooth = FALSE)
  vdiffr::expect_doppelganger("line by catchment with obj groups", sdl_line_catchment)

  # jittering- set the seed each time or the jitters differ
  set.seed(18)
  sdl_smooth_mean_jf <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'delta',
                  y_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Environmental\ngroup',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  base_lev = 'base',
                  comp_fun = 'difference',
                  group_cols = c('polyID'),
                  smooth = TRUE,
                  smooth_method = 'lm',
                  position = ggplot2::position_jitter(width = 0.01, height = 0))
  vdiffr::expect_doppelganger("jitter_function", sdl_smooth_mean_jf)

  # jittering- default
  set.seed(18)
  sdl_smooth_mean_jc <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'delta',
                  y_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Environmental\ngroup',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  base_lev = 'base',
                  comp_fun = 'difference',
                  group_cols = c('polyID'),
                  smooth = TRUE,
                  smooth_method = 'lm',
                  position = 'jitter')

  vdiffr::expect_doppelganger("jitter_character", sdl_smooth_mean_jc)

  skip("skip loess- warnings can't be silenced. Inspect visually")
  # The loess isn't happy about singularities. I don't want to silence them, but also don't care for this example.
  sdl_line_catchment_smooth <- obj_sdl_to_plot |>
    plot_outcomes(y_col = 'allArith',
                  x_col = 'delta',
                  y_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  color_lab = 'Catchment',
                  colorgroups = NULL,
                  colorset = 'SWSDLName',
                  point_group = NULL,
                  pal_list = list('RColorBrewer::Dark2'),
                  facet_row = 'env_group',
                  facet_col = '.',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  base_lev = 'base',
                  comp_fun = 'difference',
                  group_cols = c('env_obj', 'polyID'),
                  smooth = TRUE)

  vdiffr::expect_doppelganger("smoothed_lines",
                              suppressWarnings(print(sdl_line_catchment_smooth)))

})

test_that("maps", {

  obj_sdl_to_plot <- agg_theme_space$sdl_units |>
    dplyr::rename(allArith = 4) # for readability

  # create a quant description of scenarios
  scenarios <- tibble::tibble(scenario = c('base', 'down4', 'up4'), delta = c(1, 0.25, 4))

  # Create a grouping variable
  obj_sdl_to_plot <- obj_sdl_to_plot |>
    dplyr::mutate(env_group = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(env_group)) |>
    dplyr::arrange(env_group, env_obj) |>
    # and join the quant descriptions
    dplyr::left_join(scenarios, by = 'scenario')

  scene_pal <- make_pal(levels = unique(obj_sdl_to_plot$scenario),
                        palette = 'calecopal::superbloom3')

  obj_pal <- make_pal(levels = unique(obj_sdl_to_plot$env_group),
                      palette = 'scico::berlin')

  env_obj_points_to_plot <- agg_theme_space$env_obj |>
    dplyr::rename(allArith = 5)

  # Make a minimal map
  sdl_map <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'EF') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("sdl_map_simple", sdl_map)

  # control the limits- typically this would be done over several plots
  # Make a minimal map
  sdl_mapL <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'EF') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  setLimits = c(0, 2))
  vdiffr::expect_doppelganger("sdl_map_limits", sdl_mapL)


  # put the basin in the background
  sdl_basin_background <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = list(underlay = basin, underlay_pal = 'azure'))

  vdiffr::expect_doppelganger("sdl_basin_background", sdl_basin_background)

  # If I try to put a palette on the background with a palette in the foreground, it should warn and swap to NA
  # Using cewo valleys because then it makes sense to have a fill sometimes.
  expect_warning(sdl_valley_background_warn <- obj_sdl_to_plot |>
                   dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
                   plot_outcomes(y_col = 'allArith',
                                 x_col = 'map',
                                 colorgroups = NULL,
                                 colorset = 'allArith',
                                 pal_list = list('scico::berlin'),
                                 facet_col = 'env_obj',
                                 facet_row = 'scenario',
                                 scene_pal = scene_pal,
                                 sceneorder = c('down4', 'base', 'up4'),
                                 underlay_list = list(underlay = cewo_valleys,
                                                      underlay_ycol = 'ValleyName',
                                                      underlay_pal = 'scico::hawaii')))

  vdiffr::expect_doppelganger("sdl_valley_background_warn", sdl_valley_background_warn)

  # gauges as main focus
  gauges_map <- env_obj_points_to_plot |> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = list(underlay = 'basin',
                                       underlay_pal = 'azure')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map", gauges_map)

  # gauges with filled underlay values
  gauges_map_sdl <- env_obj_points_to_plot |> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = list(underlay = sdl_units,
                                       underlay_ycol = 'SWSDLName',
                                       underlay_pal = 'scico::oslo')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map_sdl", gauges_map_sdl)

  # Test with a continuous variable too
  # Have to be careful with these though to not have the underlay overwhelm the main data- see the filtering.
  # Should be able to do that automatically
  gauges_map_sdl_agg <- env_obj_points_to_plot |> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = list(underlay = dplyr::filter(obj_sdl_to_plot, env_obj == 'NF1'),
                                       underlay_ycol = 'allArith',
                                       underlay_pal = 'scico::oslo')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map_sdl_agg", gauges_map_sdl_agg)

  # that one would be good with two levels- basin and sdl
  gauges_map_2_level <- env_obj_points_to_plot |> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = list(list(underlay = 'basin',
                                            underlay_pal = 'cornsilk'),
                                       list(underlay = dplyr::filter(obj_sdl_to_plot, env_obj == 'NF1'),
                                            underlay_ycol = 'allArith',
                                            underlay_pal = 'scico::oslo'))) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map_2_level", gauges_map_2_level)

  # include gauges- looks better without facetting
  sdl_gauges_all <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = 'basin',
                  overlay_list = list(overlay = 'bom_basin_gauges', overlay_pal = 'purple')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_all", sdl_gauges_all)

  # clipped to the main data
  sdl_gauges_clip <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = 'basin',
                  overlay_list = list(overlay = 'bom_basin_gauges',
                                      overlay_pal = 'purple',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_clip", sdl_gauges_clip)

  # with a meaningful palette- simple qualitative to start
  sdl_gauges_qual <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = 'basin',
                  overlay_list = list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                      overlay_pal = 'ggsci::nrc_npg',
                                      overlay_ycol = 'scenario',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_qual", sdl_gauges_qual)

  # with a meaningful palette- simple quantitative and an underlay
  sdl_gauges_quant <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = 'basin',
                  overlay_list = list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                      overlay_pal = 'scico::oslo',
                                      overlay_ycol = 'allArith',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_quant", sdl_gauges_quant)


  # check that scenariofilter works on underlay and overlay
  # that one would be good with two levels- basin and sdl
  gauges_scenefilter_u <- env_obj_points_to_plot |> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  scenariofilter = c('down4', 'up4'),
                  underlay_list = list(list(underlay = dplyr::filter(obj_sdl_to_plot, env_obj == 'NF1'),
                                            underlay_ycol = 'allArith',
                                            underlay_pal = 'scico::oslo'))) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_scenefilter_u", gauges_scenefilter_u)

  gauges_scenefilter_o <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = 'basin',
                  scenariofilter = c('down4', 'up4'),
                  overlay_list = list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                      overlay_pal = 'scico::oslo',
                                      overlay_ycol = 'allArith',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_scenefilter_o", gauges_scenefilter_o)


  # Does it work for the basin?
  basin_map <- agg_theme_space$mdb |>
    dplyr::rename(allArith = 4) |> # for readability
    dplyr::filter(Objective %in% c("Maintain water-dependent species richness",
                                   "Increase opportunities for colonial waterbird breeding*",
                                   "Support instream & floodplain productivity")) |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'Objective',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  overlay_list = 'bom_basin_gauges')+
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("basin_map", basin_map)

  # How about multi-overlays
  basin_map_multi <- agg_theme_space$mdb |>
    dplyr::rename(allArith = 4) |> # for readability
    dplyr::filter(Objective %in% c("Maintain water-dependent species richness",
                                   "Increase opportunities for colonial waterbird breeding*",
                                   "Support instream & floodplain productivity")) |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'Objective',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  overlay_list = list(list(overlay = 'sdl_units', overlay_pal = 'black'),
                                      list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                           overlay_pal = 'scico::oslo',
                                           overlay_ycol = 'allArith')))+
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("basin_map_multi", basin_map_multi)


  # How under- and overlays. nearly identical to above, but make sure palettes work with under
  sdl_gauges_quant_basinpal <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  underlay_list = list(underlay = 'basin', underlay_pal = "azure"),
                  overlay_list = list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                      overlay_pal = 'scico::oslo',
                                      overlay_ycol = 'allArith',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_quant_basinpal", sdl_gauges_quant_basinpal)

  # Difference from baseline
  # put the basin in the background

  sdl_basin_background_difference <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('ggthemes::Orange-Blue-White Diverging'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  base_lev = 'base',
                  comp_fun = 'difference',
                  group_cols = c('env_obj', 'polyID'), # Do I need to group_by polyID for the maps? Yes. should probably automate that.
                  underlay_list = list(underlay = basin, underlay_pal = 'azure'))

  vdiffr::expect_doppelganger("sdl_basin_background_difference", sdl_basin_background_difference)

  # Relative to baseline
  sdl_basin_background_rel <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
    plot_outcomes(y_col = 'allArith',
                  x_col = 'map',
                  colorgroups = NULL,
                  colorset = 'allArith',
                  pal_list = list('ggthemes::Orange-Blue-White Diverging'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  scene_pal = scene_pal,
                  sceneorder = c('down4', 'base', 'up4'),
                  base_lev = 'base',
                  comp_fun = 'relative',
                  zero_adjust = 'auto',
                  transy = 'log10',
                  group_cols = c('env_obj', 'polyID'), # Do I need to group_by polyID for the maps? Yes. should probably automate that.
                  underlay_list = list(underlay = basin, underlay_pal = 'azure'))

  skip("strange bug introduces NaN and Inf, but only when printed to vdiffr. Inspect manually")
  # It's unclear why, but just plotting the plot does *not* throw warnings, and
  # does *not* have NaNs. But when it prints, it does. I can't find the bug, so
  # for the moment checking that it works right in normal use by just having a
  # bare object call, and suppressing warnings in the vdiffr. Changing the
  # filename seems to have fixed it so I've removed the expect_warning until it
  # crops back up. It's intermittent, so I'm really not sure what to do about
  # it. NOW it's *not* throwing the warning when wrapped in `expect_warning`,
  # but *is* when it's not. So I'm just going to skip it until I can figure out
  # what's going on, I think. And leave the bare call here just to make sure it doesn't start throwing a warning in normal use.
  sdl_basin_background_rel
  vdiffr::expect_doppelganger("sdl_basin_background_rel", sdl_basin_background_rel)
})

test_that("ewr works as in `plot_outcomes_bar`", {
  ewr_to_bar_data <- summary_ewr_output |>
    # just grab the first code_timing
    dplyr::group_by(ewr_code, gauge, scenario) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::filter(ewr_code %in% c('BF1', 'LF1', "OB5") &
                    gauge %in% c("412002", "412005", "412038"))

  scene_pal <- make_pal(levels = unique(ewr_to_bar_data$scenario), palette = 'calecopal::superbloom3')


  ewr_plot <- plot_outcomes(ewr_to_bar_data,
                            y_col = 'ewr_achieved',
                            x_col = 'scenario',
                            facet_row = 'gauge',
                            facet_col = 'ewr_code',
                            colorset = 'scenario',
                            pal_list = scene_pal,
                            sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_ewr", ewr_plot)
})

test_that("basin works as in `plot_outcomes_bar` (facet_wrap, no gauge, better aggregated)", {
  basin_to_plot <- agg_theme_space$mdb |>
    dplyr::rename(allArith = 4, oneLimiting = 5) |> # for readability
    dplyr::filter(!is.na(Objective))

  scene_pal <- make_pal(levels = unique(basin_to_plot$scenario), palette = 'calecopal::superbloom3')
  basin_plot <- plot_outcomes(basin_to_plot,
                              y_col = 'allArith',
                              x_col = 'scenario',
                              colorset = 'scenario',
                              pal_list = scene_pal,
                              facet_wrapper = 'Objective',
                              sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin", basin_plot)

  basin_plot_L <- plot_outcomes(basin_to_plot,
                                y_col = 'allArith',
                                y_lab = "Aggregated outcome",
                                facet_wrapper = 'Objective',
                                colorset = 'scenario',
                                pal_list = scene_pal,
                                sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_lab", basin_plot_L)
})

