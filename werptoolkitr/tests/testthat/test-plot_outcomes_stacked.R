test_that("basin works with single color palette", {
  basin_to_plot <- agg_theme_space$mdb %>%
    dplyr::rename(allArith = 4, oneLimiting = 5) %>% # for readability
    dplyr::filter(!is.na(Objective))

  basin_plot <- plot_outcomes_stacked(basin_to_plot,
                                      y_col = 'allArith',
                                      colorset = 'Objective',
                                      pal_list = list("scico::oslo"),
                                      sceneorder = c('down4', 'base', 'up4')) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("stacked bar simple", basin_plot)
})

test_that("multi-palette and facetting", {
  obj_sdl_to_plot <- agg_theme_space$sdl_units %>%
    dplyr::rename(allArith = 4) # for readability

  # Create a grouping variable
  obj_sdl_to_plot <- obj_sdl_to_plot |>
    dplyr::mutate(colcol = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(colcol)) |>
    dplyr::arrange(colcol, env_obj)

  # Create a palette list
  grouplist = list(EF = 'grDevices::Purp',
                   NF = 'grDevices::Mint',
                   NV = 'grDevices::Burg',
                   OS = 'grDevices::Blues',
                   WB = 'grDevices::Peach')

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_plot <- obj_sdl_to_plot |>
    plot_outcomes_stacked(y_col = 'allArith',
                          colorgroups = 'colcol',
                          colorset = 'env_obj',
                          pal_list = grouplist,
                          facet_wrapper = 'SWSDLName',
                          sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_group_sdl", sdl_plot)

  # This is really just a test that passing '.' to a facet_row or facet_col
  # behaves as expected
  sdl_plot_facrow <- obj_sdl_to_plot |>
    plot_outcomes_stacked(y_col = 'allArith',
                          colorgroups = 'colcol',
                          colorset = 'env_obj',
                          pal_list = grouplist,
                          facet_col = 'SWSDLName',
                          facet_row = '.',
                          sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_group_sdlrow", sdl_plot_facrow)

  sdl_plot_factgroup <- obj_sdl_to_plot |>
    plot_outcomes_stacked(y_col = 'allArith',
                          colorgroups = 'colcol',
                          colorset = 'env_obj',
                          pal_list = grouplist,
                          facet_row = 'colcol',
                          facet_col = 'SWSDLName',
                          sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_groupfacet_sdl", sdl_plot_factgroup)

})

test_that("flipped", {
  # What I want to do is just swap the x and fill arguments and pass in the
  # scenario palette. Can I do that easily?

  obj_sdl_to_plot <- agg_theme_space$sdl_units %>%
    dplyr::rename(allArith = 4) # for readability

  # Create a grouping variable
  obj_sdl_to_plot <- obj_sdl_to_plot |>
    dplyr::mutate(colcol = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(colcol)) |>
    dplyr::arrange(colcol, env_obj)

  scene_pal <- make_pal(levels = unique(obj_sdl_to_plot$scenario),
                        palette = 'calecopal::superbloom3')

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_plot <- obj_sdl_to_plot |>
    plot_outcomes_stacked(y_col = 'allArith',
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
  obj_pal <- make_pal(levels = unique(obj_sdl_to_plot$colcol),
                      palette = 'scico::berlin')

  sdl_plot_g <- obj_sdl_to_plot |>
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'env_obj',
                          colorgroups = 'colcol',
                          colorset = 'env_obj',
                          pal_list = obj_pal,
                          facet_row = 'SWSDLName',
                          facet_col = '.',
                          scene_pal = scene_pal,
                          sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("scenario stack group", sdl_plot_g)

  # interesting plot, good for testing labels
  sdl_plot_groupblock <- obj_sdl_to_plot |>
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'scenario',
                          colorgroups = 'colcol',
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
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'scenario',
                          colorgroups = 'colcol',
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
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'scenario',
                          colorgroups = 'colcol',
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
    dplyr::mutate(colcol = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(colcol)) |>
    dplyr::arrange(colcol, env_obj) |>
    # and join the quant descriptions
    dplyr::left_join(scenarios, by = 'scenario')

  scene_pal <- make_pal(levels = unique(obj_sdl_to_plot$scenario),
                        palette = 'calecopal::superbloom3')

  obj_pal <- make_pal(levels = unique(obj_sdl_to_plot$colcol),
                      palette = 'scico::berlin')

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_line <- obj_sdl_to_plot |>
    plot_outcomes_stacked(y_col = 'allArith',
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
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'delta',
                          y_lab = 'Proportion met',
                          x_lab = 'Change in flow',
                          transx = 'log10',
                          color_lab = 'Environmental\ngroup',
                          colorgroups = 'colcol',
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
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'delta',
                          y_lab = 'Proportion met',
                          x_lab = 'Change in flow',
                          transx = 'log10',
                          color_lab = 'Catchment',
                          colorgroups = NULL,
                          colorset = 'SWSDLName',
                          point_group = 'env_obj',
                          pal_list = list('RColorBrewer::Dark2'),
                          facet_row = 'colcol',
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
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'delta',
                          y_lab = 'Proportion met',
                          x_lab = 'Change in flow',
                          transx = 'log10',
                          color_lab = 'Environmental\ngroup',
                          colorgroups = 'colcol',
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
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'delta',
                          y_lab = 'Proportion met',
                          x_lab = 'Change in flow',
                          transx = 'log10',
                          color_lab = 'Environmental\ngroup',
                          colorgroups = 'colcol',
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
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'delta',
                          y_lab = 'Proportion met',
                          x_lab = 'Change in flow',
                          color_lab = 'Catchment',
                          colorgroups = NULL,
                          colorset = 'SWSDLName',
                          point_group = NULL,
                          pal_list = list('RColorBrewer::Dark2'),
                          facet_row = 'colcol',
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
    dplyr::mutate(colcol = stringr::str_extract(env_obj, '^[A-Z]+')) |>
    dplyr::filter(!is.na(colcol)) |>
    dplyr::arrange(colcol, env_obj) |>
    # and join the quant descriptions
    dplyr::left_join(scenarios, by = 'scenario')

  scene_pal <- make_pal(levels = unique(obj_sdl_to_plot$scenario),
                        palette = 'calecopal::superbloom3')

  obj_pal <- make_pal(levels = unique(obj_sdl_to_plot$colcol),
                      palette = 'scico::berlin')

  # Make a minimal map
  sdl_map <- obj_sdl_to_plot |>
    dplyr::filter(colcol == 'EF') |> # Need to reduce dimensionality
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'map',
                          colorgroups = NULL,
                          colorset = 'allArith',
                          pal_list = list('scico::berlin'),
                          facet_col = 'env_obj',
                          facet_row = 'scenario',
                          scene_pal = scene_pal,
                          sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("sdl_map_simple", sdl_map)

  # put the basin in the background
  sdl_basin_background <- obj_sdl_to_plot |>
    dplyr::filter(colcol == 'WB') |> # Need to reduce dimensionality
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'map',
                          colorgroups = NULL,
                          colorset = 'allArith',
                          pal_list = list('scico::berlin'),
                          facet_col = 'env_obj',
                          facet_row = 'scenario',
                          scene_pal = scene_pal,
                          sceneorder = c('down4', 'base', 'up4'),
                          plotbasin = TRUE,
                          basincolor = 'azure')

  vdiffr::expect_doppelganger("sdl_basin_background", sdl_basin_background)

  # include gauges- looks better without facetting
  sdl_gauges_all <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'map',
                          colorgroups = NULL,
                          colorset = 'allArith',
                          pal_list = list('scico::berlin'),
                          facet_col = 'scenario',
                          facet_row = 'env_obj',
                          scene_pal = scene_pal,
                          sceneorder = c('down4', 'base', 'up4'),
                          plotbasin = TRUE,
                          plotgauges = 'all') +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_all", sdl_gauges_all)

  sdl_gauges_clip <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'map',
                          colorgroups = NULL,
                          colorset = 'allArith',
                          pal_list = list('scico::berlin'),
                          facet_col = 'scenario',
                          facet_row = 'env_obj',
                          scene_pal = scene_pal,
                          sceneorder = c('down4', 'base', 'up4'),
                          plotbasin = TRUE,
                          plotgauges = 'clip') +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_clip", sdl_gauges_clip)

  # Does it work for gauges?
  gauges_map <- agg_theme_space$env_obj |>
    dplyr::rename(allArith = 5) |> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'map',
                          colorgroups = NULL,
                          colorset = 'allArith',
                          pal_list = list('scico::berlin'),
                          facet_col = 'scenario',
                          facet_row = 'env_obj',
                          scene_pal = scene_pal,
                          sceneorder = c('down4', 'base', 'up4'),
                          plotbasin = TRUE) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map", gauges_map)

  # Does it work for the basin?
  basin_map <- agg_theme_space$mdb |>
    dplyr::rename(allArith = 4) |> # for readability
    dplyr::filter(Objective %in% c("Maintain water-dependent species richness",
                                 "Increase opportunities for colonial waterbird breeding*",
                                 "Support instream & floodplain productivity")) |> # Need to reduce dimensionality
    plot_outcomes_stacked(y_col = 'allArith',
                          x_col = 'map',
                          colorgroups = NULL,
                          colorset = 'allArith',
                          pal_list = list('scico::berlin'),
                          facet_col = 'scenario',
                          facet_row = 'Objective',
                          scene_pal = scene_pal,
                          sceneorder = c('down4', 'base', 'up4'),
                          plotgauges = 'clip')+
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("basin_map", basin_map)

})
