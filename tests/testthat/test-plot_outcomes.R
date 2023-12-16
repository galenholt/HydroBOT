
# Should be a way to speed this bit up, the first happens again inside the second
ewr_to_agg <- make_test_ewr_prepped()
agg_theme_space <- make_test_agg(namehistory = FALSE)

# # temp-do we have waterbirds in both catchments?
# agg_theme_space$sdl_units |>
#   dplyr::filter(grepl("^WB", env_obj)) |>
#   dplyr::group_by(SWSDLName) |>
#   dplyr::summarise(nbirds = dplyr::n())
#
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data =sdl_units) +
#   ggplot2::geom_sf(data = (agg_theme_space$env_obj |>
#                              dplyr::filter(grepl("^WB", env_obj))))

# create a quant description of scenarios
scenarios <- tibble::tibble(scenario = c('base_base', 'down4_down4', 'up4_up4'), delta = c(1, 0.25, 4))

obj_sdl_to_plot <- agg_theme_space$sdl_units |>

  dplyr::mutate(env_group = stringr::str_extract(env_obj, '^[A-Z]+')) |>
  dplyr::filter(!is.na(env_group)) |>
  dplyr::arrange(env_group, env_obj) |>
  # and join the quant descriptions
  dplyr::left_join(scenarios, by = 'scenario')

basin_to_plot <- agg_theme_space$mdb |>
  dplyr::filter(!is.na(Objective))


# palettes
SDL_pal <- make_pal(unique(agg_theme_space$sdl_units$SWSDLName),
                    palette = "ggsci::nrc_npg")

scene_pal <- make_pal(levels = unique(obj_sdl_to_plot$scenario),
                      palette = 'calecopal::superbloom3')

obj_pal <- make_pal(levels = unique(obj_sdl_to_plot$env_group),
                    palette = 'scico::berlin')

test_that("basin works with single color palette", {


  basin_plot <- plot_outcomes(basin_to_plot,
                              outcome_col = 'ewr_achieved',
                              colorset = 'Objective',
                              pal_list = list("scico::oslo"),
                              sceneorder = c('down4_down4', 'base_base', 'up4_up4')) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("stacked bar simple", basin_plot)
})

test_that("a fixed color works (contrived)", {

  basin_plotred <- plot_outcomes(basin_to_plot,
                              outcome_col = 'ewr_achieved',
                              colorset = 'Objective',
                              pal_list = 'firebrick',
                              sceneorder = c('down4_down4', 'base_base', 'up4_up4')) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("stackedred", basin_plotred)
})

test_that("multi-palette and facetting", {


  # Create a palette list
  grouplist = list(EB = 'grDevices::Grays',
                   EF = 'grDevices::Purp',
                   NF = 'grDevices::Mint',
                   NV = 'grDevices::Burg',
                   OS = 'grDevices::Blues',
                   WB = 'grDevices::Peach')

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_plot <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = grouplist,
                  facet_wrapper = 'SWSDLName',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("bar_basin_group_sdl", sdl_plot)

  # This is really just a test that passing '.' to a facet_row or facet_col
  # behaves as expected
  sdl_plot_facrow <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = grouplist,
                  facet_col = 'SWSDLName',
                  facet_row = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("bar_basin_group_sdlrow", sdl_plot_facrow)

  sdl_plot_factgroup <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  colorgroups = 'env_group',
                  colorset = 'env_obj',
                  pal_list = grouplist,
                  facet_row = 'env_group',
                  facet_col = 'SWSDLName',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("bar_basin_groupfacet_sdl", sdl_plot_factgroup)

})

test_that("flipped", {
  # What I want to do is just swap the x and fill arguments and pass in the
  # scenario palette. Can I do that easily?

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_plot <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'env_obj',
                  colorgroups = NULL,
                  colorset = 'scenario',
                  pal_list = scene_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("scenario stack", sdl_plot)


  # outcome groups
  sdl_plot_g <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'env_obj',
                  colorgroups = 'env_group',
                  colorset = 'scenario',
                  pal_list = scene_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("scenario stack group", sdl_plot_g)

  # interesting plot, good for testing labels
  sdl_plot_groupblock <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'scenario',
                  colorset = 'env_group',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("stacked_objgroups", sdl_plot_groupblock)

  # change label name
  # interesting plot, good for testing labels
  sdl_plot_groupblock_l <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'scenario',
                  colorset = 'env_group',
                  color_lab = 'Environmental\ngroup',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))
  vdiffr::expect_doppelganger("legend_name", sdl_plot_groupblock_l)

  # Drop labels
  sdl_plot_groupblock_n <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'scenario',
                  colorset = 'env_group',
                  color_lab = NULL,
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("no legend title", sdl_plot_groupblock_n)

  # do I want to allow dropping the legend?

})

test_that("quant x", {
  # What I want to do is just use a quantitative x and have it automatically use
  # a line. Can I do that easily?

  # create a quant description of scenarios
  scenarios <- tibble::tibble(scenario = c('base_base', 'down4_down4', 'up4_up4'), delta = c(1, 0.25, 4))

  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_line <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'delta',
                  colorset = 'env_obj',
                  pal_list = list('scico::berlin'),
                  facet_row = 'SWSDLName',
                  facet_col = '.')

  vdiffr::expect_doppelganger("line defaults", sdl_line)

  # change a bunch of options
  sdl_line_options <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'delta',
                  outcome_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Environmental\ngroup',
                  colorset = 'env_group',
                  point_group = 'env_obj',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  base_list = list(base_lev = 'base_base', comp_fun = 'difference',
                                   group_cols = c('env_obj', 'polyID')))

  vdiffr::expect_doppelganger("line with scaling and labels", sdl_line_options)

  # check that it works with other things as colors
  # and that the point_group argument works.
  sdl_line_catchment <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'delta',
                  outcome_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Catchment',
                  colorset = 'SWSDLName',
                  point_group = 'env_obj',
                  pal_list = list('RColorBrewer::Dark2'),
                  facet_row = 'env_group',
                  facet_col = '.',
                  base_list = list(base_lev = 'base_base', comp_fun = 'difference',
                                   group_cols = c('env_obj', 'polyID')))
  vdiffr::expect_doppelganger("line by catchment with obj groups", sdl_line_catchment)

  # jittering- set the seed each time or the jitters differ
  set.seed(18)
  sdl_smooth_mean_jf <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'delta',
                  outcome_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Environmental\ngroup',
                  colorset = 'env_group',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  base_list = list(base_lev = 'base_base',
                                   comp_fun = 'difference',
                                   group_cols = c('polyID')),
                  smooth_arglist = list(method = 'lm'),
                  position = ggplot2::position_jitter(width = 0.01, height = 0))

  vdiffr::expect_doppelganger("jitter_function", sdl_smooth_mean_jf)

  # jittering- default
  set.seed(18)
  sdl_smooth_mean_jc <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'delta',
                  outcome_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  transx = 'log10',
                  color_lab = 'Environmental\ngroup',
                  colorset = 'env_group',
                  pal_list = obj_pal,
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  base_list = list(base_lev = 'base_base',
                                   comp_fun = 'difference',
                                   group_cols = c('polyID')),
                  smooth_arglist = list(method = 'lm'),
                  position = 'jitter')

  vdiffr::expect_doppelganger("jitter_character", sdl_smooth_mean_jc)

  skip("skip loess- warnings can't be silenced. Inspect visually")
  # The loess isn't happy about singularities. I don't want to silence them, but also don't care for this example.
  # I'm not entirely sure why this works- i think I *should* have to say `smooth_arglist = TRUE`, but it seems to work with just `smooth`
  sdl_line_catchment_smooth <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'delta',
                  outcome_lab = 'Proportion met',
                  x_lab = 'Change in flow',
                  color_lab = 'Catchment',
                  colorgroups = NULL,
                  colorset = 'SWSDLName',
                  point_group = NULL,
                  pal_list = list('RColorBrewer::Dark2'),
                  facet_row = 'env_group',
                  facet_col = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  base_list = list(base_lev = 'base_base', comp_fun = 'difference',
                                   group_cols = c('env_obj', 'polyID')),
                  smooth = TRUE)

  vdiffr::expect_doppelganger("smoothed_lines",
                              suppressWarnings(print(sdl_line_catchment_smooth)))

})

test_that("maps", {

  # Make a minimal map
  sdl_map <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'EF') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("sdl_map_simple", sdl_map)

  # Make a minimal map, change the label
  sdl_map_l <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'EF') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  outcome_lab = 'New label',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("sdl_map_colorlabel", sdl_map)


  # put the basin in the background
  sdl_basin_background <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = list(underlay = basin, underlay_pal = 'azure'))

  vdiffr::expect_doppelganger("sdl_basin_background", sdl_basin_background)

  # If I try to put a palette on the background with a palette in the foreground, it should warn and swap to NA
  # Using cewo valleys because then it makes sense to have a fill sometimes.
  expect_warning(sdl_valley_background_warn <- obj_sdl_to_plot |>
                   dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
                   plot_outcomes(outcome_col = 'ewr_achieved',
                                 plot_type = 'map',
                                 colorgroups = NULL,
                                 colorset = 'ewr_achieved',
                                 pal_list = list('scico::berlin'),
                                 facet_col = 'env_obj',
                                 facet_row = 'scenario',
                                 sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                                 underlay_list = list(underlay = cewo_valleys,
                                                      underlay_ycol = 'ValleyName',
                                                      underlay_pal = 'scico::hawaii')))

  vdiffr::expect_doppelganger("sdl_valley_background_warn", sdl_valley_background_warn)

  # gauges as main focus
  gauges_map <- agg_theme_space$env_obj|> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = list(underlay = 'basin',
                                       underlay_pal = 'azure')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map", gauges_map)

  # gauges with filled underlay values
  gauges_map_sdl <- agg_theme_space$env_obj|> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  outcome_lab = 'New label',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = list(underlay = sdl_units,
                                       underlay_ycol = 'SWSDLName',
                                       underlay_pal = 'scico::oslo',
                                       outcome_lab = 'SDL unit')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map_sdl", gauges_map_sdl)

  # Test with a continuous variable too
  # Have to be careful with these though to not have the underlay overwhelm the main data- see the filtering.
  # Should be able to do that automatically
  gauges_map_sdl_agg <- agg_theme_space$env_obj|> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = list(underlay = dplyr::filter(obj_sdl_to_plot, env_obj == 'NF1'),
                                       underlay_ycol = 'ewr_achieved',
                                       underlay_pal = 'scico::oslo')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map_sdl_agg", gauges_map_sdl_agg)

  # that one would be good with two levels- basin and sdl
  gauges_map_2_level <- agg_theme_space$env_obj|> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = list(list(underlay = 'basin',
                                            underlay_pal = 'cornsilk'),
                                       list(underlay = dplyr::filter(obj_sdl_to_plot, env_obj == 'NF1'),
                                            underlay_ycol = 'ewr_achieved',
                                            underlay_pal = 'scico::oslo'))) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("gauges_map_2_level", gauges_map_2_level)

  # include gauges- looks better without facetting
  sdl_gauges_all <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = 'basin',
                  overlay_list = list(overlay = 'bom_basin_gauges', overlay_pal = 'purple')) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_all", sdl_gauges_all)

  # clipped to the main data
  sdl_gauges_clip <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = 'basin',
                  overlay_list = list(overlay = 'bom_basin_gauges',
                                      overlay_pal = 'purple',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_clip", sdl_gauges_clip)

  # with a meaningful palette- simple qualitative to start
  sdl_gauges_qual <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
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
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = 'basin',
                  overlay_list = list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                      overlay_pal = 'scico::oslo',
                                      overlay_ycol = 'ewr_achieved',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_quant", sdl_gauges_quant)


  # Does it work for the basin?
  basin_map <- agg_theme_space$mdb |>
    dplyr::filter(Objective %in% c("Maintain water-dependent species richness",
                                   "Increase opportunities for colonial waterbird breeding*",
                                   "Support instream & floodplain productivity")) |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'Objective',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  overlay_list = 'bom_basin_gauges')+
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("basin_map", basin_map)

  # How about multi-overlays
  basin_map_multi <- agg_theme_space$mdb |>

    dplyr::filter(Objective %in% c("Maintain water-dependent species richness",
                                   "Increase opportunities for colonial waterbird breeding*",
                                   "Support instream & floodplain productivity")) |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'Objective',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  overlay_list = list(list(overlay = 'sdl_units', overlay_pal = NA),
                                      list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                           overlay_pal = 'scico::oslo',
                                           overlay_ycol = 'ewr_achieved')))+
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("basin_map_multi", basin_map_multi)


  # How under- and overlays. nearly identical to above, but make sure palettes work with under
  sdl_gauges_quant_basinpal <- obj_sdl_to_plot |>
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = list(underlay = 'basin', underlay_pal = "azure"),
                  overlay_list = list(overlay = dplyr::filter(env_obj_points_to_plot, env_obj == 'NF1'),
                                      overlay_pal = 'scico::oslo',
                                      overlay_ycol = 'ewr_achieved',
                                      clip = TRUE)) +
    ggplot2::theme(legend.position = 'bottom')

  vdiffr::expect_doppelganger("sdl_gauges_quant_basinpal", sdl_gauges_quant_basinpal)

  # TRYING NEW SYNTAX
  gauges_map_2_level <- agg_theme_space$env_obj|> # for readability
    dplyr::filter(env_obj == 'NF1') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'scenario',
                  facet_row = 'env_obj',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  underlay_list = list(list(underlay = 'basin',
                                            underlay_pal = 'cornsilk'),
                                       list(underlay = dplyr::filter(obj_sdl_to_plot, env_obj == 'NF1'),
                                            underlay_ycol = 'ewr_achieved',
                                            underlay_pal = 'scico::oslo'))) +
    ggplot2::theme(legend.position = 'bottom')

  # Difference from baseline
  # put the basin in the background

  sdl_basin_background_difference <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('ggthemes::Orange-Blue-White Diverging'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  base_list = list(base_lev = 'base_base', comp_fun = 'difference',
                                   group_cols = c('env_obj', 'polyID')), # Do I need to group_by polyID for the maps? Yes. should probably automate that.
                  underlay_list = list(underlay = basin, underlay_pal = 'azure'))

  vdiffr::expect_doppelganger("sdl_basin_background_difference", sdl_basin_background_difference)

  # Relative to baseline
  sdl_basin_background_rel <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'WB') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('ggthemes::Orange-Blue-White Diverging'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  base_list = list(base_lev = 'base_base', comp_fun = 'relative',
                                   group_cols = c('env_obj', 'polyID')),
                  zero_adjust = 'auto',
                  transoutcome = 'log10', # Do I need to group_by polyID for the maps? Yes. should probably automate that.
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

test_that("setLimits works", {

  basin_plot20 <- plot_outcomes(basin_to_plot,
                                outcome_col = 'ewr_achieved',
                                colorset = 'Objective',
                                pal_list = list("scico::oslo"),
                                sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                                setLimits = c(0,20)) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("stacked bar 20", basin_plot20)

  basin_plot10 <- plot_outcomes(basin_to_plot,
                                outcome_col = 'ewr_achieved',
                                colorset = 'Objective',
                                pal_list = list("scico::oslo"),
                                sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                                setLimits = c(0,10)) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("stacked bar 10", basin_plot10)

  # Line plots
  # need to facet by space sdl unit and create a group col to take multiple palettes
  sdl_line_75 <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  x_col = 'delta',
                  colorgroups = NULL,
                  colorset = 'env_obj',
                  pal_list = list('scico::berlin'),
                  point_group = 'env_obj',
                  facet_row = 'SWSDLName',
                  facet_col = '.',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  setLimits = c(0, 0.75))

  vdiffr::expect_doppelganger("line_75", sdl_line_75)

  # Maps
  # Make a minimal map
  sdl_mapL <- obj_sdl_to_plot |>
    dplyr::filter(env_group == 'EF') |> # Need to reduce dimensionality
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'map',
                  colorgroups = NULL,
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::berlin'),
                  facet_col = 'env_obj',
                  facet_row = 'scenario',
                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                  setLimits = c(0, 2))

  vdiffr::expect_doppelganger("sdl_map_limits", sdl_mapL)

})

test_that("ewr works as in `plot_outcomes_bar`", {
  ewr_to_bar_data <- ewr_to_agg |>
    # just grab the first code_timing
    dplyr::group_by(ewr_code, gauge, scenario) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::filter(ewr_code %in% c('BF1', 'LF1', "OB5") &
                    gauge %in% c("412002", "412005", "412038")) |>
    dplyr::mutate(ewr_achieved = as.numeric(ewr_achieved)) # logicals fail


  ewr_plot <- plot_outcomes(ewr_to_bar_data,
                            outcome_col = 'ewr_achieved',
                            x_col = 'scenario',
                            facet_row = 'gauge',
                            facet_col = 'ewr_code',
                            colorset = 'scenario',
                            pal_list = scene_pal,
                            sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("bar_ewr", ewr_plot)
})

test_that("basin works as in `plot_outcomes_bar` (facet_wrap, no gauge, better aggregated)", {

  basin_plot <- plot_outcomes(basin_to_plot,
                              outcome_col = 'ewr_achieved',
                              x_col = 'scenario',
                              colorset = 'scenario',
                              pal_list = scene_pal,
                              facet_wrapper = 'Objective',
                              sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("bar_basin", basin_plot)

  basin_plot_L <- plot_outcomes(basin_to_plot,
                                outcome_col = 'ewr_achieved',
                                outcome_lab = "Aggregated outcome",
                                facet_wrapper = 'Objective',
                                colorset = 'scenario',
                                pal_list = scene_pal,
                                sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  vdiffr::expect_doppelganger("bar_basin_lab", basin_plot_L)
})


test_that("facet addition works", {

  sdl_colors_row <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  outcome_lab = 'Proportion\nEWR achieved',
                  x_col = 'SWSDLName',
                  facet_row = 'env_group + SWSDLName',
                  colorset = 'scenario',
                  pal_list = scene_pal,
                  position = 'dodge',
                  setLimits = c(0,1))

  sdl_colors_col <- obj_sdl_to_plot |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  outcome_lab = 'Proportion\nEWR achieved',
                  x_col = 'SWSDLName',
                  facet_col = 'env_group + SWSDLName',
                  colorset = 'scenario',
                  pal_list = scene_pal,
                  position = 'dodge',
                  setLimits = c(0,1))

  vdiffr::expect_doppelganger(" sdl_colors_row",  sdl_colors_row)
  vdiffr::expect_doppelganger(" sdl_colors_col",  sdl_colors_col)

})

test_that("hydrographs", {
  hydro_to_plot <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))

  # This one sums the flwos
 hydplot_bar <- plot_outcomes(hydro_to_plot,
                              outcome_col = 'flow',
                              colorset = 'scenario',
                              pal_list = scene_pal,
                              facet_wrapper = 'gauge',
                              sceneorder = c('down4_down4', 'base_base', 'up4_up4')) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("hydroplot_bar", hydplot_bar)

  # basic plot
  hydplot <- plot_outcomes(hydro_to_plot,
                               outcome_col = 'flow',
                           x_col = 'Date',
                               colorset = 'scenario',
                               pal_list = scene_pal,
                           facet_wrapper = 'gauge',
                               sceneorder = c('down4_down4', 'base_base', 'up4_up4')) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("hydroplot", hydplot)

  # basic plot, ncdf, swap color and facetting
  hydcdf <- read_hydro(hydropath = system.file("extdata/ncdfexample/nchydros", package = 'werptoolkitr'), format = 'nc', gaugemap = 'iqqm')

  hydplot_nc <- plot_outcomes(hydcdf,
                           outcome_col = 'flow',
                           x_col = 'Date',
                           colorset = 'gauge',
                           pal_list = list('scico::lisbon'),
                           facet_wrapper = 'scenario')

  vdiffr::expect_doppelganger("hydroplot", hydplot)

  # trans and freey
  hydplot_st <- plot_outcomes(hydro_to_plot |> dplyr::mutate(flow = flow + 1),
                           outcome_col = 'flow',
                           x_col = 'Date',
                           colorset = 'scenario',
                           pal_list = scene_pal,
                           facet_wrapper = 'gauge',
                           scales = 'free_y',
                           transoutcome = 'log10',
                           sceneorder = c('down4_down4', 'base_base', 'up4_up4')) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("hydroplot_st", hydplot_st)

  # baselined diff
  hydplot_baseD <- plot_outcomes(hydro_to_plot,
                           outcome_col = 'flow',
                           x_col = 'Date',
                           colorset = 'scenario',
                           pal_list = scene_pal,
                           facet_wrapper = 'gauge',
                           sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                           base_list = list(base_lev = 'base_base',
                                            comp_fun = 'difference',
                                            group_cols = c('Date', 'gauge'))) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("hydplot_baseD", hydplot_baseD)

  # baseline rel
  hydplot_baseR <- plot_outcomes(hydro_to_plot,
                                  outcome_col = 'flow',
                                  x_col = 'Date',
                                  colorset = 'scenario',
                                  pal_list = scene_pal,
                                  facet_wrapper = 'gauge',
                                  sceneorder = c('down4_down4', 'base_base', 'up4_up4'),
                                 zero_adjust = 'auto',
                                 transoutcome = 'log10',
                                  base_list = list(base_lev = 'base_base',
                                                   comp_fun = 'relative',
                                                   group_cols = c('Date', 'gauge'))) +
    ggplot2::theme(legend.position = 'none')

  vdiffr::expect_doppelganger("hydplot_baseR", hydplot_baseR)
})

# work in progress --------------------------------------------------------

test_that("scenarios aren't special", {

  # This is a strange example, because scenario can easily be special here. But
  # roll with it to solve the more general issue.

  sdl_colors <- obj_sdl_to_plot |>
    dplyr::summarise(ewr_achieved = mean(ewr_achieved, na.rm = TRUE),
                     .by = c(SWSDLName, scenario, geometry)) |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  outcome_lab = 'Proportion\nEWR achieved',
                  x_col = 'SWSDLName',
                  facet_row = 'scenario',
                  colorset = 'SWSDLName',
                  pal_list = SDL_pal,
                  position = 'dodge',
                  setLimits = c(0,1))
})



test_that("heatmaps", {

  # create a quant description of scenarios
  # scenarios <- tibble::tibble(scenario = c('base_base', 'down4_down4', 'up4_up4'), delta = c(1, 0.25, 4))

  # Mock data to have two dimensions. This is silly, but doesn't matter.
  ostp05 <- obj_sdl_to_plot |>
    dplyr::mutate(scenario = stringr::str_c(scenario, 'minus1'),
                  ewr_achieved = ewr_achieved - 1,
                  adelta = -1)
  ostp2 <- obj_sdl_to_plot |>
    dplyr::mutate(scenario = stringr::str_c(scenario, 'plus1'),
                  ewr_achieved = ewr_achieved + 1,
                  adelta = 1)

  ostp <- obj_sdl_to_plot |>
    dplyr::mutate(adelta = 0,
                  scenario = stringr::str_c(scenario, 'zero')) |>
    dplyr::bind_rows(ostp05, ostp2)




  # need to facet by space sdl unit and create a group col to take multiple palettes
  # first, make sure things are unique- later, will need to check they are as in maps. At least for heatmaps. Contours should? be OK?
  sdl_heat <- ostp |>
    sf::st_drop_geometry() |>
    dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'heatmap',
                  x_col = 'delta',
                  y_col = 'adelta',
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::turku'),
                  facet_row = 'SWSDLName',
                  facet_col = 'env_group')

  vdiffr::expect_doppelganger("sdl_heat", sdl_heat)

  sdl_heat_tx <- ostp |>
    sf::st_drop_geometry() |>
    dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'heatmap',
                  x_col = 'delta',
                  y_col = 'adelta',
                  transx = 'log10',
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::turku'),
                  facet_row = 'SWSDLName',
                  facet_col = 'env_group')

  vdiffr::expect_doppelganger("sdl_heat_tx", sdl_heat_tx)

  # same, contour defaults
  sdl_contour <- ostp |>
    sf::st_drop_geometry() |>
    dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'heatmap',
                  x_col = 'delta',
                  y_col = 'adelta',
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::turku'),
                  facet_row = 'SWSDLName',
                  facet_col = 'env_group',
                  contour_arglist = list())

  vdiffr::expect_doppelganger("sdl_contour", sdl_contour)

  # baseline, specify breaks
  sdl_contour_base_breaks <- ostp |>
    sf::st_drop_geometry() |>
    dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'heatmap',
                  x_col = 'delta',
                  y_col = 'adelta',
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::turku'),
                  facet_row = 'SWSDLName',
                  facet_col = 'env_group',
                  contour_arglist = list(breaks = c(-3000, -2000, -1000, 0, 1000, 2000)),
                  zero_adjust = 'auto',
                  base_list = list(base_lev = 'base_basezero', comp_fun = 'relative',
                                   group_cols = c('env_group', 'SWSDLName')))

  vdiffr::expect_doppelganger("sdl_contour_base_breaks", sdl_contour_base_breaks)

  # specify bins, use transx, transoutcome. Using a difference from lowest to avoid negative numbers so the trnas works
  # and check the auto-drop of geometry
  sdl_contour_base_bin <- ostp |>
    # sf::st_drop_geometry() |>
    dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
    plot_outcomes(outcome_col = 'ewr_achieved',
                  plot_type = 'heatmap',
                  x_col = 'delta',
                  y_col = 'adelta',
                  colorset = 'ewr_achieved',
                  pal_list = list('scico::turku'),
                  facet_row = 'SWSDLName',
                  facet_col = 'env_group',
                  contour_arglist = list(bins = 5),
                  zero_adjust = 'auto',
                  transx = 'log10',
                  transoutcome = 'sqrt',
                  base_list = list(base_lev = 'down4_down4minus1', comp_fun = 'difference',
                                   group_cols = c('env_group', 'SWSDLName')))

    vdiffr::expect_doppelganger("sdl_contour_base_bin", sdl_contour_base_bin)


  # catch an overplot by not facetting in one dimension
    expect_error(sdl_heat_overplot <- ostp |>
      sf::st_drop_geometry() |>
      # don't do the grouping correctly
      dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
      plot_outcomes(outcome_col = 'ewr_achieved',
                    plot_type = 'heatmap',
                    x_col = 'delta',
                    y_col = 'adelta',
                    colorset = 'ewr_achieved',
                    pal_list = list('scico::turku'),
                    facet_row = 'SWSDLName',
                    facet_col = '.'))


    # check sceneorder is transferring where needed. This is contrived and looks terrible, but that's not the point.
    sdl_heat_so <- ostp |>
      sf::st_drop_geometry() |>
      dplyr::filter(SWSDLName == 'Lachlan') |>
      dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, delta, adelta)) |>
      plot_outcomes(outcome_col = 'ewr_achieved',
                    plot_type = 'heatmap',
                    x_col = 'delta',
                    y_col = 'adelta',
                    transx = 'log10',
                    colorset = 'ewr_achieved',
                    pal_list = list('scico::turku'),
                    sceneorder = c('down4_down4minus1', 'down4_down4zero', 'down4_down4plus1',
                                   'base_baseminus1', 'base_basezero', 'base_baseplus1',
                                   'up4_up4minus1', 'up4_up4zero', 'up4_up4plus1'),
                    facet_row = 'scenario',
                    facet_col = 'env_group')

    vdiffr::expect_doppelganger("sdl_heat_so", sdl_heat_so)

    # interpolated raster
    sdl_heat_interp <- ostp |>
      sf::st_drop_geometry() |>
      dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
      plot_outcomes(outcome_col = 'ewr_achieved',
                    plot_type = 'heatmap',
                    x_col = 'delta',
                    y_col = 'adelta',
                    colorset = 'ewr_achieved',
                    pal_list = list('scico::turku'),
                    transx = 'log10',
                    facet_row = 'SWSDLName',
                    facet_col = 'env_group',
                    contour_arglist = list(interpolate = TRUE))

    vdiffr::expect_doppelganger("sdl_heat_interp", sdl_heat_interp)

    # Qualitative x-y (e.g. named scenario types)
    sdl_heat_char <- ostp |>
      sf::st_drop_geometry() |>
      dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
      dplyr::mutate(delta = as.character(delta),
                    adelta = as.character(adelta)) |>
      plot_outcomes(outcome_col = 'ewr_achieved',
                    plot_type = 'heatmap',
                    x_col = 'delta',
                    y_col = 'adelta',
                    colorset = 'ewr_achieved',
                    pal_list = list('scico::turku'),
                    facet_row = 'SWSDLName',
                    facet_col = 'env_group')

    vdiffr::expect_doppelganger("sdl_heat_char", sdl_heat_char)

    sdl_heat_fact <- ostp |>
      sf::st_drop_geometry() |>
      dplyr::summarise(ewr_achieved = mean(ewr_achieved), .by = c(env_group, scenario, SWSDLName, delta, adelta)) |>
      dplyr::mutate(delta = as.factor(delta),
                    adelta = as.factor(adelta)) |>
      plot_outcomes(outcome_col = 'ewr_achieved',
                    plot_type = 'heatmap',
                    x_col = 'delta',
                    y_col = 'adelta',
                    colorset = 'ewr_achieved',
                    pal_list = list('scico::turku'),
                    facet_row = 'SWSDLName',
                    facet_col = 'env_group')

    vdiffr::expect_doppelganger("sdl_heat_fact", sdl_heat_fact)
  ## Still to test/develop
  # grouped colors? Unclear how that'd work... Different ramps for the different groups? Needed? Would look cool, but I think like maps, do that later

})
