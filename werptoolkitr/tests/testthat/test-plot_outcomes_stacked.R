test_that("basin works with single color palette", {
  basin_to_plot <- agg_theme_space$mdb %>%
    dplyr::rename(allArith = 4, oneLimiting = 5) %>% # for readability
    dplyr::filter(!is.na(Objective))

  basin_plot <- plot_outcomes_stacked(basin_to_plot,
                                      y_col = 'allArith',
                                      colorset = 'Objective',
                                      pal_list = list("scico::oslo"),
                                      sceneorder = c('down4', 'base', 'up4'))

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
                          facet_row = 'SWSDLName',
                          facet_col = 'colcol',
                          sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_groupfacet_sdl", sdl_plot_factgroup)

})

test_that("flipped", {
  # What I want to do is just swap the x and fill arguments and pass in the
  # scenario palette. Can I do that easily?


  # obj_sdl_to_plot <- agg_theme_space$sdl_units %>%
  #   dplyr::rename(allArith = 4) # for readability
  #
  # # Create a grouping variable
  # obj_sdl_to_plot <- obj_sdl_to_plot |>
  #   dplyr::mutate(colcol = stringr::str_extract(env_obj, '^[A-Z]+')) |>
  #   dplyr::filter(!is.na(colcol)) |>
  #   dplyr::arrange(colcol, env_obj)
  #
  # scene_pal <- make_pal(levels = unique(basin_to_plot$scenario), palette = 'calecopal::superbloom3')
  #
  # # need to facet by space sdl unit and create a group col to take multiple palettes
  # sdl_plot <- obj_sdl_to_plot |>
  #   plot_outcomes_stacked(y_col = 'allArith',
  #                         colorgroups = 'colcol',
  #                         colorset = 'env_obj',
  #                         pal_list = scene_pal,
  #                         facet_wrapper = 'SWSDLName',
  #                         sceneorder = c('down4', 'base', 'up4'))
  # sdl_plot
  #

})
