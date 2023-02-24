test_that("ewr works", {
  ewr_to_bar_data <- summary_ewr_output %>%
    # just grab the first code_timing
    dplyr::group_by(ewr_code, gauge, scenario) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ewr_code %in% c('BF1', 'LF1', "OB5") &
                    gauge %in% c("412002", "412005", "412038"))

  ewr_plot <- plot_outcomes_1d(ewr_to_bar_data,
                               y_col = 'ewr_achieved',
                               facet_row = 'gauge',
                               facet_col = 'ewr_code',
                               colors = scene_pal,
                               sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_ewr", ewr_plot)
})

test_that("basin works (facet_wrap, no gauge, better aggregated)", {
  basin_to_plot <- agg_theme_space$mdb %>%
    dplyr::rename(allArith = 4, oneLimiting = 5) %>% # for readability
    dplyr::filter(!is.na(Objective))

  scene_pal <- make_pal(levels = unique(basin_to_plot$scenario), palette = 'calecopal::superbloom3')
  basin_plot <- plot_outcomes_1d(basin_to_plot,
                                 y_col = 'allArith',
                                 facet_wrapper = 'Objective',
                                 colors = scene_pal,
                                 sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin", basin_plot)

  basin_plot_L <- plot_outcomes_1d(basin_to_plot,
                                 y_col = 'allArith',
                                 y_lab = "Aggregated outcome",
                                 facet_wrapper = 'Objective',
                                 colors = scene_pal,
                                 sceneorder = c('down4', 'base', 'up4'))

  vdiffr::expect_doppelganger("bar_basin_lab", basin_plot_L)
})
