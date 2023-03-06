test_that("character sceneorder works", {
  obj_sdl <- agg_theme_space$sdl_units %>%
    dplyr::rename(allArith = 4)

  test_prep <- plot_prep(obj_sdl, y_col = 'allArith',
                         sceneorder = c('down4', 'base', 'up4'))

  expect_equal(levels(test_prep$data$scenario), c('down4', 'base', 'up4'))
})

test_that("factor sceneorder works", {
  scenarios <- tibble::tibble(scenario = c('base', 'down4', 'up4'), delta = c(1, 0.25, 4))
  sceneorder_f <- forcats::fct_reorder(scenarios$scenario, scenarios$delta)

  test_prep <- plot_prep(obj_sdl, y_col = 'allArith',
                         sceneorder = sceneorder_f)

  expect_equal(levels(test_prep$data$scenario), c('down4', 'base', 'up4'))

})

