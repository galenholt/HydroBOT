# build the aggregated data so we don't have to include it in the package

agg_theme_space <- make_test_agg(namehistory = FALSE)


test_that("character sceneorder works", {


  test_prep <- plot_data_prep(agg_theme_space$sdl_units, outcome_col = 'ewr_achieved',
                         sceneorder = c('down4_down4', 'base_base', 'up4_up4'))

  expect_equal(levels(test_prep$data$scenario), c('down4_down4', 'base_base', 'up4_up4', 'MAX'))
})

test_that("factor sceneorder works", {
  scenarios <- tibble::tibble(scenario = c('down4_down4', 'base_base', 'up4_up4', 'MAX'), delta = c(1, 0.25, 4, Inf))
  sceneorder_f <- forcats::fct_reorder(scenarios$scenario, scenarios$delta)

  test_prep <- plot_data_prep(agg_theme_space$sdl_units, outcome_col = 'ewr_achieved',
                         sceneorder = sceneorder_f)

  expect_equal(levels(test_prep$data$scenario), levels(sceneorder_f))

})

