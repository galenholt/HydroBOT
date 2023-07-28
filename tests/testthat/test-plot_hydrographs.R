# hard to test really well that it *looks* right. I guess let's just check it makes a ggplot
# the answer is {vdiffr} tidyverse.org/blog/2021/06/vdiffr-1-0-0/
test_that("expected use makes plot", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3')
  hydplot <- plot_hydrographs(hydlong, colors = hydpal)
  expect_s3_class(hydplot, 'ggplot')
  vdiffr::expect_doppelganger("default hydroplot", hydplot)

})

test_that("sceneorder works", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3')
  hydplot <- plot_hydrographs(hydlong, colors = hydpal, sceneorder = c('down4', 'base', 'up4'))
  expect_s3_class(hydplot, 'ggplot')
  vdiffr::expect_doppelganger("sceneorder hydroplot", hydplot)

})

test_that("palette name makes plot", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydplot <- plot_hydrographs(hydlong, colors = 'calecopal::superbloom3')
  expect_s3_class(hydplot, 'ggplot')
  vdiffr::expect_doppelganger("palname hydro", hydplot)

})

test_that("gauge and scenariofilter work", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3')
  gauges_to_plot <- c('412002', '421001')
  scenes_to_plot <- c('down4', 'up4')
  hydplot <- plot_hydrographs(hydlong, colors = hydpal, gaugefilter = gauges_to_plot, scenariofilter = scenes_to_plot)
  expect_s3_class(hydplot, 'ggplot')
  vdiffr::expect_doppelganger("filtered hydro", hydplot)

})

test_that("scales and transy makes plot", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3')
  # Add an eps to avoid log(0) warnings, since that's not the point here
  hydplot <- hydlong |>
    dplyr::mutate(flow = flow + 1) |>
    plot_hydrographs(colors = hydpal, scales = 'free_y', transy = 'log10')
  expect_s3_class(hydplot, 'ggplot')
  vdiffr::expect_doppelganger("scaletrans hydro", hydplot)

})

test_that("auto-baselining works", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3', refvals = 'base', refcols = 'black')
  hydplot <- plot_hydrographs(hydlong, colors = hydpal, base_lev = 'base',
                              comp_fun = difference,
                              group_cols = c('Date', 'gauge'))
  hydplot_rel <- plot_hydrographs(hydlong, colors = hydpal,
                                  base_lev = 'base', comp_fun = relative,
                                  group_cols = c('Date', 'gauge'),
                                  transy = 'log10', add_eps = 1e-6)
  expect_s3_class(hydplot, 'ggplot')
  vdiffr::expect_doppelganger("difference baseline hydro", hydplot)
  expect_s3_class(hydplot_rel, 'ggplot')
  vdiffr::expect_doppelganger("relative baseline hydro", hydplot_rel)

})
