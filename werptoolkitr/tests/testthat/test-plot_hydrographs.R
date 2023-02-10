# hard to test really well that it *looks* right. I guess let's just check it makes a ggplot
# the answer is {vdiffr} tidyverse.org/blog/2021/06/vdiffr-1-0-0/
test_that("expected use makes plot", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3')
  hydplot <- plot_hydrographs(hydlong, colors = hydpal)
  expect_equal(class(hydplot), c('gg', 'ggplot'))
  vdiffr::expect_doppelganger("default hydroplot", hydplot)

})

test_that("palette name makes plot", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydplot <- plot_hydrographs(hydlong, colors = 'calecopal::superbloom3')
  expect_equal(class(hydplot), c('gg', 'ggplot'))
  vdiffr::expect_doppelganger("palname hydro", hydplot)

})

test_that("gauge and scenariofilter work", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3')
  gauges_to_plot <- c('412002', '421001')
  scenes_to_plot <- c('down4', 'up4')
  hydplot <- plot_hydrographs(hydlong, colors = hydpal, gaugefilter = gauges_to_plot, scenariofilter = scenes_to_plot)
  expect_equal(class(hydplot), c('gg', 'ggplot'))
  vdiffr::expect_doppelganger("filtered hydro", hydplot)

})

test_that("scales and transy makes plot", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  hydpal <- make_pal(levels = unique(hydlong$scenario), palette = 'calecopal::superbloom3')
  hydplot <- plot_hydrographs(hydlong, colors = hydpal, scales = 'free_y', transy = 'log10')
  expect_equal(class(hydplot), c('gg', 'ggplot'))
  vdiffr::expect_doppelganger("scaletrans hydro", hydplot)

})
